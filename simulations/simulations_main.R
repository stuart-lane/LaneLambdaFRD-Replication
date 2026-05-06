### ============================================================================
### SETUP
### ============================================================================

packages <- c("parallel", "doParallel", "foreach", "triangle", "expm",
              "dplyr", "rdrobust", "RDHonest")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

### ============================================================================
### DATA FRAME CONFIGURATION
### ============================================================================

total_combinations <- length(RUNNING_VARIABLES) * length(SETUPS) * length(P1_VALUES) * length(ERROR_DISTRIBUTIONS) * length(DGPS)

results_df <- data.frame(
  
  ## Parameter configurations
  dgp = integer(total_combinations),
  error_distribution = numeric(total_combinations),
  running_variable = integer(total_combinations),
  setup = integer(total_combinations),
  p1 = numeric(total_combinations),
  n = numeric(total_combinations),
  true_treatment_effect = numeric(total_combinations),
  
  ## Median bias
  median_bias_iv_ccf = numeric(total_combinations),
  median_bias_iv_ik = numeric(total_combinations),
  median_bias_lambda_1_ccf = numeric(total_combinations),
  median_bias_lambda_1_ik = numeric(total_combinations),
  median_bias_lambda_4_ccf = numeric(total_combinations),
  median_bias_lambda_4_ik = numeric(total_combinations),
  
  ## Median absolute deviation
  mad_iv_ccf = numeric(total_combinations),
  mad_iv_ik = numeric(total_combinations),
  mad_lambda_1_ccf = numeric(total_combinations),
  mad_lambda_1_ik = numeric(total_combinations),
  mad_lambda_4_ccf = numeric(total_combinations),
  mad_lambda_4_ik = numeric(total_combinations),
  
  ## Root mean squared error
  rmse_iv_ccf = numeric(total_combinations),
  rmse_iv_ik = numeric(total_combinations),
  rmse_lambda_1_ccf = numeric(total_combinations),
  rmse_lambda_1_ik = numeric(total_combinations),
  rmse_lambda_4_ccf = numeric(total_combinations),
  rmse_lambda_4_ik = numeric(total_combinations),
  
  ## Confidence interval coverage
  cov_iv_ccf = numeric(total_combinations),
  cov_iv_ik = numeric(total_combinations),
  cov_lambda_1_ccf = numeric(total_combinations),
  cov_lambda_1_ik = numeric(total_combinations),
  cov_lambda_4_ccf = numeric(total_combinations),
  cov_lambda_4_ik = numeric(total_combinations),
  cov_ar_ba_1 = numeric(total_combinations),
  cov_ar_ba_2 = numeric(total_combinations)
)

### ============================================================================
### PARALLELISED SIMULATIONS FUNCTION
### ============================================================================

result_index <- 1

parallelise_simulation <- function(
    dgps = DGPS,
    running_variables = RUNNING_VARIABLES,
    setups = SETUPS,
    p1_values = P1_VALUES,
    error_distributions = ERROR_DISTRIBUTIONS,
    ns = NS
) {
  dir.create(SIMULATION_CSV_OUTPUT_FOLDER_PATH,
             showWarnings = FALSE, recursive = TRUE)
  
  ## Setup parallel processing -------------------------------------------------
  cat("\n\nRunning: ./simulations/simulations.R", "\n\n")
  cat("\n----- New Simulation Started -----\n")
  cat("Date and Time:", as.character(round(Sys.time(), units = "secs")), "\n")
  cat("================================== \n")
  cat("Using", NUM_CORES, "/", TOTAL_CORES, "cores\n")
  
  ## Loop over DGP and running variable combinations ---------------------------
  all_errors_list <- list() 
  error_counter <- 1
  
  ## Loop over DGP and running variable combinations ---------------------------
  for (error_distribution in error_distributions) {
    for (dgp in dgps) {
      for (running_variable in running_variables) {
      
        cat("\n")
        cat("=======================================================================\n")
        cat("Processing DGP =", dgp, ", Running Variable =", running_variable, ", Error Distribution =", error_distribution, "\n")
        cat("=======================================================================\n")
        
        start_time_combo <- Sys.time()
        
        ## Create parameter grid -------------------------------------------------
        combinations <- expand.grid(
          dgp = dgp,
          running_variable = running_variable,
          p1 = P1_VALUES,
          setup = SETUPS,
          error_distribution = error_distribution,
          n = NS
        )
        
        cat("Parameter combinations for this file:", nrow(combinations), "\n")
        cat("-------------------------------------------\n\n")
        
        ## Create cluster and export ---------------------------------------------
        cl <- makeCluster(NUM_CORES)
        registerDoParallel(cl)
        
        clusterExport(cl, c(
          "LambdaFRD", "generate_rdd_data", "calculate_ROT1", "calculate_ROT2",
          "COEFFS_CONT_LEE", "COEFFS_TREAT_LEE", "COEFFS_CONT_LM", "COEFFS_TREAT_LM",
          "X0", "NSIM", "SIMULATION_CSV_OUTPUT_FOLDER_PATH", "scale_to_mad", "target_mad"
        ))
        
        clusterEvalQ(cl, {
          library(rdrobust)
          library(RDHonest)
          library(triangle)
          library(dplyr)
        })
        
        ## Run parallel simulation for this combination --------------------------
        results_list <- foreach(
          idx = 1:nrow(combinations),
          .packages = c("rdrobust", "RDHonest", "triangle", "dplyr"),
          .combine = 'list',
          .multicombine = TRUE,
          .errorhandling = "pass"
        ) %dopar% {
          
          # Extract parameters for this iteration
          p1 <- combinations$p1[idx]
          setup <- combinations$setup[idx]
          running_variable <- combinations$running_variable[idx]
          dgp <- combinations$dgp[idx]
          error_distribution <- combinations$error_distribution[idx]
          n <- combinations$n[idx]
          
          true_treatment_effect <- ifelse(dgp == 1, 0.04,-3.44)
          
          running_variable_label <- switch(
            as.character(running_variable),
            "1" = "Normal",
            "2" = "Uniform",
            "3" = "Beta"
          )
          
          # Initialise vectors to store results
          results_iv_ccf <- numeric(NSIM)
          results_iv_ik <- numeric(NSIM)
          results_lambda_1_ik <- numeric(NSIM)
          results_lambda_1_ccf <- numeric(NSIM)
          results_lambda_4_ik <- numeric(NSIM)
          results_lambda_4_ccf <- numeric(NSIM)
          coverage_iv_ccf <- numeric(NSIM)
          coverage_iv_ik <- numeric(NSIM)
          coverage_lambda_1_ccf <- numeric(NSIM)
          coverage_lambda_1_ik <- numeric(NSIM)
          coverage_lambda_4_ik <- numeric(NSIM)
          coverage_lambda_4_ccf <- numeric(NSIM)
          coverage_ar_ba_1 <- numeric(NSIM)
          coverage_ar_ba_2 <- numeric(NSIM)
          
          iter_seeds <- numeric(NSIM)
          
          # Track failed repetitions
          error_count <- 0
          
          ## =====================================================================
          ## MAIN SIMULATION LOOP
          ## =====================================================================
          
          for (i in 1:NSIM) {
            # This seed-setting logic means results are exactly reproducible in
            # parallel for any combination of parameter configurations, including
            # single configurations of interest
            
            param_id <- dgp * 10000 + running_variable * 1000 + setup * 100 +
              round(p1 * 10) + (n %/% 100)
            iter_seed <- 1234 + param_id * NSIM + i
            set.seed(iter_seed)
            iter_seeds[i] <- iter_seed
            
            tryCatch({
              sim_data <- generate_rdd_data(n, running_variable, setup, p1,
                                            dgp, X0, error_distribution)
              
              df <- data.frame(y = sim_data$y, X = sim_data$x, D = sim_data$D)
              
              sim_data$y <- as.matrix(sim_data$y, ncol = 1)
              sim_data$x <- as.matrix(sim_data$x, ncol = 1)
              sim_data$D <- as.matrix(sim_data$D, ncol = 1)
              
              ## =================================================================
              ## BANDWIDTH SELECTION
              ## =================================================================
              
              ## Coverage optimal bandwidth --------------------------------------
              bw_ccf <- rdbwselect(y = sim_data$y, x = sim_data$x, c = X0,
                                  fuzzy = sim_data$D, bwselect = "cerrd")
              ccf_bw <- bw_ccf$bws[1, 1]
              
              ## MSE optimal bandwidth -------------------------------------------
              bw_ik <- rdbwselect(y = sim_data$y, x = sim_data$x, c = X0,
                                  fuzzy = sim_data$D)
              ik_bw <- bw_ik$bws[1, 1]
              
              ## =================================================================
              ## LAMBDA CLASS ESTIMATOR WITH λ = Λ(1)
              ## =================================================================
              
              ## Lambda 1 estimator with MSE optimal bandwidth -------------------
              result_lambda_1_ik <- LambdaFRD(
                Y = sim_data$y, D = sim_data$D, X = sim_data$x, x0 = X0, 
                exog = NULL,  bandwidth = ik_bw, Lambda = TRUE, psi = 1,  
                lambda = NULL, tau_0 = true_treatment_effect, p = 1, 
                kernel = "uniform", robust = FALSE, alpha = 0.05
              )
              
              results_lambda_1_ik[i] <- result_lambda_1_ik$tau_lambda
              coverage_lambda_1_ik[i] <- as.numeric(1 - result_lambda_1_ik$reject_t)
              
              ## Lambda 1 estimator with coverage optimal bandwidth --------------
              result_lambda_1_ccf <- LambdaFRD(
                Y = sim_data$y, D = sim_data$D, X = sim_data$x, x0 = X0,
                exog = NULL, bandwidth = ccf_bw, Lambda = TRUE, psi = 1, 
                lambda = NULL, tau_0 = true_treatment_effect, p = 1, 
                kernel = "uniform", robust = FALSE, alpha = 0.05
              )
              
              results_lambda_1_ccf[i] <- result_lambda_1_ccf$tau_lambda
              coverage_lambda_1_ccf[i] <- as.numeric(1 - result_lambda_1_ccf$reject_t)
              
              ## =================================================================
              ## LAMBDA CLASS ESTIMATOR WITH λ = Λ(4)
              ## =================================================================
              
              ## Lambda 4 estimator with MSE optimal bandwidth -------------------
              result_lambda_4_ik <- LambdaFRD(
                Y = sim_data$y, D = sim_data$D, X = sim_data$x, x0 = X0,
                exog = NULL, bandwidth = ik_bw, Lambda = TRUE, psi = 4,
                lambda = NULL, tau_0 = true_treatment_effect, p = 1,
                kernel = "uniform", robust = FALSE, alpha = 0.05
              )
              
              results_lambda_4_ik[i] <- result_lambda_4_ik$tau_lambda
              coverage_lambda_4_ik[i] <- as.numeric(1 -result_lambda_4_ik$reject_t)
              
              ## Lambda 1 estimator with cov optimal bandwidth -------------------
              result_lambda_4_ccf <- LambdaFRD(
                Y = sim_data$y, D = sim_data$D, X = sim_data$x, x0 = X0, 
                exog = NULL, bandwidth = ccf_bw, Lambda = TRUE, psi = 4,  
                lambda = NULL, tau_0 = true_treatment_effect, p = 1, 
                kernel = "uniform", robust = FALSE, alpha = 0.05
              )
              
              results_lambda_4_ccf[i] <- result_lambda_4_ccf$tau_lambda
              coverage_lambda_4_ccf[i] <- as.numeric(1 - result_lambda_4_ccf$reject_t)
              
              ## =================================================================
              ## RD ROBUST CONFIDENCE INTERVALS
              ## =================================================================
              
              ## FRD estimator with MSE optimal bandwidth ------------------------
              rd_result_ccf <- rdrobust(y = sim_data$y, x = sim_data$x, c = X0,
                                        fuzzy = sim_data$D, h = ccf_bw)
              results_iv_ccf[i] <- rd_result_ccf$coef[1]
              
              ## Coverage for FRD estimator with MSE optimal bandwidth -----------
              ci_lower_iv_ccf <- rd_result_ccf$ci[3, 1]
              ci_upper_iv_ccf <- rd_result_ccf$ci[3, 2]
              coverage_iv_ccf[i] <- as.numeric(
                true_treatment_effect >= ci_lower_iv_ccf &
                  true_treatment_effect <= ci_upper_iv_ccf
              )
              
              ## FRD estimator with MSE bandwidth --------------------------------
              rd_result_ik <- rdrobust(y = sim_data$y, x = sim_data$x,
                                      c = X0, fuzzy = sim_data$D, h = ik_bw)
              results_iv_ik[i] <- rd_result_ik$coef[1]
              
              ## Coverage for FRD estimator wMSE bandwidth -----------------------
              ci_lower_iv_ik <- rd_result_ik$ci[3, 1]
              ci_upper_iv_ik <- rd_result_ik$ci[3, 2]
              coverage_iv_ik[i] <- as.numeric(
                true_treatment_effect >= ci_lower_iv_ik &
                  true_treatment_effect <= ci_upper_iv_ik
              )
              
              ## =================================================================
              ## BIAS AWARE CONFIDENCE INTERVALS
              ## =================================================================
              
              d <- list()
              d$Y <- sim_data$y
              d$D <- sim_data$D
              d$X <- sim_data$x
              d$ind.X <- (d$X >= X0)
              
              M_rot1 <- calculate_ROT1(d, X0)
              M_rot2 <- calculate_ROT2(d, X0)
              
              df_original <- data.frame(Y = d$Y, D = d$D, X = d$X)
              
              df_transformed <- data.frame(
                Y = d$Y - true_treatment_effect * d$D,
                X = d$X
              )
              
              ## AR1 -------------------------------------------------------------
              reg_ar_1 <- suppressMessages(try({
                RDHonest(
                  Y ~ X,
                  data = df_transformed,
                  M = M_rot1[1] + abs(true_treatment_effect) * M_rot1[2],
                  cutoff = X0,
                  kern = "triangular",
                  sclass = "H",
                  opt.criterion = "FLCI"
                )
              }, silent = TRUE))
              
              if (!inherits(reg_ar_1, "try-error")) {
                ar_1_coverage <- as.numeric((0 >= reg_ar_1$coef$conf.low) &
                                              (0 <= reg_ar_1$coef$conf.high))
                ar_1_bandwidth <- reg_ar_1$coef$bandwidth
              } else {
                ar_1_coverage <- NA
                ar_1_bandwidth <- NA
              }
              
              coverage_ar_ba_1[i] <- ar_1_coverage
              
              ## AR2 -------------------------------------------------------------
              reg_ar_2 <- suppressMessages(try({
                RDHonest(
                  Y ~ X,
                  data = df_transformed,
                  M = M_rot2[1] + abs(true_treatment_effect) * M_rot2[2],
                  cutoff = X0,
                  kern = "triangular",
                  sclass = "H",
                  opt.criterion = "FLCI"
                )
              }, silent = TRUE))
              
              if (!inherits(reg_ar_2, "try-error")) {
                ar_2_coverage <- as.numeric((0 >= reg_ar_2$coef$conf.low) &
                                              (0 <= reg_ar_2$coef$conf.high))
                ar_2_bandwidth <- reg_ar_2$coef$bandwidth
              } else {
                ar_2_coverage <- NA
                ar_2_bandwidth <- NA
              }
              
              coverage_ar_ba_2[i] <- ar_2_coverage
              
            }, error = function(e) {
              error_count <<- error_count + 1
              # Mark this iteration's results as NA
              results_iv_ccf[i] <- NA
              results_iv_ik[i] <- NA
              results_lambda_1_ik[i] <- NA
              results_lambda_1_ccf[i] <- NA
              results_lambda_4_ik[i] <- NA
              results_lambda_4_ccf[i] <- NA
              coverage_iv_ccf[i] <- NA
              coverage_iv_ik[i] <- NA
              coverage_lambda_1_ccf[i] <- NA
              coverage_lambda_1_ik[i] <- NA
              coverage_lambda_4_ik[i] <- NA
              coverage_lambda_4_ccf[i] <- NA
              coverage_ar_ba_1[i] <- NA
              coverage_ar_ba_2[i] <- NA
            })
          }
          
          ## =====================================================================
          ## COMPUTE SUMMARY STATISTICS FOR PARAMETER CONFIGURATION
          ## =====================================================================
          
          # Calculate error metrics (for separate tracking)
          error_rate <- error_count / NSIM
          successful_sims <- NSIM - error_count
          
          # Calculate Median Bias (with na.rm = TRUE)
          median_bias_iv_ik <- median(results_iv_ik - true_treatment_effect, na.rm = TRUE)
          median_bias_iv_ccf <- median(results_iv_ccf - true_treatment_effect, na.rm = TRUE)
          median_bias_lambda_1_ik <- median(results_lambda_1_ik - true_treatment_effect, na.rm = TRUE)
          median_bias_lambda_1_ccf <- median(results_lambda_1_ccf - true_treatment_effect, na.rm = TRUE)
          median_bias_lambda_4_ik <- median(results_lambda_4_ik - true_treatment_effect, na.rm = TRUE)
          median_bias_lambda_4_ccf <- median(results_lambda_4_ccf - true_treatment_effect, na.rm = TRUE)
          
          # Calculate Median Absolute Deviation (with na.rm = TRUE)
          mad_iv_ik <- median(abs(results_iv_ik - true_treatment_effect), na.rm = TRUE)
          mad_iv_ccf <- median(abs(results_iv_ccf - true_treatment_effect), na.rm = TRUE)
          mad_lambda_1_ik <- median(abs(results_lambda_1_ik - true_treatment_effect), na.rm = TRUE)
          mad_lambda_1_ccf <- median(abs(results_lambda_1_ccf - true_treatment_effect), na.rm = TRUE)
          mad_lambda_4_ik <- median(abs(results_lambda_4_ik - true_treatment_effect), na.rm = TRUE)
          mad_lambda_4_ccf <- median(abs(results_lambda_4_ccf - true_treatment_effect), na.rm = TRUE)
          
          # Calculate Mean Squared Errors (with na.rm = TRUE)
          rmse_iv_ik <- sqrt(mean((results_iv_ik - true_treatment_effect)^2, na.rm = TRUE))
          rmse_iv_ccf <- sqrt(mean((results_iv_ccf - true_treatment_effect)^2, na.rm = TRUE))
          rmse_lambda_1_ik <- sqrt(mean((results_lambda_1_ik - true_treatment_effect)^2, na.rm = TRUE))
          rmse_lambda_1_ccf <- sqrt(mean((results_lambda_1_ccf - true_treatment_effect)^2, na.rm = TRUE))
          rmse_lambda_4_ik <- sqrt(mean((results_lambda_4_ik - true_treatment_effect)^2, na.rm = TRUE))
          rmse_lambda_4_ccf <- sqrt(mean((results_lambda_4_ccf - true_treatment_effect)^2, na.rm = TRUE))
          
          # Calculate Coverage Probabilities (with na.rm = TRUE)
          cov_iv_ik <- mean(coverage_iv_ik, na.rm = TRUE)
          cov_iv_ccf <- mean(coverage_iv_ccf, na.rm = TRUE)
          cov_lambda_1_ik <- mean(coverage_lambda_1_ik, na.rm = TRUE)
          cov_lambda_1_ccf <- mean(coverage_lambda_1_ccf, na.rm = TRUE)
          cov_lambda_4_ik <- mean(coverage_lambda_4_ik, na.rm = TRUE)
          cov_lambda_4_ccf <- mean(coverage_lambda_4_ccf, na.rm = TRUE)
          cov_ar_ba_1 <- mean(coverage_ar_ba_1, na.rm = TRUE)
          cov_ar_ba_2 <- mean(coverage_ar_ba_2, na.rm = TRUE)
          
          # Return results and error tracking separately
          list(
            results = data.frame(
              # Store parameter configuration
              dgp = dgp,
              error_distribution = error_distribution,
              running_variable = running_variable,
              setup = setup,
              p1 = p1,
              n = n,
              true_treatment_effect = true_treatment_effect,
              
              # Store median bias
              median_bias_iv_ccf = median_bias_iv_ccf,
              median_bias_iv_ik = median_bias_iv_ik,
              median_bias_lambda_1_ccf = median_bias_lambda_1_ccf,
              median_bias_lambda_1_ik = median_bias_lambda_1_ik,
              median_bias_lambda_4_ccf = median_bias_lambda_4_ccf,
              median_bias_lambda_4_ik = median_bias_lambda_4_ik,
              
              # Store median absolute deviation
              mad_iv_ccf = mad_iv_ccf,
              mad_iv_ik = mad_iv_ik,
              mad_lambda_1_ccf = mad_lambda_1_ccf,
              mad_lambda_1_ik = mad_lambda_1_ik,
              mad_lambda_4_ccf = mad_lambda_4_ccf,
              mad_lambda_4_ik = mad_lambda_4_ik,
              
              # Store root mean squared error
              rmse_iv_ccf = rmse_iv_ccf,
              rmse_iv_ik = rmse_iv_ik,
              rmse_lambda_1_ccf = rmse_lambda_1_ccf,
              rmse_lambda_1_ik = rmse_lambda_1_ik,
              rmse_lambda_4_ccf = rmse_lambda_4_ccf,
              rmse_lambda_4_ik = rmse_lambda_4_ik,
              
              # Store empirical coverage
              cov_iv_ccf = cov_iv_ccf,
              cov_iv_ik = cov_iv_ik,
              cov_lambda_1_ccf = cov_lambda_1_ccf,
              cov_lambda_1_ik = cov_lambda_1_ik,
              cov_lambda_4_ccf = cov_lambda_4_ccf,
              cov_lambda_4_ik = cov_lambda_4_ik,
              cov_ar_ba_1 = cov_ar_ba_1,
              cov_ar_ba_2 = cov_ar_ba_2
            ),
            errors = data.frame(
              dgp = dgp,
              error_distribution = error_distribution,
              running_variable = running_variable,
              setup = setup,
              p1 = p1,
              n = n,
              n_errors = error_count,
              error_rate = error_rate,
              successful_sims = successful_sims
            )
          )
        }
        
        ## Stop the cluster ------------------------------------------------------
        stopCluster(cl)
        
        ## Extract results and error tracking ------------------------------------
        final_results_df <- do.call(rbind, lapply(results_list, function(x) x$results))
        error_tracking_df <- do.call(rbind, lapply(results_list, function(x) x$errors))
        
        ## Store errors for later combination ------------------------------------
        all_errors_list[[error_counter]] <- error_tracking_df
        error_counter <- error_counter + 1
        
        ## Save results for this DGP × running variable combination --------------
        # Create descriptive filename
        running_var_label <- switch(
          as.character(running_variable),
          "1" = "normal",
          "2" = "uniform",
          "3" = "beta"
        )

        error_distribution_label <- switch(
          as.character(error_distribution),
          "1" = "normal",
          "2" = "t"
        )
        
        filename <- paste0("simulation_results_dgp", dgp, 
                          "_rv", running_var_label,
                          "_error", error_distribution_label, ".csv")
        filepath <- file.path(SIMULATION_CSV_OUTPUT_FOLDER_PATH, filename)
        
        write.csv(final_results_df, filepath, row.names = FALSE)
        
        ## Print error summary ---------------------------------------------------
        cat("\n--- Error Summary ---\n")
        
        total_errors <- sum(error_tracking_df$n_errors, na.rm = TRUE)
        total_sims <- sum(error_tracking_df$successful_sims, na.rm = TRUE) + total_errors
        overall_error_rate <- if(total_sims > 0) (total_errors / total_sims) * 100 else 0
        
        cat("Total simulations attempted:", total_sims, "\n")
        cat("Successful simulations:", sum(error_tracking_df$successful_sims, na.rm = TRUE), "\n")
        cat("Total errors:", total_errors, "\n")
        cat("Overall error rate:", round(overall_error_rate, 3), "%\n")
        cat("--------------------\n\n")
        
        end_time_combo <- Sys.time()
        time_taken <- difftime(end_time_combo, start_time_combo, units = "mins")
        
        cat("\n Completed DGP =", dgp, ", Running Variable =", running_variable, "\n")
        cat("  Time taken:", round(time_taken, 2), "minutes\n")
        cat("  Results saved to:\n")
        cat("  ", filename, "\n\n")
      }
    }
  }
  
  ## Combine and save all error tracking data --------------------------------
  cat("\n")
  cat("========================================\n")
  cat("Saving combined error tracking data...\n")
  cat("========================================\n")
  
  combined_errors_df <- do.call(rbind, all_errors_list)
  
  # Simplified version with just dgp, running_variable, and total errors:
  simplified_errors_df <- combined_errors_df %>%
    group_by(dgp, running_variable) %>%
    summarise(
      total_errors = sum(n_errors),
      total_simulations = sum(successful_sims) + sum(n_errors),
      error_rate_percent = (sum(n_errors) / (sum(successful_sims) + sum(n_errors))) * 100,
      .groups = 'drop'
    )
  
  # Save both versions
  write.csv(
    combined_errors_df, 
    file.path(SIMULATION_CSV_OUTPUT_FOLDER_PATH, "error_tracking_detailed.csv"), 
    row.names = FALSE
  )
  
  write.csv(
    simplified_errors_df, 
    file.path(SIMULATION_CSV_OUTPUT_FOLDER_PATH, "error_tracking_summary.csv"), 
    row.names = FALSE
  )
  
  cat("  Error tracking saved:\n")
  cat("  error_tracking_detailed.csv (full details)\n")
  cat("  error_tracking_summary.csv (by dgp × running_variable)\n\n")
  
  cat("\n")
  cat("========================================\n")
  cat("ALL SIMULATIONS COMPLETE!\n")
  cat("========================================\n")
}

## Run the simulation ----------------------------------------------------------
start_time_overall <- Sys.time()
parallelise_simulation()
end_time_overall <- Sys.time()

cat(
  "\nTotal simulation time:",
  difftime(end_time_overall, start_time_overall, units = "hours"),
  "hours\n\n"
)

### ============================================================================
### END OF SCRIPT
### ============================================================================