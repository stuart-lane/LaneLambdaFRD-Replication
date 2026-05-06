### ============================================================================
### HELPER FUNCTIONS FOR SIMULATUONS
### ============================================================================

target_mad = 0.2

scale_to_mad <- function(x, target = target_mad) {
  current_mad <- median(abs(x - median(x)))
  x * (target / current_mad)
}

generate_rdd_data <- function(
  n, running_variable, setup, p1, dgp, x0, error_distribution) 
{

  p0 <- 1 - p1

  # Generate the running variable x based on the specified type
  if (running_variable == 1) {
    x <- rnorm(n, mean = 0, sd = 1)
  } else if (running_variable == 2) {
    x <- runif(n, min = -1, max = 1)
  } else {
    x <- 2 * rbeta(n, shape1 = 2, shape2 = 4) - 1
  }

  # Generate error terms
  if (error_distribution == 1) {
    u <- rnorm(n, mean = 0, sd = 0.3)
  } else {
    u <- rt(n, df = 2.5)
    u <- scale_to_mad(u)
  }
  
  # Generate treatments
  if (setup == 1) {
    P <- ifelse(x < x0, p0, p1)
    T <- rbinom(n, 1, P)
  } else if (setup == 2) {
    P <- ifelse(x < -1, 0, ifelse(x <= x0, (1 - p1) * x + (1 - p1),
                                  ifelse(x < 1, (1 - p1) * x + p1, 1)))
    T <- rbinom(n, 1, P)
  } else {
    P <- (x < x0) * p0 * exp(0.2*x) + (x >= x0) * (p1 + p0 * (1 - exp(-0.2*x)))
    T <- rbinom(n, 1, P)
  }

  # Generate structural function
  X <- cbind(1, x, x^2, x^3, x^4, x^5)
  if (dgp == 1) {
    y <- (X %*% COEFFS_CONT_LEE) * (x < x0) + (X %*% COEFFS_TREAT_LEE) * (x >= x0) + u
  } else {
    y <- (X %*% COEFFS_CONT_LM) * (x < x0) + (X %*% COEFFS_TREAT_LM) * (x >= x0) + u
  }

  return(list(y = y, D = T, x = x, exog = NULL))
}

calculate_ROT1 <- function(d, cutoff) {
  # Fit fourth-order polynomials on either side of the cutoff
  fit_y_below <- lm(d$Y ~ d$X + I(d$X^2) + I(d$X^3) + I(d$X^4), subset = which(d$X < cutoff))
  fit_y_above <- lm(d$Y ~ d$X + I(d$X^2) + I(d$X^3) + I(d$X^4), subset = which(d$X >= cutoff))
  fit_d_below <- lm(d$D ~ d$X + I(d$X^2) + I(d$X^3) + I(d$X^4), subset = which(d$X < cutoff))
  fit_d_above <- lm(d$D ~ d$X + I(d$X^2) + I(d$X^3) + I(d$X^4), subset = which(d$X >= cutoff))
  
  # Create a grid of x values to evaluate the second derivatives
  x_grid_below <- seq(min(d$X[d$X < cutoff]), 0, length.out = 100)
  x_grid_above <- seq(0, max(d$X[d$X >= cutoff]), length.out = 100)
  
  # Calculate second derivatives for outcome function
  y_second_deriv_below <- 2*coef(fit_y_below)[3] +
    6*coef(fit_y_below)[4]*x_grid_below +
    12*coef(fit_y_below)[5]*x_grid_below^2
  
  y_second_deriv_above <- 2*coef(fit_y_above)[3] +
    6*coef(fit_y_above)[4]*x_grid_above +
    12*coef(fit_y_above)[5]*x_grid_above^2
  
  # Calculate second derivatives for treatment function
  d_second_deriv_below <- 2*coef(fit_d_below)[3] +
    6*coef(fit_d_below)[4]*x_grid_below +
    12*coef(fit_d_below)[5]*x_grid_below^2
  
  d_second_deriv_above <- 2*coef(fit_d_above)[3] +
    6*coef(fit_d_above)[4]*x_grid_above +
    12*coef(fit_d_above)[5]*x_grid_above^2
  
  # Find maximum absolute second derivatives
  B_Y <- max(max(abs(y_second_deriv_below)), max(abs(y_second_deriv_above)))
  B_T <- max(max(abs(d_second_deriv_below)), max(abs(d_second_deriv_above)))
  
  return(c(B_Y, B_T))
}

calculate_ROT2 <- function(d, x0) {
  # Fit quadratic polynomials on either side of the cutoff
  fit_y_below <- lm(d$Y ~ d$X + I(d$X^2), subset = which(d$X < x0))
  fit_y_above <- lm(d$Y ~ d$X + I(d$X^2), subset = which(d$X >= x0))
  fit_d_below <- lm(d$D ~ d$X + I(d$X^2), subset = which(d$X < x0))
  fit_d_above <- lm(d$D ~ d$X + I(d$X^2), subset = which(d$X >= x0))

  # Get the second derivatives (twice the coefficient of x²)
  y_second_deriv_below <- 2 * coef(fit_y_below)[3]
  y_second_deriv_above <- 2 * coef(fit_y_above)[3]
  d_second_deriv_below <- 2 * coef(fit_d_below)[3]
  d_second_deriv_above <- 2 * coef(fit_d_above)[3]

  # Find maximum absolute second derivatives and multiply by 2
  B_Y <- 2 * max(abs(y_second_deriv_below), abs(y_second_deriv_above))
  B_T <- 2 * max(abs(d_second_deriv_below), abs(d_second_deriv_above))

  return(c(B_Y, B_T))
}

### ============================================================================
### GENERATE FIGURE FUNCTION
### ============================================================================

produce_theoretical_comparison <- function(n, nsim, true_treatment_effect,
                                           generate_matlab_files, figure_folder) {
  # Create empirical estimates dataframe
  empirical_estimates <- data.frame(
    estimator = c(rep("FRD", nsim), rep("Lambda_1", nsim), rep("Lambda_4", nsim)),
    value = c(
      results_iv_ik - true_treatment_effect,
      results_lambda_1_ik - true_treatment_effect,
      results_lambda_4_ik - true_treatment_effect),
    rep = rep(1:nsim, 3)
  )
  
  # Calibrate to  λ = Λ(4)
  lambda_mean <- mean(results_lambda_4_ik)
  lambda_var <- var(results_lambda_4_ik)
  lambda_sd <- sd(results_lambda_4_ik)
  
  # For Normal: centre at true effect, use λ = Λ(4) variance
  normal_mean <- 0
  normal_sd <- lambda_sd
  
  # For Cauchy: centre at true effect, use λ = Λ(4) for scale parameter
  cauchy_location <- 0
  # Convert λ = Λ(4) standard deviation to approximate Cauchy scale
  cauchy_scale <- lambda_sd / 1.814
  lambda_IQR <- IQR(results_lambda_4_ik)
  cauchy_scale <- lambda_IQR / 2  # IQR of standard Cauchy is 2
  
  # Trim extreme values that distort the visualisation
  q01 <- quantile(empirical_estimates$value, 0.01)
  q99 <- quantile(empirical_estimates$value, 0.99)
  min_x <- max(min(empirical_estimates$value), q01 - 2*(q99-q01))
  max_x <- min(max(empirical_estimates$value), q99 + 2*(q99-q01))
  
  # Use trimmed range for theoretical density calculation
  x_grid <- seq(min_x, max_x, length.out = 1500)
  
  # Generate theoretical density data
  theoretical_data <- data.frame(
    x = rep(x_grid, 2),
    y = c(
      dnorm(x_grid, mean = normal_mean, sd = normal_sd),
      dcauchy(x_grid, location = cauchy_location, scale = cauchy_scale)
    ),
    estimator = rep(c("Normal", "Cauchy"), each = length(x_grid))
  )
  
  empirical_estimates$estimator <- factor(empirical_estimates$estimator,
                                          levels = c("FRD", "Lambda_1", "Lambda_4"))
  theoretical_data$estimator <- factor(theoretical_data$estimator,
                                       levels = c("Normal", "Cauchy"))
  

  estimator_labels <- c(
    "FRD" = TeX("$\\lambda = 1$"),
    "Lambda_1" = TeX("$\\lambda = \\Lambda(1)$"),
    "Lambda_4" = TeX("$\\lambda = \\Lambda(4)$"),
    "Normal" = "Normal",
    "Cauchy" = "Cauchy"
  )
  
  # Create the plot using geom_density for smoother empirical curves
  density_plot <- ggplot() +
    geom_density(data = empirical_estimates,
                 aes(x = value, color = estimator),
                 alpha = 0, size = 1.2) +
    geom_line(data = theoretical_data,
              aes(x = x, y = y, color = estimator),
              size = 1) +
    geom_vline(xintercept = 0,
               linetype = "dotted", color = "black", size = 0.8) +
    scale_x_continuous(
      limits = c(-1, 1),
      "Estimated treatment effect"
    ) +
    scale_color_manual(
      name = NULL,
      values = c("Lambda_4" = "#E41A1C", "Lambda_1" = "#FF7F00", "FRD" = "#377EB8",
                 "Normal" = "#4DAF4A", "Cauchy" = "#984EA3"),
      labels = estimator_labels
    ) +
    labs(
      x = "Estimated treatment effect",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 16)
    ) +
    guides(
      color = guide_legend(override.aes = list(linetype = "solid", fill = NA))
    )
  
  # Print the plot
  print(density_plot)
    
  if (!generate_matlab_files) {
    
    ggsave(
      paste0(figure_folder, "/Figure_", (n / 300), ".pdf"),
      density_plot,
      width = 8.5,
      height = 6,
      device = cairo_pdf,
      dpi = 300
    ) 
  } else {
    # Export data for MATLAB
    output_data <- list(
      # Empirical estimates
      empirical_estimates = empirical_estimates,
      
      # Theoretical distribution parameters
      theoretical_params = list(
        normal_mean = normal_mean,
        normal_sd = normal_sd,
        cauchy_location = cauchy_location,
        cauchy_scale = cauchy_scale
      ),
      
      # Grid for theoretical densities
      x_grid = x_grid,
      theoretical_data = theoretical_data,
      
      # Other parameters
      true_treatment_effect = true_treatment_effect,
      n = n,
      nsim = nsim,
      
      # Plot limits
      xlim = c(-1, 1)
    )
    
    write.csv(empirical_estimates, 
              paste0("simulations/empirical_estimates_n", n, ".csv"), 
              row.names = FALSE)
    
    write.csv(theoretical_data, 
              paste0("simulations/theoretical_data_n", n, ".csv"), 
              row.names = FALSE)
    
    write.csv(data.frame(
      parameter = c("normal_mean", "normal_sd", "cauchy_location", "cauchy_scale", 
                    "true_effect", "n", "nsim"),
      value = c(normal_mean, normal_sd, cauchy_location, cauchy_scale, 
                true_treatment_effect, n, nsim)
    ), paste0("simulations/parameters_n", n, ".csv"), row.names = FALSE)
  }
  
  return(density_plot)
}