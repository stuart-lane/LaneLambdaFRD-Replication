cat("Running: ./simulations/estimator_summary.R", "\n\n")

## Load all simulation CSVs ---------------------------------------------------
files <- list.files(
  path = SIMULATION_CSV_OUTPUT_FOLDER_PATH,
  pattern = "^simulation.*\\.csv$",
  full.names = TRUE
)

master_df <- do.call(rbind, lapply(files, read.csv))

cat("Total configurations loaded:", nrow(master_df), "\n")
cat("Files loaded:\n")
cat(paste(" ", files), sep = "\n")
cat("\n")

## Estimator names and metric prefixes ----------------------------------------
estimator_names <- c("iv_ccf", "iv_ik", "lambda_1_ccf",
                     "lambda_1_ik", "lambda_4_ccf", "lambda_4_ik")

metrics <- list(
  median_bias = list(prefix = "median_bias_", use_abs = TRUE),
  mad         = list(prefix = "mad_",         use_abs = FALSE),
  rmse        = list(prefix = "rmse_",        use_abs = FALSE)
)

## Helper: count wins, skipping ties and non-finite values --------------------
count_wins <- function(df, cols, use_abs) {
  wins <- integer(length(cols))
  names(wins) <- estimator_names
  n_skipped <- 0
  
  for (r in seq_len(nrow(df))) {
    row <- as.numeric(df[r, cols])
    row[!is.finite(row)] <- NA
    vals <- if (use_abs) abs(row) else row
    
    min_val <- min(vals, na.rm = TRUE)
    best <- which(vals == min_val)
    
    if (length(best) == 1) {
      wins[best] <- wins[best] + 1
    } else {
      n_skipped <- n_skipped + 1
    }
  }
  
  attr(wins, "n_skipped") <- n_skipped
  wins
}

## Summary by error distribution ----------------------------------------------
for (ed in sort(unique(master_df$error_distribution))) {
  
  sub_df <- master_df[master_df$error_distribution == ed, ]
  ed_label <- ifelse(ed == 1, "Normal", "t (heavy-tailed)")
  
  cat("================================================================\n")
  cat("Error distribution:", ed_label, "(", nrow(sub_df), "configurations )\n")
  cat("================================================================\n\n")
  
  results_matrix <- matrix(
    0L,
    nrow = length(metrics),
    ncol = length(estimator_names),
    dimnames = list(names(metrics), estimator_names)
  )
  
  skipped_vec <- setNames(integer(length(metrics)), names(metrics))
  
  for (metric_name in names(metrics)) {
    metric_info <- metrics[[metric_name]]
    cols <- paste0(metric_info$prefix, estimator_names)
    wins <- count_wins(sub_df, cols, metric_info$use_abs)
    results_matrix[metric_name, ] <- wins
    skipped_vec[metric_name] <- attr(wins, "n_skipped")
  }
  
  print(as.data.frame(results_matrix))
  cat("\nTied/skipped rows per metric:\n")
  print(skipped_vec)
  cat("\n")
}

## Overall summary (all error distributions combined) -------------------------
cat("================================================================\n")
cat("Overall ( all", nrow(master_df), "configurations )\n")
cat("================================================================\n\n")

overall_matrix <- matrix(
  0L,
  nrow = length(metrics),
  ncol = length(estimator_names),
  dimnames = list(names(metrics), estimator_names)
)

for (metric_name in names(metrics)) {
  metric_info <- metrics[[metric_name]]
  cols <- paste0(metric_info$prefix, estimator_names)
  wins <- count_wins(master_df, cols, metric_info$use_abs)
  overall_matrix[metric_name, ] <- wins
}

print(as.data.frame(overall_matrix))

## Save -----------------------------------------------------------------------
write.csv(
  as.data.frame(overall_matrix),
  file.path(SIMULATION_CSV_OUTPUT_FOLDER_PATH, "estimator_summary.csv"),
  row.names = TRUE
)

cat("\n✓ Summary saved to estimator_summary.csv\n")
