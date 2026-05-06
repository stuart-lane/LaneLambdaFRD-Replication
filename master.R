## =============================================================================
## 1. SET WORKING DIRECTORY AND TIMER
## =============================================================================


## Set working director to current directory -----------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  stop(paste("Please run this script from RStudio or manually set working",
             "directory below within 'master.R'"))
}

# setwd() # set your working directory here if not using RStudio

# Start master timer -----------------------------------------------------------
start_time_master <- Sys.time()

## =============================================================================
## 2. LOAD CONFIGURATIONS (CAN BE CUSTOMISED, DEFAULT SETTINGS REPLICATE PAPER)
##    AND LAMBDAFRD FUNCTION
## =============================================================================

# Load config file -------------------------------------------------------------
source("./config.R")

# Load LambdaFRD function -----------------------------------------------------
source("./LambdaFRD.R")

## =============================================================================
## 3. GENERATE MAIN SIMULATION TABLES
## =============================================================================

# Start Monte Carlo timer ------------------------------------------------------
mc_start_time <- Sys.time()

# Load simulation utility functions --------------------------------------------
source(file.path(SIMULATIONS_FOLDER, SIMULATIONS_UTILS_FILE))

# Run main Monte Carlo simulations ---------------------------------------------
source(file.path(SIMULATIONS_FOLDER, MAIN_SIMULATIONS_FILE))

# Compute time to run Monte Carlo simulations ----------------------------------
mc_end_time <- Sys.time()
mc_runtime_hours <- as.numeric(
  difftime(mc_end_time, mc_start_time, units = "hours")
)

# Produce estimator summary table ----------------------------------------------
source(file.path(SIMULATIONS_FOLDER, MAIN_ESTIMATOR_SUMMARY_FILE))

# Convert .csv files into .tex tables ------------------------------------------
python_command <- sprintf(
  "python simulations/latex/convert_simulation_csv_to_latex.py --csv-folder %s --output-folder %s",
  SIMULATION_CSV_OUTPUT_FOLDER_PATH,
  TEX_OUTPUT_FOLDER
)

system(python_command)

## =============================================================================
## 4. GENERATE ANGRIST LAVY (1999) REPLICATION TABLES 
## =============================================================================

# Load application utility functions -------------------------------------------
source(file.path(APPLICATION_FOLDER, APPLICATION_UTILS_FILE))

# Run main application analysis ------------------------------------------------
source(file.path(APPLICATION_FOLDER, MAIN_APPLICATION_FILE))

# Convert .csv file into .tex tables -------------------------------------------
python_command <- sprintf(
  "python application/latex/convert_application_csv_to_latex.py --csv-file %s --output-folder %s",
  shQuote(file.path(APPLICATION_FOLDER, APPLICATION_CSV_FILE)),
  shQuote(TEX_OUTPUT_FOLDER)
)

system(python_command)

## =============================================================================
## 5. COMPUTATIONAL COMPLEXITY
## =============================================================================

# Generate and print time taken, and computer resources ------------------------
end_time_master <- Sys.time()
total_time_master <- difftime(end_time_master, start_time_master, units = "hours")

system_info <- get_system_info()

invisible({
  cat("============================================================\n")
  cat("Replication completed successfully!\n")
  cat("------------------------------------------------------------\n")
  cat("Start time:                 ", format(start_time_master), "\n")
  cat("End time:                   ", format(end_time_master), "\n")
  cat("Total runtime:              ",
      format_hours_minutes(as.numeric(total_time_master)), "\n")
  cat("Monte Carlo runtime:        ",
      format_hours_minutes(mc_runtime_hours), "\n")
  cat(sprintf("Share of total runtime:      %.1f%%\n",
              100 * mc_runtime_hours / as.numeric(total_time_master)))
  cat(sprintf("MC core-hours used:          %.1f\n",
              mc_runtime_hours * NUM_CORES))
  cat("------------------------------------------------------------\n")
  cat("Parallel configuration:\n")
  cat("  Total CPU cores detected: ", TOTAL_CORES, "\n")
  cat("  Cores used for MC:        ", NUM_CORES, "\n")
  cat("  Parallel backend:          fork / mclapply\n")
  cat("------------------------------------------------------------\n")
  cat("System information:\n")
  cat("  OS:               ", system_info$os, system_info$os_release, "\n")
  cat("  Machine:          ", system_info$machine, "\n")
  cat("  R version:        ", system_info$r_version, "\n")
  cat("  Total RAM:        ", system_info$total_ram_gb, " GB\n")
  cat("============================================================\n\n")
})

### ============================================================================
### END OF SCRIPT
### ============================================================================