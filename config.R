### ============================================================================
### PACKAGE REQUIREMENTS
### ============================================================================

packages <- c("dplyr", "haven", "rdrobust", "RDHonest", "parallel",
              "doParallel", "foreach", "expm", "ggplot2", "latex2exp")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Python pandas is required for the custom csv-to-latex scripts
INSTALL_PANDAS <- TRUE

if (INSTALL_PANDAS) {
  system('python -m pip install --quiet --upgrade "pandas>=2.0.0"',
         ignore.stdout = TRUE)
  cat("Python dependencies verified\n") 
}

### ============================================================================
### PATH CONFIGURATIONS (DO NOT CHANGES THESE)
### ============================================================================

# DIRECTORIES
SIMULATIONS_FOLDER <- "simulations"
APPLICATION_FOLDER <- "application"
MAIN_OUTPUT_FOLDER <- "output"
TEX_OUTPUT_FOLDER <- "tex_tables"
FIGURE_FOLDER <- "figures"
SIMULATION_CSV_OUTPUT_FOLDER <- "output"

# MAIN FILES
MAIN_SIMULATIONS_FILE <- "simulations_main.R"
MAIN_ESTIMATOR_SUMMARY_FILE <- "estimator_summary.R"
MAIN_FIGURES_FILE <- "generate_figures.R"
MAIN_APPLICATION_FILE <- "application_main.R"

# OTHER FILES
SIMULATIONS_UTILS_FILE <- "simulation_utils.R"
APPLICATION_UTILS_FILE <- "application_utils.R"
APPLICATION_DATA_FILE <- "data/final4.dta"
APPLICATION_CSV_FILE <- "anglavy99_output.csv"

### ============================================================================
### CONFIGURATION FOR MAIN SIMULATION LOOP
### ============================================================================

# Note: SIMULATIONS_CSV_OUTPUT_FOLDER below is for intermediate csv files with 
# the simulations directory, not the final output folder
SIMULATION_CSV_OUTPUT_FOLDER_PATH <- file.path(SIMULATIONS_FOLDER,
                                               SIMULATION_CSV_OUTPUT_FOLDER)
TEX_OUTPUT_FOLDER <- file.path(MAIN_OUTPUT_FOLDER, TEX_OUTPUT_FOLDER)

# Simulation parameters --------------------------------------------------------

# Number of repetitions per parameter configuration
NSIM = 10000  

# Parallel processing parameters
TOTAL_CORES <- parallel::detectCores()                 # Detect total cores
cat("Total cores:", TOTAL_CORES, "\n")
cat("Unused cores must be at most:", TOTAL_CORES - 1, "\n")
UNUSED_CORES <- 1                                      # Set unused cores
NUM_CORES <- max(1, TOTAL_CORES - UNUSED_CORES)

# Parameter grid
DGPS <- c(1, 2)                     # Structural function design
RUNNING_VARIABLES <- c(1, 3)     # Distribution of X
SETUPS <- c(1, 2, 3)                   # Treatment assignment probability function
P1_VALUES <- c(0.6, 0.7, 0.8, 0.9)  # Treatment assignment probability constant
ERROR_DISTRIBUTIONS <- c(2)      # Error distributions
NS <- c(300, 600)                   # Sample size

# Cutoff
X0 = 0

# Polynomial coefficients for control/treatment potential outcomes (Lee is dgp=1, LM is dgp=2)
COEFFS_CONT_LEE <- matrix(c(0.48, 1.27, 7.18, 20.21, 21.54, 7.33), ncol = 1)
COEFFS_TREAT_LEE <- matrix(c(0.52, 0.84, -3, 7.99, -9.01, 3.56), ncol = 1)
COEFFS_CONT_LM <- matrix(c(3.7, 2.99, 3.28, 1.45, 0.22, 0.03), ncol = 1)
COEFFS_TREAT_LM <- matrix(c(0.26, 18.49, -54.8, 74.3, -45.02, 9.83), ncol = 1)


### ============================================================================
### CONFIGURATION FOR FIGURES
### ============================================================================

FIGURE_FOLDER_PATH <- file.path(MAIN_OUTPUT_FOLDER, FIGURE_FOLDER)

# These parameters have suffix "_FIGURES" to allow them to explicitly vary
# from the main simulation loop parameters
NSIM_FIGURES <- 20000

X0_FIGURES <- 0 
RUNNING_VARIABLE_FIGURES = 1               
SETUP_FIGURES = 1                         
DGP_FIGURES = 1                            
P1_FIGURES = 0.8                          
TRUE_TREATMENT_EFFFECT_FIGURES <- 0.04

# Sample sizes for the two figures
NS_FIGURES <- c(300, 600)

# NOTE: Figures in the paper use MATLAB for styling/appearance (personal preference).
# Setting FALSE uses ggplot2 instead - data identical, styling different.
GENERATE_MATLAB_FILES <- FALSE

REMOVE_INTERMEDIATE_CSV_FILES <- TRUE

# Only applicable if GENERATE_MATLAB_FILES <- TRUE
OPERATING_SYSTEM = "Windows" # Options: ("Windows", "Mac/Linux")


### ============================================================================
### CONFIGURATION FOR ANGRIST & LAVY (1999) APPLICATION
### ============================================================================

APPLICATION_DATA_FILE_PATH <- file.path(APPLICATION_FOLDER, 
                                        APPLICATION_DATA_FILE)
APPLICATION_CSV_FILE_PATH <- file.path(APPLICATION_FOLDER, APPLICATION_CSV_FILE) 
APPLICATION_TEX_OUTPUT_FOLDER <- file.path(MAIN_OUTPUT_FOLDER, TEX_OUTPUT_FOLDER)

## Parameter configurations
TESTS <- c("verb", "math")
BANDWIDTHS <- c(6, 8, 10, 12, 14, 16, 18)
CUTOFFS <- c(40)

# Bias-aware Anderson-Rubin test grid parameters
AR_LOWER <- -5
AR_UPPER <- 5
GRID_POINTS <- 250

## Parameter values for printing results in console (if PRINT_OUTPUT == TRUE)
PRINT_OUTPUT = FALSE
DECIMAL_PLACES = 2
WIDTH = DECIMAL_PLACES + 4

### ============================================================================
### HELPER FUNCTIONS FOR SYSTEM/COMPUTATION INFORMATION/TIMINGS
### ============================================================================

get_system_info <- function() {
  info <- list(
    os = Sys.info()[["sysname"]],
    os_release = Sys.info()[["release"]],
    machine = Sys.info()[["machine"]],
    r_version = R.version.string,
    cpu_cores = parallel::detectCores(logical = TRUE),
    cpu_cores_physical = tryCatch(
      parallel::detectCores(logical = FALSE),
      error = function(e) NA
    ),
    total_ram_gb = tryCatch(
      round(as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo",
                              intern = TRUE)) / 1024^2, 2),
      error = function(e) NA
    )
  )
  return(info)
}

format_hours_minutes <- function(hours) {
  total_minutes <- round(hours * 60)
  h <- total_minutes %/% 60
  m <- total_minutes %% 60
  sprintf("%d hours %d minutes", h, m)
}
