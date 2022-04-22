# ==============================================================================
# Smoothing and centroiding of profile mode data from Sciex QToF instruments
#
# Authors:
# - Michael Witting, HMGU
#
# This data analysis workflow perform annotation of untargeted LC-MS data on the
# MS1 and MS2 level using different libraries and matching functions
# ==============================================================================
# get project directory to work on
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# check if args is supplied, else run demote data
if(is.na(args[1])) {
  message("Running demo data!")
  settings_yaml <- "settings.yaml"
} else {
  settings_yaml <- args[1]
}

# ==============================================================================
# 0. Setup 
# ==============================================================================
# source required functions
source("R/00_Setup.R")

# Read in settings of yaml file ------------------------------------------------
settings <- read_yaml(settings_yaml)

# validate settings ------------------------------------------------------------
#settings <- validateSettings(settings)

# setup parallel backend -------------------------------------------------------
if(is.na(settings$cores) | settings$cores == 1) {
  BPParam <- SerialParam()
} else {
  if(.Platform$OS.type == "windows") {
    BPParam <- SnowParam(workers = settings$cores,
                         tasks = 5,
                         progressbar = TRUE)
  } else {
    BPParam <- MulticoreParam(workers = settings$cores,
                              tasks = 5,
                              progressbar = TRUE)
  }
}

# Store yaml file in output directory
#write_yaml(settings, paste0(settings$output_dir, "/input_settings.yaml"))

# ==============================================================================
# 1. perform centroiding
# ==============================================================================
# source required functions ----------------------------------------------------
source("R/01_Centroiding.R")

# perform centroiding on folder specified in settings
perform_centroiding(settings, BPPARAM = BPParam)
