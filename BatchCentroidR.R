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
if(!length(args)) {
  
  message("Running demo data!")
  input <- "mzML_profile"
  output <- "mzML_centroid_MSnbase"
  settings_file <- paste0(input, "/settings.yaml")
  
} else {
  
  # check for correct length of arguments
  if(!length(args) == 2) {
    
  }
  
  # check if input folder exists
  if(!dir.exists(args[1])) {
    stop(paste0("Input folder ", args[1], " does not exist!"))
  }
  
  # check if settings file is present in input
  if(!file.exists(paste0(args[1], "/settings.yaml"))) {
    stop("Missing settings.yaml in input folder!")
  }
  
  # check if mzML files exist in input folder
  mzML_files <- list.files(args[1],
                           pattern = ".mzML$")
  
  if(!length(mzML_files)) {
    stop("No .mzML files found in input folder")
  }
  
  # check for output folder and create if not present
  if(!dir.exists(args[2])) {
    dir.create(args[2])
  }
  
  input <- args[1]
  output <- args[2]
  settings_file <- paste0(input, "/settings.yaml")
}


# ==============================================================================
# 0. Setup 
# ==============================================================================
# source required functions
source("R/00_Setup.R")

# Read in settings of yaml file ------------------------------------------------
settings <- read_yaml(settings_file)

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
perform_centroiding(input,
                    output,
                    settings,
                    BPPARAM = BPParam)
