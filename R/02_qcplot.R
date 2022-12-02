# function to perform qc plots
qc_plot <- function(output,
                    settings,
                    BPPARAM = SerialParam()) {
  
  # get files for conversion
  centroid_files <- list.files(output,
                               pattern = ".mzML$",
                               full.names = TRUE)
  
  # perform qc plots on MS1 data
  if(settings$ms1_qc) {
    
    # create folder for output
    if(!dir.exists(paste0(output, "/ms1_qc_plots"))) {
      dir.create(paste0(output, "/ms1_qc_plots"))
    }
    
    # perform centroiding on each file
    percentile <- bplapply(centroid_files,
                           .qc_plot_single,
                           BPPARAM = SerialParam(),
                           outdir = paste0(output, "/ms1_qc_plots"),
                           msLevel = 1L)
    
    
    success_data <- read.csv(paste0(output, "/success.csv"))
    
    write.csv(cbind(success_data,
                    ms1 = unlist(percentile)),
              paste0(output, "/success.csv"),
              row.names = FALSE)
    
  }
  
  # perform qc plots on MS1 data
  if(settings$ms2_qc) {
    
    # create folder for output
    if(!dir.exists(paste0(output, "/ms2_qc_plots"))) {
      dir.create(paste0(output, "/ms2_qc_plots"))
    }
    
    # perform centroiding on each file
    percentile <- bplapply(centroid_files,
                           .qc_plot_single,
                           BPPARAM = SerialParam(),
                           outdir = paste0(output, "/ms2_qc_plots"),
                           msLevel = 2L)
    
    
    success_data <- read.csv(paste0(output, "/success.csv"))
    
    write.csv(cbind(success_data,
                    ms2 = unlist(percentile)),
              paste0(output, "/success.csv"),
              row.names = FALSE)
    
  }
}

# function to perform centroiding on a single file
.qc_plot_single <- function(file_in,
                            outdir = NA,
                            percentile = 0.05,
                            msLevel = 1L) {
  
  gc()
  
  success <- tryCatch({
    
    # workaround until bplapply loads MSnbase
    suppressWarnings(library(MSnbase))
    
    # construct file_out
    file_out <- paste0(outdir, "/", basename(file_in), ".jpg")
    
    # if file exists, delete it
    if(file.exists(file_out)) {
      file.remove(file_out)
    }
    
    # read data and get intensities of all centroids
    ms_centroid_data <- readMSData(file_in,
                                   msLevel = msLevel,
                                   centroided = TRUE,
                                   mode = "onDisk")

    intensities <- unname(unlist(intensity(ms_centroid_data)))
    
    intensities <- intensities[which(intensities > 0)]
    
    # create histogram of intensities
    jpeg(file = file_out,
        width = 500,
        height = 500)

    hist(log10(intensities),
         freq = FALSE,
         ylim = c(0, 1.1),
         main = paste0(basename(file_in), " / MS", msLevel))

    abline(v = quantile(log10(intensities),
                        probs = percentile,
                        na.rm = TRUE),
           col = "red",
           lty = "dashed")

    dev.off()
    
    # return value with percentile
    quantile(intensities,
             probs = percentile,
             na.rm = TRUE)
    
  }, error = function(e) {return(1000)})
  
}
