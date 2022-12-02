# function to perform qc plots
qc_plot <- function(output,
                    settings,
                    BPPARAM = SerialParam()) {
  
  # get files for conversion
  centroid_files <- list.files(output,
                               pattern = ".mzML$",
                               full.names = TRUE)
  
  # create folder for output
  if(!dir.exists(paste0(output, "/qc_plots"))) {
    dir.create(paste0(output, "/qc_plots"))
  }
  
  # perform centroiding on each file
  percentile <- bplapply(centroid_files,
                         .qc_plot_single,
                         BPPARAM = SerialParam(),
                         outdir = paste0(output, "/qc_plots"))
  
  
  success_data <- read.csv(paste0(output, "/summary.csv"))
  
  write.csv(cbind(success_data,
                  perc_int = unlist(percentile)),
            paste0(output, "/summary.csv"),
            row.names = FALSE)
  
}

# function to perform centroiding on a single file
.qc_plot_single <- function(file_in,
                            outdir = NA,
                            percentile = 0.05) {
  
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
                                   msLevel = 1L,
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
         ylim = c(0, 1.1))

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
