perform_centroiding <- function(file_in,
                                outdir = NA,
                                format = "mzml") {
  
  # construct file_out
  file_out <- paste0(outdir, "/", basename(file_in))
  
  ms_profile_data <- readMSData(file_in,
                                mode = "onDisk")
  
  if(any(msLevel(ms_profile_data) == 1L)) {
    
    if(any(msLevel(ms_profile_data) > 1L)) {
      
      # Perform smoothing and centroiding only on MS1
      suppressWarnings(
        ms_centroid_data <- pickPeaks(smooth(ms_profile_data,
                                             method = "SavitzkyGolay",
                                             halfWindowSize = 6L,
                                             msLevel. = 1L),
                                      refineMz = "descendPeak",
                                      signalPercentage = 33,
                                      msLevel. = 1L)
      )
      
      # Perform centroid on MS1 and MS2
      suppressWarnings(
        ms_centroid_data <- pickPeaks(ms_centroid_data,
                                      halfWindowSize = 4L,
                                      SNR = 1L,
                                      refineMz = "descendPeak",
                                      signalPercentage = 50,
                                      msLevel. = 2L)
      )
      
    } else {
      message("File does not contain any MS1 spectra.")
    }
  }
  
  writeMSData(ms_centroid_data,
              file = file_out,
              outformat = format,
              copy = TRUE)
  
}
