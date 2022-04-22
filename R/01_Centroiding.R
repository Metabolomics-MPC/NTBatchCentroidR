# function to perform centroiding on all files
perform_centroiding <- function(settings) {
  
  # get files for conversion
  profile_files <- list.files(settings$files_in,
                              pattern = ".mzML$",
                              full.names = TRUE)
  
  # TODO switch from for loop to bplapply
  
  # create progress bar
  pb = txtProgressBar(min = 0, max = length(profile_files), initial = 0)
  
  for(i in 1:length(profile_files)) {
    
    .perform_centroiding_single(profile_files[i],
                                outdir = settings$files_out,
                                format = settings$format)
    
    setTxtProgressBar(pb,i)
    
  }
  close(pb)
  
}


# function to perform centroiding on a single file
.perform_centroiding_single <- function(file_in,
                                        outdir = NA,
                                        format = "mzml") {
  
  # construct file_out
  file_out <- paste0(outdir, "/", basename(file_in))
  
  # if file exists, delete it
  if(file.exists(file_out)) {
    file.remove(file_out)
  }
  
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
                                      halfWindowSize = 3L,
                                      SNR = 0L,
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
