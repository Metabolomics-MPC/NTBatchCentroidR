# function to perform centroiding on all files
perform_centroiding <- function(input,
                                output,
                                settings,
                                BPPARAM = SerialParam()) {
  
  # register specific parallel backend
  #register(bpstart(BPParam))
  
  # get files for conversion
  profile_files <- list.files(input,
                              pattern = ".mzML$",
                              full.names = TRUE)
  
  # perform centroiding on each file
  success <- bplapply(profile_files,
                      .perform_centroiding_single,
                      BPPARAM = BPPARAM,
                      outdir = output,
                      format = settings$format,
                      ms1_smooth_method = settings$ms1_smooth_method,
                      ms1_smooth_halfWindowSize = settings$ms1_smooth_halfWindowSize,
                      ms1_pick_halfWindowSize = settings$ms1_pick_halfWindowSize,
                      ms1_pick_snr = settings$ms1_pick_snr,
                      ms1_pick_refineMz = settings$ms1_pick_refineMz,
                      ms1_pick_signalPercentage = settings$ms1_pick_signalPercentage,
                      ms2_pick_halfWindowSize = settings$ms2_pick_halfWindowSize,
                      ms2_pick_snr = settings$ms2_pick_snr,
                      ms2_pick_refineMz = settings$ms2_pick_refineMz,
                      ms2_pick_signalPercentage = settings$ms2_pick_signalPercentage)
  
  # write overview file if all samples could be processed
  write.csv(cbind(path = basename(profile_files),
                  success = success),
            paste0(output, "/success.csv"),
            row.names = FALSE)
  
  # write summary file required by SLAW
  write.csv(cbind(path = basename(profile_files),
                  type = "sample"),
            paste0(output, "/samples.csv"),
            row.names = FALSE)
  
}


# function to perform centroiding on a single file
.perform_centroiding_single <- function(file_in,
                                        outdir = NA,
                                        format = "mzml",
                                        ms1_smooth_method = "SavitzkyGolay",
                                        ms1_smooth_halfWindowSize = 6L,
                                        ms1_pick_halfWindowSize = 3L,
                                        ms1_pick_snr = 0L,
                                        ms1_pick_refineMz = "descendPeak",
                                        ms1_pick_signalPercentage = 33,
                                        ms2_pick_halfWindowSize = 4,
                                        ms2_pick_snr = 1,
                                        ms2_pick_refineMz = "descendPeak",
                                        ms2_pick_signalPercentage = 50) {
  
  
  success <- tryCatch({
    
    # workaround until bplapply loads MSnbase
    suppressWarnings(library(MSnbase))
    
    # construct file_out
    file_out <- paste0(outdir, "/", basename(file_in))
    
    # if file exists, delete it
    if(file.exists(file_out)) {
      file.remove(file_out)
    }
    
    ms_profile_data <- readMSData(file_in,
                                  cache. = 0L,
                                  mode = "onDisk")
    
    if(any(msLevel(ms_profile_data) == 1L)) {
      
      if(any(msLevel(ms_profile_data) > 1L)) {
        
        # Perform smoothing and centroiding only on MS1
        # suppressWarnings(
        #   ms_centroid_data <- pickPeaks(smooth(ms_profile_data,
        #                                        method = ms1_smooth_method,
        #                                        halfWindowSize = ms1_smooth_halfWindowSize,
        #                                        msLevel. = 1L),
        #                                 halfWindowSize = ms1_pick_halfWindowSize,
        #                                 SNR = ms1_pick_snr,
        #                                 refineMz = ms1_pick_refineMz,
        #                                 signalPercentage = ms1_pick_signalPercentage,
        #                                 msLevel. = 1L)
        # )
        
        # Perform centroiding only on MS1
        suppressWarnings(
          ms_centroid_data <- pickPeaks(ms_profile_data,
                                        halfWindowSize = ms1_pick_halfWindowSize,
                                        SNR = ms1_pick_snr,
                                        refineMz = ms1_pick_refineMz,
                                        signalPercentage = ms1_pick_signalPercentage,
                                        msLevel. = 1L)
        )
        
        
        # Perform centroid on MS1 and MS2
        suppressWarnings(
          ms_centroid_data <- pickPeaks(ms_centroid_data,
                                        halfWindowSize = ms2_pick_halfWindowSize,
                                        SNR = ms2_pick_snr,
                                        refineMz = ms2_pick_refineMz,
                                        signalPercentage = ms2_pick_signalPercentage,
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
    
    TRUE
    
  }, error = function(e) {return(FALSE)})
  
}
