shearwater_subtitle <- function(input_csv, output_srt) {

  library(data.table)
  
  # Input Shearwater CSV file
  df <- fread(input_csv, skip = 2)
  
  # Data extraction
  df <- df[, .(`Time (ms)`, Depth, `Time To Surface (min)`, `Water Temp`)]
  df[, `Time (ms)` := as.numeric(`Time (ms)`) / 1000]
  df[, Depth := round(as.numeric(Depth), 1)]
  df[, `Time To Surface (min)` := as.character(floor(as.numeric(`Time To Surface (min)`)))]
  df[, `Water Temp` := as.integer(round(as.numeric(`Water Temp`)))]
  
  # Timestamp
  format_timestamp <- function(seconds) {
    hrs <- sprintf("%02d", floor(seconds / 3600))
    mins <- sprintf("%02d", floor((seconds %% 3600) / 60))
    secs <- sprintf("%02d", floor(seconds %% 60))
    millis <- sprintf("%03d", (seconds %% 1) * 1000)
    return(paste0(hrs, ":", mins, ":", secs, ",", millis))
  }
  
  # Generate SRT file
  srt_content <- ""
  for (i in 1:nrow(df)) {
    if (i == 1) {
      start_time <- format_timestamp(0)
      end_time <- format_timestamp(5)
    } else {
      start_time <- format_timestamp(df$`Time (ms)`[i] - 5)
      end_time <- format_timestamp(df$`Time (ms)`[i] + 5)
    }
    
    srt_content <- paste0(srt_content, i, "\n")
    srt_content <- paste0(srt_content, start_time, " --> ", end_time, "\n")
    srt_content <- paste0(srt_content, "Depth: ", df$Depth[i], "m\n")
    srt_content <- paste0(srt_content, "TTS: ", df$`Time To Surface (min)`[i], "min\n")
    srt_content <- paste0(srt_content, "Temp: ", df$`Water Temp`[i], "°C\n\n")
  }
  
  # Output SRT file
  writeLines(srt_content, output_srt)
}

# Example usage:
# source("shearwater_subtitle.R")
# shearwater_subtitle("input.csv", "output.srt")