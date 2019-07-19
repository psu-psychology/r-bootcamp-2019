clean_registrant_data <- function(df) {
  if (is.null(df)) stop('Data frame is NULL.')
  
  library(tidyverse)
  
  times <- lubridate::mdy_hms(df$`Timestamp`)
  regis_clean <- dplyr::rename(df,
                               email = `Email Address`,
                               first = `First Name`,
                               last = `Last Name`,
                               dept = Department,
                               position = `Current position at Penn State`,
                               how_hear = `How did you hear about the bootcamp?`)
  
  regis_clean <- dplyr::mutate(regis_clean, regis_time = lubridate::mdy_hms(Timestamp))
  
  dplyr::select(regis_clean, -Timestamp)
}
