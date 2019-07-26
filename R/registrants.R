# registrants.R
#
# This set of helper functions gathers the registration data for the 2019 R Bootcamp
# and then cleans it.
#
# We gather the relevant functions together in one file to make it easier to source
# the relevant ones depending on the context.
#
# Author: Rick Gilmore, rog1@psu.edu

get_registration_data <- function(verbose = FALSE, sheet_name = 'PSU R Bootcamp 2019 Sign-up (Responses)') {
  # Download 2019 Bootcamp registration data from GoogleSheet
  library(googledrive)
  library(googlesheets)
  
  drive_auth(use_oob = TRUE)
  options(httr_oob_default = TRUE)
  
  bootcamp_gs <- googlesheets::gs_title(sheet_name)
  bootcamp_data <- googlesheets::gs_read(ss = bootcamp_gs,
                                         ws = 'Form Responses 1')
  bootcamp_data
}

clean_registration_data <- function(df) {
  # Inelegant, but easy to follow
  df0 <- convert_timestamp(df)
  df1 <- clean_registration_field_names(df0)
  df2 <- clean_dept_field(df1)
  df3 <- clean_position_field(df2)
  return(df3)
}

save_registration_data <- function(df, fn = "data/csv/registrants.csv") {
  df <- dplyr::select(df, -email, -first, -last)
  write.csv(df, fn, row.names = FALSE)
}

update_registration_data <- function() {
  save_registration_data(
    clean_registration_data(
      get_registration_data()
    ) 
  )
}

clean_dept_field <- function(df) {
  if (is.null(df)) stop('Data frame is NULL.')
  
  # "Teaching & Learning with Technology - AND - Geography/GIS"
  tlt <- stringr::str_detect(df$dept, "Teaching ")
  
  # "Educational Leadership"
  ed_ldr <- stringr::str_detect(df$dept, "Educational Leadership")
  
  # Psychology
  some_psych <- stringr::str_detect(df$dept, "(p|P)(s|a)ych")
  io <- stringr::str_detect(df$dept, "I/O")
  devel <- stringr::str_detect(df$dept, "(D|d)evel")
  soc <- stringr::str_detect(df$dept, "(S|s)ocial")
  clin <- stringr::str_detect(df$dept, "(C|c)lin")
  cog <- stringr::str_detect(df$dept, "(C|c)ogn")
  
  # HDFS / Criminology
  hdfs <- stringr::str_detect(df$dept, "HDFS")
  
  # Biobehavioral Health
  bbh <- stringr::str_detect(df$dept, "Biobe") | stringr::str_detect(df$dept, "BBH")
  
  # Neuroscience
  neuro <- stringr::str_detect(df$dept, "Neuro")
  
  out_df <- df
  out_df$dept[some_psych] <- "Psych"
  out_df$dept[tlt] <- "TLT"
  out_df$dept[ed_ldr] <- "Ed Ldrshp"
  out_df$dept[hdfs] <- "HDFS"
  out_df$dept[bbh] <- "BBH"
  out_df$dept[neuro] <- "Neuro"
  
  out_df$area <- NA
  out_df$area[io] <- "I/O"
  out_df$area[devel] <- "Developmental"
  out_df$area[soc] <- "Social"
  out_df$area[clin] <- "Clinical"
  out_df$area[cog] <- "Cognitive"
  
  return(out_df)
}

clean_position_field <- function(df) {
  if (is.null(df)) stop('Data frame is NULL.')

  # "Staff and Graduate Student"
  staff <- stringr::str_detect(df$position, "Staff and Graduate Student")
  fac <- stringr::str_detect(df$position, "Faculty")
  grad <- stringr::str_detect(df$position, "Grad")
  post <- stringr::str_detect(df$position, "Post")
  
  out_df <- df
  
  out_df$position[post] <- "Post"
  out_df$position[grad] <- "Grad"
  out_df$position[fac] <- "Fac"
  out_df$position[staff] <- "Staff"
  
  return(out_df)
}

convert_timestamp <- function(df) {
  df$Timestamp <- lubridate::mdy_hms(df$Timestamp)
  return(df)
}

clean_registration_field_names <- function(df) {
  if (is.null(df)) stop('Data frame is NULL.')

  regis_clean <- dplyr::rename(df,
                               time_stamp = `Timestamp`,
                               email = `Email Address`,
                               first = `First Name`,
                               last = `Last Name`,
                               dept = Department,
                               position = `Current position at Penn State`,
                               how_hear = `How did you hear about the bootcamp?`)
  
  return(regis_clean)
}

