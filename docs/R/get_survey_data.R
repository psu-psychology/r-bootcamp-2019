get_survey_data <- function(verbose = FALSE, 
                            sheet_name = 'PSU R Bootcamp 2019 Survey (Responses)') {
  # Download 2019 Bootcamp registration data from GoogleSheet
  library(googledrive)
  library(googlesheets)
  
  drive_auth(use_oob = TRUE)
  options(httr_oob_default = TRUE)
  
  survey_gs <- googlesheets::gs_title(sheet_name)
  survey_data <- googlesheets::gs_read(ss = survey_gs,
                                         ws = 'Form Responses 1')
  survey_data
}
