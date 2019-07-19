get_registrant_data <- function(verbose = FALSE, sheet_name = 'PSU R Bootcamp 2019 Sign-up (Responses)') {
  
  require(googledrive)
  drive_auth(use_oob = TRUE)
  require(googlesheets) 
  options(httr_oob_default = TRUE)
  
  bootcamp_gs <- googlesheets::gs_title(sheet_name)
  bootcamp_data <- googlesheets::gs_read(ss = bootcamp_gs,
                                         ws = 'Form Responses 1')
  bootcamp_data
}
