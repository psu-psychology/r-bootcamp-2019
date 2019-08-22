# Functions to handle emails

source("R/registrants.R")

send_test_email <- function() {
  #fac_emails <- get_faculty_emails()
  fac_email_df <- make_fac_email_df()
  fac_email_mime <- make_fac_email_mime(fac_email_df)
  send_mime_emails(fac_email_mime)
}

get_participant_emails <- function() {
  # Get and clean before extracting
  d0 <- get_registration_data()
  d1 <- clean_registration_data(d0)
  
  d2 <- dplyr::mutate(d1, name = paste(first, last), sep=" ")
  
  # Select and drop duplicates
  d3 <- dplyr::select(d2, name, email)
  
  d4 <- dplyr::filter(d3, email %in% unique(email))
  d4
}

get_faculty_emails <- function() {
  library(googledrive)
  library(googlesheets)
  
  drive_auth(use_oob = TRUE)
  options(httr_oob_default = TRUE)
  
  gs <- googlesheets::gs_title("PSU R Bootcamp 2019 Faculty")
  fac_data <- googlesheets::gs_read(ss = gs,
                                         ws = 'Sheet1')
  
  fac_data <- mutate(fac_data, name = paste(first, last, sep = " "))
  fac_data
}

make_email_df <- function(df = data.frame(name = "Rick Gilmore",
                                              email = "thatrickgilmore@gmail.com"), 
                           email_from = "Rick O. Gilmore <rick.o.gilmore@gmail.com>",
                           email_subj = "TEST",
                           email_bcc = "rogilmore@mac.com",
                           email_body = "I love sending email.") {
  
  if (is.null(df)) stop('Data frame is NULL.')
  
  df0 <- dplyr::mutate(df,
    to = sprintf('%s <%s>', name, email),
    from = email_from,
    subject = email_subj,
    bcc = email_bcc,
    body = email_body) %>%
    select(to, from, subject, bcc, body)
  df0
}

make_email_mime <- function(df) {
  if (is.null(df)) stop('Data frame is NULL.')
  purrr::pmap(df, gmailr::mime)
}

send_mime_emails <- function(email_mime) {
  # Only works if local account has Google apps credentials file in root
  gmailr::use_secret_file(list.files(pattern = "\\.json$"))
  
  safe_send_message <- purrr::safely(gmailr::send_message)
  purrr::map(email_mime, safe_send_message)
}

report_email_errors <- function(sent_mails) {
  x0 <- purrr::transpose(sent_mails)
  errors <- purrr::map_lgl(x0$error, Negate(is.null))
  sent_mails[errors]
}
