# Based on https://github.com/jennybc/send-email-with-r

suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)

source("R/registrants.R")

registrant_info <- extract_emails()
addresses <- data.frame(name = "Rick Gilmore", email = "rog1@psu.edu")
my_dat <- addresses

email_id <- "test"
email_subj <- "Welcome to the R Bootcamp 2019"
email_subj <- "TESTING bootcamp email workflow"
email_from <- 'Rick Gilmore <rick.o.gilmore@gmail.com>' # your Gmail address
email_bcc <- paste('Michael Hallquist <michael.hallquist@gmail.com>',
                  'Nilam Ram <nilam.ram@psu.edu>',
                  'Sy-Miin Chow <symiin@psu.edu>', sep=", ")

email_body <- "Hello.

This message is to confirm that you are registered for the R Bootcamp 2019.

The Bootcamp begins next Wednesday morning, August 21, 2019 at 9:00 am in 210 and 211 Keller. Please join us for a continental breakfast beforehand in the Moore Building lobby, beginning at 8:30 am. Note that we will have breakfast at 8:30 am Wednesday through Friday, in addition to lunch on Friday, all in the Moore Building lobby.
We thank the Department of Psychology, the Social, Life, & Engineering Sciences Imaging Center (SLEIC), the Department of Human Development and Family Studies, and the Child Study Center for financial and logistical support of the Bootcamp. 

The Bootcamp website has additional details: 

https://psu-psychology.github.io/r-bootcamp-2019/

Please make ensure that you have completed the activities outlined here:

https://psu-psychology.github.io/r-bootcamp-2019/before-the-bootcamp.html

We're looking forward to seeing you next week.

Rick Gilmore
Professor of Psychology
"

edat <- my_dat %>%
  mutate(
    to = sprintf('%s <%s>', name, email),
    from = email_from,
    subject = email_subj,
    bcc = email_bcc,
    body = email_body) %>%
  select(to, from, subject, optional_bcc, body)
edat

#write_csv(edat, "data/csv/composed-emails.csv")

emails <- edat %>%
  pmap(., mime)

## optional: use if you've created your own client id
use_secret_file(list.files(pattern = "\\.json$"))

safe_send_message <- safely(send_message)
sent_mail <- emails %>%
  map(., safe_send_message)

errors <- sent_mail %>%
  transpose() %>%
  .$error %>%
  map_lgl(Negate(is.null))
sent_mail[errors]
