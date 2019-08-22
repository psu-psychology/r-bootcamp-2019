# send_test_email_2_registrants.R
# Sending email to registrants coaches

source("R/email_helpers.R")

e_sub <- "R Bootcamp 2019"
e_body <- "Hello.

If you are receiving this email, you are registered for the R Bootcamp.

We start next Wednesday morning, August 21, 2019, at 8:30 am with a Continental breakfast in the Moore Building lobby.

The Bootcamp itself starts at 9:00 am sharp in rooms 210 and 211 Keller Building.

Please review the Bootcamp website (https://psu-psychology.github.io/r-bootcamp-2019/) for up-to-date information about the event.

We look forward to seeing you next week!

Rick Gilmore
Professor of Psychology

P.S. -- If you have headphones that work with a computer audio jack, you might want to bring them in the event that you want to monitor the other 'parallel' session.
"

particip_emails <- get_participant_emails()

# Send test
email_df <- make_email_df(df = particip_emails,
                          email_subj = e_sub,
                          email_body = e_body,
                          email_bcc = "")
email_mime <- make_email_mime(email_df)
sent_emails <- send_mime_emails(email_mime)
report_email_errors(sent_emails)
