# send_test_email_to_fac.R
# Sending email to faculty/grad coaches

source("R/email_helpers.R")

e_sub <- "Test of R Bootcamp email system"
e_body <- "Hi, all.

I'm testing an R-based workflow to send emails.

This message is going just to the faculty and grad coaches.

If you get the message, please reply.

Thanks.

Rick
"

fac_emails <- get_faculty_emails()
fac_email_df <- make_fac_email_df(df = fac_emails,
                                  email_subj = e_sub,
                                  email_body = e_body)
fac_email_mime <- make_fac_email_mime(fac_email_df)
send_mime_emails(fac_email_mime)
