# make_bootcamp_site.R
#
# Updates and makes R bootcamp site

# Update and clean data from registrants and survey
source("R/registrants.R")
source("R/survey.R")
update_registration_data()
update_survey_data()

# Gilmore's talks
rmarkdown::render(input = "talks/bootcamp-day-1-intro.Rmd", 
                  output_format = "ioslides_presentation")

rmarkdown::render(input = "talks/slow-r.Rmd", 
                  output_format = c("html_document"))

rmarkdown::render(input = "talks/r-eproducible-science.Rmd", 
                  output_format = c("html_document"))

# Render site last so that updated versions get copied to docs/
rmarkdown::render_site()
