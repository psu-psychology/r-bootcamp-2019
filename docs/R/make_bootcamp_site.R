# make_bootcamp_site.R
#
# Updates and makes R bootcamp site

# Install and/or load packages
source("R/manage_packages.R")
install_bootcamp_pkgs() # Not essential, but probably good practice

# Copy *.Rmd to Box (ROG only)
source("R/copy_rmd_2_box.R")
copy_rmd_2_box()

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

# Albohn et al.
rmarkdown::render(input = "talks/data_analyses.Rmd", 
                  output_format = c("html_document"))

rmarkdown::render(input = "talks/data_analyses_practice.Rmd", 
                  output_format = c("html_document"))

# Ram & Brinberg
# Didn't render 2019-08-21-05:00. No time to debug.
# rmarkdown::render(input = "talks/RBootcamp_MLMInteractions_2019_0820_Final2.Rmd", 
#                   output_format = c("html_document"))

# Render site last so that updated versions get copied to docs/
rmarkdown::render_site()
