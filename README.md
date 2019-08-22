# r-bootcamp-2019
Files related to the R bootcamp held in August 2019.

## Contents

- The repository root directory contains `*.yml` `*.Rmd` and `*.html` files used to generate the complete bootcamp website. 
- The `/docs` directory is where the rendered HTML files are copied. GitHub pages renders the project website (https://psu-psychology.github.io/r-bootcamp-2019/) from this folder.

## Rendering the site

- Clone the repository.
- From within the repository directory, run `rmarkdown::render_site()`. 
- To ensure that all of the associated RMarkdown documents get updated, too, run `source("R/make_bootcamp_site.R")` and then run this command from the console `make_bootcamp_site()`.
- You may then view the updated site locally in `docs/`.
