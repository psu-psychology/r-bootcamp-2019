# manage_packages.R
# helper functions to make installing and loading packages easier

bootcamp_pkgs <- c("afex",
                   "apaTables",
                   "backports",
                   "car",
                   "dataMaid",
                   "effects",
                   "emmeans",
                   "ez",
                   "ggfortify",
                   "ggplot2",
                   "googlesheets", 
                   "googledrive",
                   "Hmisc",
                   "interactions",
                   "knitr",
                   "lme4",
                   "lmerTest",
                   "jtools",
                   "plyr",
                   "psych",
                   "rcompanion",
                   "swirl",
                   "tidyverse",
                   "tufte")

install_bootcamp_pkgs <- function(pkgs = bootcamp_pkgs) {
  install_packages(bootcamp_pkgs)
}

install_pacman <- function() {
  if (!require(pacman)) {
    message("Installing 'pacman'")
    install.packages("pacman")
  }
  library(pacman)
}

install_install.load <- function() {
  if (!require(install.load)) {
    message("Installing 'install.load' package from CRAN")
    install.packages("install.load")
  }
  library(install.load)
}

install_packages <- function(pkg_list = c("tidyverse", "ggplot2")) {
  install_install.load()
  if (!is.null(pkg_list)) {
    results <- lapply(pkg_list, install_load)
    } else {
      stop("'pkg_list' is NULL.")
    }
}
