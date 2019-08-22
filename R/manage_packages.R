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
                   "ggmosaic",
                   "googlesheets",
                   "googledrive",
                   "Hmisc",
                   "interactions",
                   "knitr",
                   "lavaan",
                   "lme4",
                   "lmerTest",
                   "jtools",
                   "plyr",
                   "psych",
                   "rcompanion",
                   "Rtools",
                   "skimr",
                   "swirl",
                   "tidyverse",
                   "tufte",
                   "vcd")

install_bootcamp_pkgs <- function(pkgs = bootcamp_pkgs, install_message = TRUE,
                                  force_reinstall = FALSE) {
  install_pkgs(bootcamp_pkgs, install_message, force_reinstall)
}

test_bootcamp_pkg_install <- function() {
  if (!is.null(install_pkgs(c("ggplot2", "dplyr")))) {
    message("Test fails.")
  } else {
    message("Test passes.")
  }
}

install_pkg <- function(pkg, install_message = FALSE, force_reinstall = FALSE) {
  if (force_reinstall) {
    if (install_message) {
      message(paste0("Reinstalling package: '", pkg, "'\n"))
    }
    install.packages(pkg, quiet = TRUE, verbose = FALSE)
  } else {
    if(require(pkg, character.only = TRUE, quietly = TRUE)) {
      if (install_message) {
        message(paste0("Package '", pkg, "': already installed.\n"))
      }
    } else {
      if (install_message) {
        message(paste0("Installing package: '", pkg, "'\n"))
      }
      install.packages(pkg, quiet = TRUE, verbose = FALSE)
    }
  }
}

install_pkgs <- function(pkgs, install_message = FALSE, force_reinstall = FALSE) {
  unlist(lapply(pkgs, install_pkg, install_message, force_reinstall))
}
