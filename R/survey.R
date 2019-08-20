# survey.R
#
# This set of helper functions gathers the 2019 R Bootcamp User Survey and cleans it.
# 
# We gather these functions togther to make it easier to source the relevant ones depending
# on the context.
#
# author: Rick Gilmore, rog1@psu.edu

get_survey_data <- function(verbose = FALSE, 
                            sheet_name = 'PSU R Bootcamp 2019 Survey (Responses)',
                            save_raw = TRUE) {
  # Download 2019 Bootcamp registration data from GoogleSheet
  library(googledrive)
  library(googlesheets)
  
  drive_auth(use_oob = TRUE)
  options(httr_oob_default = TRUE)
  
  survey_gs <- googlesheets::gs_title(sheet_name)
  survey_data <- googlesheets::gs_read(ss = survey_gs,
                                       ws = 'Form Responses 1')
  
  if (save_raw) {
    readr::write_csv(survey_data, "data/csv/survey_raw.csv")
  }
  survey_data
}

clean_survey_data <- function(df) {
  # Clean the 2019 R Bootcamp Survey Data
  
  df_0 <- clean_timestamp(df)
  df_1 <- clean_other_languages(df_0)
  df_2 <- clean_hrs_sleep(df_1)
  df_3 <- clean_survey_names(df_2)
  df_4 <- gather_known_langs(df_3)
  df_5 <- clean_repro_crisis(df_4)
  
  return(df_5)
}

save_survey_data <- function(df, fn = "data/csv/survey.csv", verbose = FALSE) {
  write.csv(df, fn, row.names = FALSE)
  if (verbose) message(paste0("Survey data saved to ", fn))
}

update_survey_data <- function() {
  save_survey_data(
    clean_survey_data(
      get_survey_data()
      )
  )
}

clean_repro_crisis <- function(df) {
   df$crisis <- factor(df$crisis,
                       levels = c("Yes, a significant crisis", 
                                     "Yes, a slight crisis", 
                                     "No crisis", "Don't know"),
                       labels = c("Significant", "Slight",
                                  "No crisis", "Don't know"))
  return(df)
}

clean_other_languages <- function(df) {
  # Clean the 'Other programming languages you know' field
  out_df <- df
  
  # Create Booleans for different languages/language categories
  python <- stringr::str_detect(df$`Other programming languages you know`, "(P|p)ython")
  spss_sas <- stringr::str_detect(df$`Other programming languages you know`, "SPSS/SAS")
  mplus <- stringr::str_detect(df$`Other programming languages you know`, "(M|m)plus")
  lisrel <- stringr::str_detect(df$`Other programming languages you know`, "(L|l)isrel")
  none <- stringr::str_detect(df$`Other programming languages you know`, "None")
  js_html_css <- stringr::str_detect(df$`Other programming languages you know`, "HTML")
  java <- stringr::str_detect(df$`Other programming languages you know`, "Java")
  unix <- stringr::str_detect(df$`Other programming languages you know`, "nix")
  swift <- stringr::str_detect(df$`Other programming languages you know`, "Swift")
  msdos <- stringr::str_detect(df$`Other programming languages you know`, "MS DOS")
  
  # Create new fields for each language; easier to gather separately
  out_df$python <- NA
  out_df$python[python == TRUE] <- "python"
  
  out_df$spss_sas <- NA
  out_df$spss_sas[spss_sas == TRUE] <- "spss_sas"
  
  out_df$mplus <- NA
  out_df$mplus[mplus == TRUE] <- "mplus"
  
  out_df$lisrel <- NA
  out_df$lisrel[lisrel == TRUE] <- "lisrel"
  
  out_df$none <- NA
  out_df$none[none == TRUE] <- "none"
  
  out_df$js_html_css <- NA
  out_df$js_html_css[js_html_css == TRUE] <- "js_html_css"
  
  out_df$java <- NA
  out_df$java[java == TRUE] <- "java"
  
  out_df$unix <- NA
  out_df$unix[unix == TRUE] <- "unix"
  
  out_df$swift <- NA
  out_df$swift[swift == TRUE] <- "swift"
  
  out_df$msdos <- NA
  out_df$msdos[msdos == TRUE] <- "msdos"
  
  return(out_df)
}

clean_hrs_sleep <- function(df) {
  # `Preferred number of hours spent sleeping/day`
  
  # "7-8"
  clean_this <- df$`Preferred number of hours spent sleeping/day` == "7-8"
  df$`Preferred number of hours spent sleeping/day`[clean_this] <- "7.5"

  # 8-9
  clean_this_too <- df$`Preferred number of hours spent sleeping/day` == "8-9"
  df$`Preferred number of hours spent sleeping/day`[clean_this_too] <- "8.5"

  df$`Preferred number of hours spent sleeping/day` <- as.numeric(df$`Preferred number of hours spent sleeping/day`)
  
  return(df)
}

clean_survey_names <- function(df) {
  # Create shorter names for variables
  df <- dplyr::rename(df, time_stamp = Timestamp)
  df <- dplyr::rename(df, r_exp = `Your current level of experience/expertise with R`)
  df <- dplyr::rename(df, other_langs = `Other programming languages you know`)
  df <- dplyr::rename(df, beverage = `Your favorite beverage`)
  df <- dplyr::rename(df, age_yrs = `Age in years`)
  df <- dplyr::rename(df, sleep_hrs = `Preferred number of hours spent sleeping/day`)
  df <- dplyr::rename(df, got_s8 =  `Your enthusiasm for \"Game of Thrones\" Season 8.`)
  df <- dplyr::rename(df, day = `Favorite day of the week`)
  df <- dplyr::rename(df, tidy_data = `Are your data tidy?`)
  df <- dplyr::rename(df, crisis = `Is there a reproducibility crisis?`)
  
  return(df) 
}

clean_timestamp <- function(df) {
  df$Timestamp <- lubridate::mdy_hms(df$Timestamp)
  return(df)
}

gather_known_langs <- function(df) {
  # Create tidy data tibble when there are multiple languages known
  df1 <- dplyr::select(df, time_stamp, python:msdos)
  df2 <- tidyr::gather(df1, "lang", "lang_known", -time_stamp)
  df3 <- dplyr::filter(df2, !is.na(lang_known))
  df4 <- dplyr::select(df3, -lang)
  df5 <- dplyr::left_join(df, df4, by = 'time_stamp')
  df6 <- dplyr::select(df5, -other_langs, -(python:msdos))
  return(df6)
}

test_clean_survey <- function() {
  source("R/get_survey_data.R")
  
  survey <- get_survey_data()
  if (is.null(survey)) {
    stop("Error in importing survey data")
  } else {
    message("Survey data imported.")
    s0 <- clean_timestamp(survey)
    if (is.null(s0)) {
      stop("Error in cleaning Timestamp field.")
    } else {
      message("Timestamp field cleaned.")
      s1 <- clean_other_languages(s0)
      if (is.null(s1)) {
        stop("Error in cleaning other languages field.")
      } else {
        message("Other languages field cleaned.")
        s2 <- clean_hrs_sleep(s1)
        if (is.null(s2)) {
          stop("Error in cleaning sleep hours field.")
        } else {
          message("Sleep hours field cleaned.")
          s3 <- clean_survey_names(s2)
          if (is.null(s3)) {
            stop("Error in cleaning survey names.")
          } else {
            message("Survey field names simplified.")
            s4 <- gather_known_langs(s3)
            if (is.null(s4)) {
              stop("Error in gathering known languages.")
            } else {
              message("Known languages field tidy.")
              s5 <- clean_repro_crisis(s4)
              if (is.na(s5)) {
                stop("Error in cleaning reproducibility crisis field")
              } else {
                message("Reproducibility crisis field cleaned.")
                return(s5)
              }
            }
          }
        }
      }
    }
  }
}
