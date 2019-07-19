clean_position_field <- function(df) {
  # "Staff and Graduate Student"
  staff <- stringr::str_detect(df$position, "Staff and Graduate Student")
  fac <- stringr::str_detect(df$position, "Faculty")
  grad <- stringr::str_detect(df$position, "Grad")
  post <- stringr::str_detect(df$position, "Post")
  
  out_df <- df
  
  out_df$position[post] <- "Post"
  out_df$position[grad] <- "Grad"
  out_df$position[fac] <- "Fac"
  out_df$position[staff] <- "Staff"
  return(out_df)
}
