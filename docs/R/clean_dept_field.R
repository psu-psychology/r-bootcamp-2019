clean_dept_field <- function(df) {
  
  # "Teaching & Learning with Technology - AND - Geography/GIS"
  tlt <- stringr::str_detect(df$dept, "Teaching ")
  
  # "Educational Leadership"
  ed_ldr <- stringr::str_detect(df$dept, "Educational Leadership")
  
  # Psychology
  some_psych <- stringr::str_detect(df$dept, "(p|P)(s|a)ych")
  io <- stringr::str_detect(df$dept, "I/O")
  devel <- stringr::str_detect(df$dept, "(D|d)evel")
  soc <- stringr::str_detect(df$dept, "(S|s)ocial")
  clin <- stringr::str_detect(df$dept, "(C|c)lin")
  cog <- stringr::str_detect(df$dept, "(C|c)ogn")
  
  out_df <- df
  out_df$dept[some_psych] <- "Psych"
  out_df$dept[tlt] <- "TLT"
  out_df$dept[ed_ldr] <- "Ed Ldrshp"
  
  out_df$area <- NA
  out_df$area[io] <- "I/O"
  out_df$area[devel] <- "Developmental"
  out_df$area[soc] <- "Social"
  out_df$area[clin] <- "Clinical"
  out_df$area[cog] <- "Cognitive"
  
  return(out_df)
}
