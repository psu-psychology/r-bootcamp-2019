require(dplyr)
data("sat.act", package="psych")
sat.act <- sat.act %>% dplyr::rename(sex=gender) %>% 
  mutate(sex=factor(sex, levels=c(1,2), labels=c("male", "female")))
