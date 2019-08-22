require(dplyr)
data("sat.act", package="psych")
sat.act <- sat.act %>% dplyr::rename(sex=gender) %>% 
  mutate(sex=factor(sex, levels=c(1,2), labels=c("male", "female")),
         education=factor(education, levels=c(0,1,2,3,4,5), 
                          labels=c("0", "1", "2", "3", "4", "5")))
