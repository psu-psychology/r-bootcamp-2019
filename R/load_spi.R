require(dplyr)
data("spi", package="psych")
spi <- spi %>% 
  renmame(exercise = exer) %>%
  mutate(sex=factor(sex, levels=c(1,2), labels=c("male", "female")),
         wellness=factor(wellness, levels=c(1,2), labels=c("Low", "High")),
         exercise=factor(exercise, levels=c(1,2,3), labels=c("Rarely", "Sometimes", "Often")),
         wellness=factor(wellness, levels=c(1,2), labels=c("Low", "High")),
         ER=factor(ER, levels=c(1,2,3,4), labels=c("None", "1x", "2x", "3 or more")))
