

library(dplyr)
#library(formattable)
#library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
#library(patchwork)


rm(list=ls()) # Caution: this clears the Environment

delta <- read.csv("rawdata/delta_WTP_redo.csv")

delta$delta <- if_else(delta$state %in% c("Mississippi","Arkansas","Louisiana","Tennessee","Kentucky"), 1, 0)
delta$race_black <- if_else(delta$Q15_2 %in% c("Black or African American"), 1, 0)
delta$race_white <- if_else(delta$Q15_1 %in% c("White"), 1, 0)



summary_blackownfarm <- delta %>% group_by(delta, Q59) %>%
  summarise(count = n())

summary_community <- delta %>% group_by(delta, Q58) %>%
  summarise(count = n())

summary_community2 <- delta %>% group_by(race_black, Q58) %>%
  summarise(count = n())

summary_community3 <- delta %>% group_by(race_white, Q58) %>%
  summarise(count = n())


summary_equity <- delta %>% group_by(race_black, Q87) %>%
  summarise(count = n())

summary_blackownB <- delta %>% group_by(race_black, Q86) %>%
  summarise(count = n())