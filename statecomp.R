
##state comparisons


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(patchwork)

windowsFonts(A = windowsFont("Times New Roman"))

rm(list=ls()) # Caution: this clears the Environment

delta <- read.csv("cleaneddata/deltacurrent.csv")


delta <- select(delta, c("Q2","Q11"))


delta <- delta %>% group_by(Q2,Q11) %>% summarise(count = n())


delta <- delta[!delta$count == 1,]
delta <- delta[!delta$count == 2,]
delta <- delta[!delta$Q11 == "none",]
delta <- delta[!delta$Q11 == "cotton",]

delta_test <- delta %>%
  group_by(Q2) %>%
  arrange(desc(count)) %>%
  slice_head(n = 5)

#delta_test <- delta_test[!delta_test$count == "NA",]


ar_test <- delta_test[delta_test$Q2 == "arkansas",]
ar_test$Q11 <- factor(ar_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans"))


mo_test <- delta_test[delta_test$Q2 == "missouri",]
mo_test$Q11 <- factor(mo_test$Q11, levels=c("corn","rice","seafood","soybeans","wheat"))


la_test <- delta_test[delta_test$Q2 == "louisiana",]
la_test$Q11 <- factor(la_test$Q11, levels=c("corn","rice","seafood","soybeans","sugarcane"))



tn_test <- delta_test[delta_test$Q2 == "tennessee",]
tn_test$Q11 <- factor(tn_test$Q11, levels=c("corn","rice","seafood","soybeans","potatoes"))


ms_test <- delta_test[delta_test$Q2 == "mississippi",]
ms_test$Q11 <- factor(ms_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans"))




ar <- ggplot() + geom_col(data= ar_test, aes(x = Q11, y= count))
mo <- ggplot() + geom_col(data= mo_test, aes(x = Q11, y= count))
la <- ggplot() + geom_col(data= la_test, aes(x = Q11, y= count))
tn <- ggplot() + geom_col(data= tn_test, aes(x = Q11, y= count))
ms <- ggplot() + geom_col(data= ms_test, aes(x = Q11, y= count))


delt <- ar/la/mo/ms/tn + plot_layout(ncol = 1)
delt

##top 5 overall-----------------------------------------------------------------------------

rm(list=ls()) # Caution: this clears the Environment

delta <- read.csv("deltacurrent.csv")


delta <- select(delta, c("Q2","Q11"))
delta <- delta[!delta$Q11 == "none",]
delta <- delta[!delta$Q11 == "cotton",]

deltatop5 <- delta %>% group_by(Q11) %>% summarise(count = n())
deltatop5 <- deltatop5 %>%
  arrange(desc(count)) %>%
  slice_head(n = 7)

delta <- delta %>% group_by(Q2,Q11) %>% summarise(count = n()) %>% ungroup()

delta$keep <- "no"
delta$keep <- if_else(delta$Q11 %in% c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"), "yes", "no")
delta_test <- delta[!delta$keep == "no",]
delta_test <- select(delta_test, c("Q2","Q11","count"))
delta_test <- delta_test %>% add_row(Q2 = "arkansas", Q11 = "sugarcane", count = 0)
delta_test <- delta_test %>% add_row(Q2 = "mississippi", Q11 = "sugarcane", count = 0)



ar_test <- delta_test[delta_test$Q2 == "arkansas",]
ar_test$total <- sum(ar_test$count)
ar_test$prop <- (ar_test$count/ar_test$total)*100
ar_test$Q11 <- factor(ar_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"))


mo_test <- delta_test[delta_test$Q2 == "missouri",]
mo_test$total <- sum(mo_test$count)
mo_test$prop <- (mo_test$count/mo_test$total)*100
mo_test$Q11 <- factor(mo_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"))


la_test <- delta_test[delta_test$Q2 == "louisiana",]
la_test$total <- sum(la_test$count)
la_test$prop <- (la_test$count/la_test$total)*100
la_test$Q11 <- factor(la_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"))



tn_test <- delta_test[delta_test$Q2 == "tennessee",]
tn_test$total <- sum(tn_test$count)
tn_test$prop <- (tn_test$count/tn_test$total)*100
tn_test$Q11 <- factor(tn_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"))


ms_test <- delta_test[delta_test$Q2 == "mississippi",]
ms_test$total <- sum(ms_test$count)
ms_test$prop <- (ms_test$count/ms_test$total)*100
ms_test$Q11 <- factor(ms_test$Q11, levels=c("chicken","corn","rice","seafood","soybeans","sugarcane","wheat"))

ar <- ggplot() + geom_col(data= ar_test, aes(x = Q11, y= prop)) + theme_minimal(base_family = "A") + 
  labs( x=" ", y=" ",  title = "Arkansas") + coord_cartesian(ylim = c(0, 40))
mo <- ggplot() + geom_col(data= mo_test, aes(x = Q11, y= prop)) + theme_minimal(base_family = "A") + 
  labs( x=" ", y=" ",  title = "Missouri") + coord_cartesian(ylim = c(0, 40))
la <- ggplot() + geom_col(data= la_test, aes(x = Q11, y= prop)) + theme_minimal(base_family = "A") + 
  labs( x=" ", y=" ",  title = "Louisiana") + coord_cartesian(ylim = c(0, 40))
tn <- ggplot() + geom_col(data= tn_test, aes(x = Q11, y= prop)) + theme_minimal(base_family = "A") + 
  labs( x=" ", y=" ",  title = "Tennessee") + coord_cartesian(ylim = c(0, 40))
ms <- ggplot() + geom_col(data= ms_test, aes(x = Q11, y= prop)) + theme_minimal(base_family = "A") + 
  labs( x=" ", y=" ",  title = "Mississippi") + coord_cartesian(ylim = c(0, 40))


delt <- ar/la/ms/tn + plot_layout(ncol = 2)
delt

ar

