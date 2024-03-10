


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(patchwork)

windowsFonts(A = windowsFont("Times New Roman"))

rm(list=ls()) # Caution: this clears the Environment

# regressions

deltafood <- readRDS("cleaned2.rds")
summary(deltafood)

full_reg <- lm(premium ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region), data = deltafood)
summary(full_reg)

okra_reg <- lm(okra ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region) +
                  RP + CP + SP + TP, data = deltafood)
summary(okra_reg)

tomato_reg <- lm(tomato ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region) +
                 RP + CP + SP + OP, data = deltafood)
summary(tomato_reg)

sweetpot_reg <- lm(sweetpot ~ local1 + knowledge + white + Q18_5 + age + farmbiz + income + factor(region) +
                   RP + CP + TP + OP, data = deltafood)
summary(sweetpot_reg)

collard_reg <- lm(collard ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region) +
                     RP + SP + TP + OP, data = deltafood)
summary(collard_reg)

rice_reg <- lm(collard ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region) +
                    CP + SP + TP + OP, data = deltafood)
summary(rice_reg)

rice_reg2 <- lm(collard ~ local1 + knowledge + Q18_5 + age + farmbiz + income + factor(region), data = deltafood)
summary(rice_reg2)







