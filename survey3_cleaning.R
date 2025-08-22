

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

dat <- read.csv("rawdata/DFI_survey3.csv")
premium <- read.csv("cleaneddata/wtpsummary.csv")

wtpdat <- premium[complete.cases(premium), ]

wtpdat$calcwtp <- (wtpdat$price*wtpdat$value)+wtpdat$price
wtpdat_all <- wtpdat[wtpdat$infotreatment == "all", ]
wtpdat_rice <- wtpdat[wtpdat$crop == "rice", ]
wtpdat_watermelon <- wtpdat[wtpdat$crop == "watermelon", ]

# tables ggplot

r <- ggplot() + geom_col(data= wtpdat_rice, 
                    aes(x = infotreatment, y= calcwtp, fill = region), 
                    position = "dodge") + theme_minimal()

r + geom_hline(yintercept=1.75, linetype="dashed", color = "black")




w <- ggplot() + geom_col(data= wtpdat_watermelon, 
                         aes(x = infotreatment, y= calcwtp, fill = region), 
                         position = "dodge") + theme_minimal()

w + geom_hline(yintercept=1.75, linetype="dashed", color = "black")