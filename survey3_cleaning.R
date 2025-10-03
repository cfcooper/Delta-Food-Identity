

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

dat <- read.csv("rawdata/DWTP_3.csv")
#premium <- read.csv("cleaneddata/wtpsummary.csv")

dat <- dat[!is.na(dat$full_launch), ]
names(dat) <- trimws(names(dat))
dat_b <- dat[dat$black != 0, ]
dat_d <- dat[dat$black != 1, ]


dat_b <- select(dat_b, c(1,35:49,79:98))  # select columns
dat_b$melon_price <- 5
dat_b$corn_price <- .33
dat_b$tater_price <- 1
dat_b$mater_price <- 1
dat_b$rice_price <- 1.75


# ****** melon ******
melon_price_b <- select(dat_b, c(1:4,29:30,37))

melon_price_b <- melon_price_b %>%
  pivot_longer(
    cols = c(2:4), # Specify the columns to pivot
    names_to = "question",   # Name of the new column storing original column names
    values_to = "choice"      # Name of the new column storing the values
  )
melon_price_b <- melon_price_b[!is.na(melon_price_b$choice), ]
melon_price_b$price <- 5
melon_price_b$price <- if_else(melon_price_b$question %in% c("melonb_low"), melon_price_b$melon_low, melon_price_b$price)
melon_price_b$price <- if_else(melon_price_b$question %in% c("melonb_high"), melon_price_b$melon_high, melon_price_b$price)
melon_price_b <- select(melon_price_b, c(1,4,6:7))

melon_price_b <- melon_price_b %>%
  rename(p_1 = melon_price)
melon_price_b <- melon_price_b %>%
  rename(p_2 = price)

melon_price_b$av_1 <- 1
melon_price_b$av_2 <- 1



#yes/yes
dat_b$melonyy <- if_else(dat_b$melonb==2 & dat_b$melonb_high==2, 1, 0)
dat_d$melonyy <- if_else(dat_d$melond==2 & dat_d$melond_high==2, 1, 0)


#yes/no
dat_b$melonyn <- if_else(dat_b$melonb==2 & dat_b$melonb_high==1, 1, 0)
dat_d$melonyn <- if_else(dat_d$melond==2 & dat_d$melond_high==1, 1, 0)


#no/yes
dat_b$melonny <- if_else(dat_b$melonb==1 & dat_b$melonb_low==2, 1, 0)
dat_d$melonny <- if_else(dat_d$melond==1 & dat_d$melond_low==2, 1, 0)

#no/no
dat_b$melonnn <- if_else(dat_b$melonb==1 & dat_b$melonb_low==1, 1, 0)
dat_d$melonnn <- if_else(dat_d$melond==1 & dat_d$melond_low==1, 1, 0)





# rice
rice_price_b <- select(dat_b, c(1,11:13,33:34,41))

rice_price_b <- rice_price_b %>%
  pivot_longer(
    cols = c(2:4), # Specify the columns to pivot
    names_to = "question",   # Name of the new column storing original column names
    values_to = "choice"      # Name of the new column storing the values
  )
rice_price_b <- rice_price_b[!is.na(rice_price_b$choice), ]
rice_price_b$price <- 1.75
rice_price_b$price <- if_else(rice_price_b$question %in% c("rice_low"), rice_price_b$rice_low.1, rice_price_b$price)
rice_price_b$price <- if_else(rice_price_b$question %in% c("rice_high"), rice_price_b$rice_high.1, rice_price_b$price)
rice_price_b <- select(rice_price_b, c(1,4,6:7))

rice_price_b <- rice_price_b %>%
  rename(p_1 = rice_price)
rice_price_b <- rice_price_b %>%
  rename(p_2 = price)





# summary info
wtpdat <- premium[complete.cases(premium), ]

wtpdat$calcwtp <- (wtpdat$price*wtpdat$value)+wtpdat$price
wtpdat_all <- wtpdat[wtpdat$infotreatment == "all", ]
wtpdat_rice <- wtpdat[wtpdat$crop == "rice", ]                                                                                                                                                                                                     .
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