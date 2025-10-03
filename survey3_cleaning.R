

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

dat <- read.csv("rawdata/DWTP_3.csv")
#premium <- read.csv("cleaneddata/wtpsummary.csv")

column_lookup <- data.frame(    # column lookup
  column_number = 1:ncol(dat),
  column_name = names(dat)
)

dat <- dat[!is.na(dat$full_launch), ]
names(dat) <- trimws(names(dat))
dat_b <- dat[dat$black != 0, ]
dat_d <- dat[dat$black != 1, ]



dat_b <- select(dat_b, c(1,35:49,89:98))  # select columns
dat_b$melon_price <- 5
dat_b$corn_price <- .33
dat_b$tater_price <- 1
dat_b$mater_price <- 1
dat_b$rice_price <- 1.75

column_lookup <- data.frame(    # column lookup
  column_number = 1:ncol(dat_d),
  column_name = names(dat_d)
)


dat_d <- select(dat_d, c(1,16:30,89:98))  # select columns
dat_d$melon_price <- 5
dat_d$corn_price <- .33
dat_d$tater_price <- 1
dat_d$mater_price <- 1
dat_d$rice_price <- 1.75

## MELON ----------------------

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


#create choice variables
dat_b$melonc1 <- if_else(dat_b$melonyn==1 | dat_b$melonyy==1, 1, 0)
dat_b$melonc2 <- if_else(dat_b$melonny==1 | dat_b$melonyy==1, 1, 0)

dat_d$melonc1 <- if_else(dat_d$melonyn==1 | dat_d$melonyy==1, 1, 0)
dat_d$melonc2 <- if_else(dat_d$melonny==1 | dat_d$melonyy==1, 1, 0)

#create premiums
dat_b$melonpremvar1 <- 0
dat_d$melonpremvar1 <- 0

dat_b$melonpremvar2 <- if_else(dat_b$melonyn==1 | dat_b$melonyy==1, 
                               ((dat_b$melon_high - dat_b$melon_price)/dat_b$melon_price), 0)

dat_d$melonpremvar2 <- if_else(dat_d$melonyn==1 | dat_d$melonyy==1, 
                               ((dat_d$melon_high - dat_d$melon_price)/dat_d$melon_price), 0)
dat_b$melonpc1 <- dat_b$melonpremvar1
dat_b$melonpc2 <- dat_b$melonpremvar2
dat_d$melonpc1 <- dat_b$melonpremvar1
dat_d$melonpc2 <- dat_b$melonpremvar2

#set prices
dat_b$melonp1 <- dat_b$melon_price
dat_d$melonp1 <- dat_d$melon_price


dat_b$melonp2 <- if_else(dat_b$melonyy==1 | dat_b$melonyn==1,
                         dat_b$melon_high, 0)
dat_b$melonp2 <- if_else(dat_b$melonnn==1 | dat_b$melonny==1,
                         dat_b$melon_low, dat_b$melonp2)

dat_d$melonp2 <- if_else(dat_d$melonyy==1 | dat_d$melonyn==1,
                         dat_d$melon_high, 0)
dat_d$melonp2 <- if_else(dat_d$melonnn==1 | dat_d$melonny==1,
                         dat_d$melon_low, dat_d$melonp2)


## rice ----------------------
#yes/yes
dat_b$riceyy <- if_else(dat_b$riceb==2 & dat_b$riceb_high==2, 1, 0)
dat_d$riceyy <- if_else(dat_d$riced==2 & dat_d$riced_high==2, 1, 0)

#yes/no
dat_b$riceyn <- if_else(dat_b$riceb==2 & dat_b$riceb_high==1, 1, 0)
dat_d$riceyn <- if_else(dat_d$riced==2 & dat_d$riced_high==1, 1, 0)

#no/yes
dat_b$riceny <- if_else(dat_b$riceb==1 & dat_b$riceb_low==2, 1, 0)
dat_d$riceny <- if_else(dat_d$riced==1 & dat_d$riced_low==2, 1, 0)

#no/no
dat_b$ricenn <- if_else(dat_b$riceb==1 & dat_b$riceb_low==1, 1, 0)
dat_d$ricenn <- if_else(dat_d$riced==1 & dat_d$riced_low==1, 1, 0)

#create choice variables
dat_b$ricec1 <- if_else(dat_b$riceyn==1 | dat_b$riceyy==1, 1, 0)
dat_b$ricec2 <- if_else(dat_b$riceny==1 | dat_b$riceyy==1, 1, 0)

dat_d$ricec1 <- if_else(dat_d$riceyn==1 | dat_d$riceyy==1, 1, 0)
dat_d$ricec2 <- if_else(dat_d$riceny==1 | dat_d$riceyy==1, 1, 0)


#create premiums
dat_b$ricepremvar1 <- 0
dat_d$ricepremvar1 <- 0

dat_b$ricepremvar2 <- if_else(dat_b$riceyn==1 | dat_b$riceyy==1, 
                               ((dat_b$rice_high - dat_b$rice_price)/dat_b$rice_price), 0)

dat_d$ricepremvar2 <- if_else(dat_d$riceyn==1 | dat_d$riceyy==1, 
                               ((dat_d$rice_high - dat_d$rice_price)/dat_d$rice_price), 0)
dat_b$ricepc1 <- dat_b$ricepremvar1
dat_b$ricepc2 <- dat_b$ricepremvar2
dat_d$ricepc1 <- dat_b$ricepremvar1
dat_d$ricepc2 <- dat_b$ricepremvar2

#set prices
dat_b$ricep1 <- dat_b$rice_price
dat_d$ricep1 <- dat_d$rice_price


dat_b$ricep2 <- if_else(dat_b$riceyy==1 | dat_b$riceyn==1,
                         dat_b$rice_high, 0)
dat_b$ricep2 <- if_else(dat_b$ricenn==1 | dat_b$riceny==1,
                         dat_b$rice_low, dat_b$ricep2)

dat_d$ricep2 <- if_else(dat_d$riceyy==1 | dat_d$riceyn==1,
                         dat_d$rice_high, 0)
dat_d$ricep2 <- if_else(dat_d$ricenn==1 | dat_d$riceny==1,
                         dat_d$rice_low, dat_d$ricep2)

## tater ----------------------
#yes/yes
dat_b$tateryy <- if_else(dat_b$taterb==2 & dat_b$taterb_high==2, 1, 0)
dat_d$tateryy <- if_else(dat_d$taterd==2 & dat_d$taterd_high==2, 1, 0)

#yes/no
dat_b$tateryn <- if_else(dat_b$taterb==2 & dat_b$taterb_high==1, 1, 0)
dat_d$tateryn <- if_else(dat_d$taterd==2 & dat_d$taterd_high==1, 1, 0)

#no/yes
dat_b$taterny <- if_else(dat_b$taterb==1 & dat_b$taterb_low==2, 1, 0)
dat_d$taterny <- if_else(dat_d$taterd==1 & dat_d$taterd_low==2, 1, 0)

#no/no
dat_b$taternn <- if_else(dat_b$taterb==1 & dat_b$taterb_low==1, 1, 0)
dat_d$taternn <- if_else(dat_d$taterd==1 & dat_d$taterd_low==1, 1, 0)

#create choice variables
dat_b$taterc1 <- if_else(dat_b$tateryn==1 | dat_b$tateryy==1, 1, 0)
dat_b$taterc2 <- if_else(dat_b$taterny==1 | dat_b$tateryy==1, 1, 0)

dat_d$taterc1 <- if_else(dat_d$tateryn==1 | dat_d$tateryy==1, 1, 0)
dat_d$taterc2 <- if_else(dat_d$taterny==1 | dat_d$tateryy==1, 1, 0)

#create premiums
dat_b$taterpremvar1 <- 0
dat_d$taterpremvar1 <- 0

dat_b$taterpremvar2 <- if_else(dat_b$tateryn==1 | dat_b$tateryy==1, 
                               ((dat_b$tater_high - dat_b$tater_price)/dat_b$tater_price), 0)

dat_d$taterpremvar2 <- if_else(dat_d$tateryn==1 | dat_d$tateryy==1, 
                               ((dat_d$tater_high - dat_d$tater_price)/dat_d$tater_price), 0)
dat_b$taterpc1 <- dat_b$taterpremvar1
dat_b$taterpc2 <- dat_b$taterpremvar2
dat_d$taterpc1 <- dat_b$taterpremvar1
dat_d$taterpc2 <- dat_b$taterpremvar2

#set prices
dat_b$taterp1 <- dat_b$tater_price
dat_d$taterp1 <- dat_d$tater_price


dat_b$taterp2 <- if_else(dat_b$tateryy==1 | dat_b$tateryn==1,
                         dat_b$tater_high, 0)
dat_b$taterp2 <- if_else(dat_b$taternn==1 | dat_b$taterny==1,
                         dat_b$tater_low, dat_b$taterp2)

dat_d$taterp2 <- if_else(dat_d$tateryy==1 | dat_d$tateryn==1,
                         dat_d$tater_high, 0)
dat_d$taterp2 <- if_else(dat_d$taternn==1 | dat_d$taterny==1,
                         dat_d$tater_low, dat_d$taterp2)

## corn ----------------------
#yes/yes
dat_b$cornyy <- if_else(dat_b$cornb==2 & dat_b$cornb_high==2, 1, 0)
dat_d$cornyy <- if_else(dat_d$cornd==2 & dat_d$cornd_high==2, 1, 0)

#yes/no
dat_b$cornyn <- if_else(dat_b$cornb==2 & dat_b$cornb_high==1, 1, 0)
dat_d$cornyn <- if_else(dat_d$cornd==2 & dat_d$cornd_high==1, 1, 0)

#no/yes
dat_b$cornny <- if_else(dat_b$cornb==1 & dat_b$cornb_low==2, 1, 0)
dat_d$cornny <- if_else(dat_d$cornd==1 & dat_d$cornd_low==2, 1, 0)

#no/no
dat_b$cornnn <- if_else(dat_b$cornb==1 & dat_b$cornb_low==1, 1, 0)
dat_d$cornnn <- if_else(dat_d$cornd==1 & dat_d$cornd_low==1, 1, 0)


#create choice variables
dat_b$cornc1 <- if_else(dat_b$cornyn==1 | dat_b$cornyy==1, 1, 0)
dat_b$cornc2 <- if_else(dat_b$cornny==1 | dat_b$cornyy==1, 1, 0)

dat_d$cornc1 <- if_else(dat_d$cornyn==1 | dat_d$cornyy==1, 1, 0)
dat_d$cornc2 <- if_else(dat_d$cornny==1 | dat_d$cornyy==1, 1, 0)


#create premiums
dat_b$cornpremvar1 <- 0
dat_d$cornpremvar1 <- 0

dat_b$cornpremvar2 <- if_else(dat_b$cornyn==1 | dat_b$cornyy==1, 
                               ((dat_b$corn_high - dat_b$corn_price)/dat_b$corn_price), 0)

dat_d$cornpremvar2 <- if_else(dat_d$cornyn==1 | dat_d$cornyy==1, 
                               ((dat_d$corn_high - dat_d$corn_price)/dat_d$corn_price), 0)
dat_b$cornpc1 <- dat_b$cornpremvar1
dat_b$cornpc2 <- dat_b$cornpremvar2
dat_d$cornpc1 <- dat_b$cornpremvar1
dat_d$cornpc2 <- dat_b$cornpremvar2

#set prices
dat_b$cornp1 <- dat_b$corn_price
dat_d$cornp1 <- dat_d$corn_price


dat_b$cornp2 <- if_else(dat_b$cornyy==1 | dat_b$cornyn==1,
                         dat_b$corn_high, 0)
dat_b$cornp2 <- if_else(dat_b$cornnn==1 | dat_b$cornny==1,
                         dat_b$corn_low, dat_b$cornp2)

dat_d$cornp2 <- if_else(dat_d$cornyy==1 | dat_d$cornyn==1,
                         dat_d$corn_high, 0)
dat_d$cornp2 <- if_else(dat_d$cornnn==1 | dat_d$cornny==1,
                         dat_d$corn_low, dat_d$cornp2)

## mater ----------------------
#yes/yes
dat_b$materyy <- if_else(dat_b$materb==2 & dat_b$materb_high==2, 1, 0)
dat_d$materyy <- if_else(dat_d$materd==2 & dat_d$materd_high==2, 1, 0)

#yes/no
dat_b$materyn <- if_else(dat_b$materb==2 & dat_b$materb_high==1, 1, 0)
dat_d$materyn <- if_else(dat_d$materd==2 & dat_d$materd_high==1, 1, 0)

#no/yes
dat_b$materny <- if_else(dat_b$materb==1 & dat_b$materb_low==2, 1, 0)
dat_d$materny <- if_else(dat_d$materd==1 & dat_d$materd_low==2, 1, 0)

#no/no
dat_b$maternn <- if_else(dat_b$materb==1 & dat_b$materb_low==1, 1, 0)
dat_d$maternn <- if_else(dat_d$materd==1 & dat_d$materd_low==1, 1, 0)


#create choice variables
dat_b$materc1 <- if_else(dat_b$materyn==1 | dat_b$materyy==1, 1, 0)
dat_b$materc2 <- if_else(dat_b$materny==1 | dat_b$materyy==1, 1, 0)

dat_d$materc1 <- if_else(dat_d$materyn==1 | dat_d$materyy==1, 1, 0)
dat_d$materc2 <- if_else(dat_d$materny==1 | dat_d$materyy==1, 1, 0)


#create premiums
dat_b$materpremvar1 <- 0
dat_d$materpremvar1 <- 0

dat_b$materpremvar2 <- if_else(dat_b$materyn==1 | dat_b$materyy==1, 
                               ((dat_b$mater_high - dat_b$mater_price)/dat_b$mater_price), 0)

dat_d$materpremvar2 <- if_else(dat_d$materyn==1 | dat_d$materyy==1, 
                               ((dat_d$mater_high - dat_d$mater_price)/dat_d$mater_price), 0)
dat_b$materpc1 <- dat_b$materpremvar1
dat_b$materpc2 <- dat_b$materpremvar2
dat_d$materpc1 <- dat_b$materpremvar1
dat_d$materpc2 <- dat_b$materpremvar2

#set prices
dat_b$materp1 <- dat_b$mater_price
dat_d$materp1 <- dat_d$mater_price


dat_b$materp2 <- if_else(dat_b$materyy==1 | dat_b$materyn==1,
                         dat_b$mater_high, 0)
dat_b$materp2 <- if_else(dat_b$maternn==1 | dat_b$materny==1,
                         dat_b$mater_low, dat_b$materp2)

dat_d$materp2 <- if_else(dat_d$materyy==1 | dat_d$materyn==1,
                         dat_d$mater_high, 0)
dat_d$materp2 <- if_else(dat_d$maternn==1 | dat_d$materny==1,
                         dat_d$mater_low, dat_d$materp2)

# save all dataframes for running models ----------------



dat_d$info_treatment <- "delta"
dat_b$info_treatment <- "black"


column_lookup <- data.frame(    # column lookup
  column_number = 1:ncol(dat_d),
  column_name = names(dat_d)
)

dat_d <- dat_d[c(1,17:92)]
dat_b <- dat_b[c(1,17:92)]

full_dat <- rbind(dat_d,dat_b)
saveRDS(dat_d, "cleaneddata/df_delta.rds")
saveRDS(dat_b, "cleaneddata/df_black.rds")
saveRDS(full_dat, "cleaneddata/df_full.rds")



