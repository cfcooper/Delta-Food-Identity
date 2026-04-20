
# --- 1. Load Libraries -------------------------------------------------------

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)


rm(list=ls()) # Caution: this clears the Environment

# --- 2. Load Data ------------------------------------------------------------

df <- readRDS("cleaneddata/df_full.rds")

# Confirm structure
glimpse(df)


# --- 3. Breakdown Groups  ----------------------------

# all is just df

group_delta <- df[df$target_state == 1, ]
group_nondelta <- df[df$target_state == 0, ]
group_bnondelta <- group_nondelta[group_nondelta$race_b == 1, ]
group_wdelta <- group_delta[group_delta$race_b == 0, ]



# --- 3. Tables: Cultural Connection  ----------------------------

culturalconnection <-  select(df, c("ResponseId",""))





