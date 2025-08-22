

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)


rm(list=ls()) # Caution: this clears the Environment

dat <- read.csv("rawdata/DFI_survey3.csv")
