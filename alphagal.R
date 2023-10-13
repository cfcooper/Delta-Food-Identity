


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)

rm(list=ls()) # Caution: this clears the Environment

windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

deltafood <- read.csv("DFI.TEST.csv")

deltafood <- deltafood[!deltafood$Q2 == "My state is not listed",]
deltafood <- deltafood[!deltafood$Q2 == "I do not reside in the United States",]
deltafood$ID <- 1:nrow(deltafood)

summaryalphagal <- deltafood %>% group_by(Q15) %>%
  summarise(count = n())

summaryalphagal2 <- deltafood %>% group_by(Q17) %>%
  summarise(count = n())