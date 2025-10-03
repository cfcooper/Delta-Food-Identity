## new code (translated from STATA)




# Install Icens from Bioconductor (required for DCchoice)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")

# Install DCchoice if needed
if (!require("DCchoice", quietly = TRUE))
  install.packages("DCchoice")



# Load required packages
library(DCchoice)
library(dplyr)


# Clear workspace
rm(list = ls())

# Import data
delta <- read.csv("rawdata/DWTP_3.csv", stringsAsFactors = FALSE)

# Trim whitespace from column names
names(delta) <- trimws(names(delta))

# Trim whitespace from all character columns
delta <- delta %>%
  mutate(across(where(is.character), trimws))

