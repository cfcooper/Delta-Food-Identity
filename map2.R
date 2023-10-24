


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(zipcodeR)
library(sf)

rm(list=ls()) # Caution: this clears the Environment

windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

deltafood <- readRDS("cleandata.RDS")
deltafood <- deltafood[!is.na(deltafood$LocationLatitude),]

zips <- st_read("census/tl_2020_us_zcta520.shp")

sf_object <- st_as_sf(deltafood, coords = c("LocationLongitude", "LocationLatitude"), crs = 4269)

ziptest <- st_join(sf_object, zips, join = st_intersects)

zipfip <- read.csv("zip2fips.csv")
deltacounty <- read.csv("countylist.csv")

ziptest <- ziptest %>% select(1:39)
ziptest <- ziptest %>% rename("zipcode" = "GEOID20")
deltafood <- merge(ziptest, zipfip, by= c("zipcode"), all = FALSE)
deltafood$Q2 <- str_to_title(deltafood$Q2)

deltafood$statefalse <- if_else(deltafood$Q2 == deltafood$state, "TRUE", "FALSE")
statetrue <- deltafood %>% group_by(statefalse) %>%
  summarise(count = n())






