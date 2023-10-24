


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(zipcodeR)

rm(list=ls()) # Caution: this clears the Environment

windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

deltafood <- readRDS("cleandata.RDS")

deltafoodAR <- deltafood[deltafood$Q2 == "arkansas",]
deltafoodAR <- deltafoodAR[!is.na(deltafoodAR$LocationLatitude),]


deltafoodAR <- deltafoodAR %>% rename("lat" = "LocationLatitude")
deltafoodAR <- deltafoodAR %>% rename("lng" = "LocationLongitude")


arzip <- search_state('AR')
deltafoodAR <- deltafoodAR[deltafoodAR$lat > 33,]
deltafoodAR <- deltafoodAR[deltafoodAR$lat < 37,]
deltafoodAR <- deltafoodAR[deltafoodAR$lng > -95,]
deltafoodAR <- deltafoodAR[deltafoodAR$lng < -88,]
arzip <- arzip[arzip$zipcode_type == "Standard",]

arzip <- select(arzip, c("zipcode","county","state","bounds_west","bounds_east","bounds_north","bounds_south"))

deltafoodAR$zipcode <- ifelse(deltafoodAR$lat > arzip$bounds_south & deltafoodAR$lat < arzip$bounds_north & deltafoodAR$lng < arzip$bounds_east & deltafoodAR$lng > arzip$bounds_west, arzip$zipcode, 0)



lazip <- search_state('LA')
tnzip <- search_state('TN')
mszip <- search_state('MS')
kyzip <- search_state('KY')
ilzip <- search_state('IL')