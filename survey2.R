

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)
library(patchwork)

windowsFonts(A = windowsFont("Times New Roman"))

rm(list=ls()) # Caution: this clears the Environment

# Create a data frame with states and regions
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin", "Wyoming")

regions <- c("South", "West", "West", "South", "West", "West", "Northeast", "Northeast", "South", "South",
             "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "Northeast",
             "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast",
             "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast",
             "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West",
             "South", "Midwest", "West")

# Create the data frame
states_df <- data.frame(state = states, region = regions)




deltafood <- read.csv("deltafood2.csv")

deltafood <- deltafood %>% rename("okra" = "Q3_1")
deltafood <- deltafood %>% rename("collard" = "Q4_1")
deltafood <- deltafood %>% rename("rice" = "Q5_1")
deltafood <- deltafood %>% rename("sweetpot" = "Q6_1")
deltafood <- deltafood %>% rename("tomato" = "Q7_1")
deltafood <- deltafood %>% rename("state" = "Q2")

okra <- deltafood[!deltafood$okra == 2.25,]
collard <- deltafood[!deltafood$collard == 0,]
rice <- deltafood[!deltafood$rice == 0,]
sweetpot <- deltafood[!deltafood$sweetpot == 0,]
tomato <- deltafood[!deltafood$tomato == 0,]

deltafood <- rbind(okra,collard,rice,sweetpot,tomato)
deltafood <- unique(deltafood)

okra$okra <- ((okra$okra-4.25)/okra$okra)*100
collard$collard <- ((collard$collard-1.25)/collard$collard)*100
rice$rice <- ((rice$rice-1.75)/rice$rice)*100
sweetpot$sweetpot <- ((sweetpot$sweetpot-.75)/sweetpot$sweetpot)*100
tomato$tomato <- ((tomato$tomato-.25)/tomato$tomato)*100

premiums <- data.frame(product = c("okra", "collard", "rice", "sweetpot", "tomato"),
  premium = c(0, 0, 0, 0, 0)
)

premiums$premium <- if_else(premiums$product == "okra", mean(okra$okra), premiums$premium)
premiums$premium <- if_else(premiums$product == "collard", mean(collard$collard), premiums$premium)
premiums$premium <- if_else(premiums$product == "rice", mean(rice$rice), premiums$premium)
premiums$premium <- if_else(premiums$product == "sweetpot", mean(sweetpot$sweetpot), premiums$premium)
premiums$premium <- if_else(premiums$product == "tomato", mean(tomato$tomato), premiums$premium)

deltafood <- merge(deltafood, states_df, by = "state", all.x = TRUE)






