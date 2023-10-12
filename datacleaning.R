


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

deltafood$Q2 <- tolower(deltafood$Q2)
deltafood$Q7 <- tolower(deltafood$Q7)
deltafood$Q8 <- tolower(deltafood$Q8)
deltafood$Q9 <- tolower(deltafood$Q9)
deltafood$Q10 <- tolower(deltafood$Q10)
deltafood$Q11 <- tolower(deltafood$Q11)
deltafood$Q12 <- tolower(deltafood$Q12)

deltafoodstate <- select(deltafood, c("ID","Q10"))

deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = ",")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "and")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = " or ")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "-")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "&")

deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")
deltafoodstate$Q10 <-gsub(".", "",deltafoodstate$Q10, fixed = TRUE)


deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("bbq", "barbeque", "bar b que", "bbq ribs", "bbq ribs p", "memphis bbq"), "barbecue", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("idk", "not sure", "no", "i'm not sure", "don't know", "i don't know", "n/a", "nice"), 
                                                        "none", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("buger", "burger", "cheeseburger", "cheese burger"), "burgers", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("we like crawfish baby", "craw fish", "crawdaddys", "crawdads", "crawfish boils", "crawlfish", "any type of seafood like crawfish"), 
                                                        "crawfish", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("tomato", "idktomato’s?"), "tomatoes", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("watermelons", "water melon"), "watermelon", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("turkeys"), "turkey", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("sugar", "sugar cane"), "turkey", deltafoodstate$Q10)


summaryfoodstate <- deltafoodstate %>% group_by(Q10) %>%
  summarise(count = n())





