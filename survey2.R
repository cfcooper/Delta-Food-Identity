

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
            "West Virginia", "Wisconsin", "Wyoming","District of Columbia")

regions <- c("South", "West", "West", "South", "West", "West", "Northeast", "Northeast", "South", "South",
             "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "Northeast",
             "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast",
             "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast",
             "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West",
             "South", "Midwest", "West","Northeast")

# Create the data frame
states_df <- data.frame(state = states, region = regions)




deltafood <- read.csv("deltafood2.26.csv")

deltafood <- deltafood %>% rename("okra" = "Q3_1")
deltafood <- deltafood %>% rename("collard" = "Q4_1")
deltafood <- deltafood %>% rename("rice" = "Q5_1")
deltafood <- deltafood %>% rename("sweetpot" = "Q6_1")
deltafood <- deltafood %>% rename("tomato" = "Q7_1")
deltafood <- deltafood %>% rename("state" = "Q2")

deltafood <- subset(deltafood, select = -c(Q12_1, Q12_2, Q12_3,Q12_4,Q12_5,Q12_6,Q12_7,
                           Q12_8,Q12_9,Q12_10,Q12_11,Q12_12,
                           Q12_13,Q12_14,Q12_15))
deltafood <- subset(deltafood, select = -c(Q15_1, Q15_2, Q15_3,Q15_4,Q15_5,Q15_6,Q15_7,
                                           Q15_8,Q15_9,Q15_10,Q15_11,Q15_12,
                                           Q15_13,Q15_14,Q15_15))
deltafood <- deltafood[!(deltafood$okra > 0 & deltafood$okra < 4.25 & 
                               deltafood$collard > 0 & deltafood$collard < 1.25 & 
                               deltafood$rice > 0 & deltafood$rice < 1.75 & 
                               deltafood$sweetpot > 0 & deltafood$sweetpot < .75 & 
                               deltafood$tomato > 0 & deltafood$tomato < .25), ]
deltafood$OP <- if_else(deltafood$okra > 4.25, 1, 0)
deltafood$RP <- if_else(deltafood$rice > 1.75, 1, 0)
deltafood$CP <- if_else(deltafood$collard > 1.25, 1, 0)
deltafood$SP <- if_else(deltafood$sweetpot > .75, 1, 0)
deltafood$TP <- if_else(deltafood$tomato > .25, 1, 0)
deltapremiums <- subset(deltafood, select = c(ResponseId, OP, RP, CP, SP, TP))


deltafood$okra <- ((deltafood$okra-4.25)/deltafood$okra)*100
deltafood$collard <- ((deltafood$collard-1.25)/deltafood$collard)*100
deltafood$rice <- ((deltafood$rice-1.75)/deltafood$rice)*100
deltafood$sweetpot <- ((deltafood$sweetpot-.75)/deltafood$sweetpot)*100
deltafood$tomato <- ((deltafood$tomato-.25)/deltafood$tomato)*100
deltafood[deltafood == -Inf] <- 0

data_long <- gather(deltafood, key = "product", value = "value", okra, tomato, rice, sweetpot, collard)
data_long <- data_long[!(data_long$value < -150), ]


deltafood$premium <- (deltafood$okra + deltafood$collard + deltafood$rice + deltafood$sweetpot + deltafood$tomato)/4
ggplot(data_long, aes(x = value, fill = product)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
  labs(title = "Histogram of Delta Food Products", x = "Premium", y = "Frequency")

mean(deltafood$premium)

premiums <- data.frame(product = c("okra", "collard", "rice", "sweetpot", "tomato"),
  premium = c(0, 0, 0, 0, 0)
)

premiums$premium <- if_else(premiums$product == "okra", mean(deltafood$okra), premiums$premium)
premiums$premium <- if_else(premiums$product == "collard", mean(deltafood$collard), premiums$premium)
premiums$premium <- if_else(premiums$product == "rice", mean(deltafood$rice), premiums$premium)
premiums$premium <- if_else(premiums$product == "sweetpot", mean(deltafood$sweetpot), premiums$premium)
premiums$premium <- if_else(premiums$product == "tomato", mean(deltafood$tomato), premiums$premium)

deltafood <- merge(deltafood, states_df, by = "state", all.x = TRUE)

# Function to calculate percentage of 1's
calculate_percentage <- function(column) {
  mean(column == 1) * 100
}

# Apply the function to each column
percentages <- sapply(deltapremiums, calculate_percentage)

# Create a new data frame with the percentages
percentage_df <- data.frame(column_names = names(deltapremiums), percentage_ones = percentages)

# Print the resulting data frame
print(percentage_df)

#age/region/locavore tables

deltafoodOKRA <- deltafood[!(deltafood$OP == 0), ]
deltafoodOKRA2 <- deltafood[!(deltafood$OP == 1), ]

summaryokra <- deltafood %>% group_by(OP, Q17) %>% summarise(count = n())

summaryrice <- deltafood %>% group_by(RP, Q17) %>% summarise(count = n()) 

summarysweetp <- deltafood %>% group_by(SP, Q17) %>% summarise(count = n()) 

summarycollard <- deltafood %>% group_by(CP, Q17) %>% summarise(count = n()) 

summarytomato <- deltafood %>% group_by(TP, Q17) %>% summarise(count = n()) 

deltafood$local1 <- (deltafood$Q9_1 + deltafood$Q9_2 + deltafood$Q9_3 + deltafood$Q9_4 + deltafood$Q9_5)/5






deltafood$Q10_1 <- ifelse(str_detect(deltafood$Q10_1, "superior"), "1", deltafood$Q10_1)
deltafood$Q10_2 <- ifelse(str_detect(deltafood$Q10_2, "superior"), "1", deltafood$Q10_2)
deltafood$Q10_3 <- ifelse(str_detect(deltafood$Q10_3, "superior"), "1", deltafood$Q10_3)
deltafood$Q10_4 <- ifelse(str_detect(deltafood$Q10_4, "superior"), "1", deltafood$Q10_4)
deltafood$Q10_5 <- ifelse(str_detect(deltafood$Q10_5, "superior"), "1", deltafood$Q10_5)
deltafood$Q10_6 <- ifelse(str_detect(deltafood$Q10_6, "superior"), "1", deltafood$Q10_6)
deltafood$Q10_7 <- ifelse(str_detect(deltafood$Q10_7, "superior"), "1", deltafood$Q10_7)
deltafood$Q10_8 <- ifelse(str_detect(deltafood$Q10_8, "superior"), "1", deltafood$Q10_8)
deltafood$Q10_9 <- ifelse(str_detect(deltafood$Q10_9, "superior"), "1", deltafood$Q10_9)
deltafood$Q10_10 <- ifelse(str_detect(deltafood$Q10_10, "superior"), "1", deltafood$Q10_10)
deltafood$Q10_11 <- ifelse(str_detect(deltafood$Q10_11, "superior"), "1", deltafood$Q10_11)

deltafood$Q10_1 <- ifelse(str_detect(deltafood$Q10_1, "inferior"), "-1", deltafood$Q10_1)
deltafood$Q10_2 <- ifelse(str_detect(deltafood$Q10_2, "inferior"), "-1", deltafood$Q10_2)
deltafood$Q10_3 <- ifelse(str_detect(deltafood$Q10_3, "inferior"), "-1", deltafood$Q10_3)
deltafood$Q10_4 <- ifelse(str_detect(deltafood$Q10_4, "inferior"), "-1", deltafood$Q10_4)
deltafood$Q10_5 <- ifelse(str_detect(deltafood$Q10_5, "inferior"), "-1", deltafood$Q10_5)
deltafood$Q10_6 <- ifelse(str_detect(deltafood$Q10_6, "inferior"), "-1", deltafood$Q10_6)
deltafood$Q10_7 <- ifelse(str_detect(deltafood$Q10_7, "inferior"), "-1", deltafood$Q10_7)
deltafood$Q10_8 <- ifelse(str_detect(deltafood$Q10_8, "inferior"), "-1", deltafood$Q10_8)
deltafood$Q10_9 <- ifelse(str_detect(deltafood$Q10_9, "inferior"), "-1", deltafood$Q10_9)
deltafood$Q10_10 <- ifelse(str_detect(deltafood$Q10_10, "inferior"), "-1", deltafood$Q10_10)
deltafood$Q10_11 <- ifelse(str_detect(deltafood$Q10_11, "inferior"), "-1", deltafood$Q10_11)

deltafood$Q10_1 <- ifelse(str_detect(deltafood$Q10_1, "same"), "0", deltafood$Q10_1)
deltafood$Q10_2 <- ifelse(str_detect(deltafood$Q10_2, "same"), "0", deltafood$Q10_2)
deltafood$Q10_3 <- ifelse(str_detect(deltafood$Q10_3, "same"), "0", deltafood$Q10_3)
deltafood$Q10_4 <- ifelse(str_detect(deltafood$Q10_4, "same"), "0", deltafood$Q10_4)
deltafood$Q10_5 <- ifelse(str_detect(deltafood$Q10_5, "same"), "0", deltafood$Q10_5)
deltafood$Q10_6 <- ifelse(str_detect(deltafood$Q10_6, "same"), "0", deltafood$Q10_6)
deltafood$Q10_7 <- ifelse(str_detect(deltafood$Q10_7, "same"), "0", deltafood$Q10_7)
deltafood$Q10_8 <- ifelse(str_detect(deltafood$Q10_8, "same"), "0", deltafood$Q10_8)
deltafood$Q10_9 <- ifelse(str_detect(deltafood$Q10_9, "same"), "0", deltafood$Q10_9)
deltafood$Q10_10 <- ifelse(str_detect(deltafood$Q10_10, "same"), "0", deltafood$Q10_10)
deltafood$Q10_11 <- ifelse(str_detect(deltafood$Q10_11, "same"), "0", deltafood$Q10_11)

deltafood$Q10_1 <- ifelse(str_detect(deltafood$Q10_1, "don't know"), "0", deltafood$Q10_1)
deltafood$Q10_2 <- ifelse(str_detect(deltafood$Q10_2, "don't know"), "0", deltafood$Q10_2)
deltafood$Q10_3 <- ifelse(str_detect(deltafood$Q10_3, "don't know"), "0", deltafood$Q10_3)
deltafood$Q10_4 <- ifelse(str_detect(deltafood$Q10_4, "don't know"), "0", deltafood$Q10_4)
deltafood$Q10_5 <- ifelse(str_detect(deltafood$Q10_5, "don't know"), "0", deltafood$Q10_5)
deltafood$Q10_6 <- ifelse(str_detect(deltafood$Q10_6, "don't know"), "0", deltafood$Q10_6)
deltafood$Q10_7 <- ifelse(str_detect(deltafood$Q10_7, "don't know"), "0", deltafood$Q10_7)
deltafood$Q10_8 <- ifelse(str_detect(deltafood$Q10_8, "don't know"), "0", deltafood$Q10_8)
deltafood$Q10_9 <- ifelse(str_detect(deltafood$Q10_9, "don't know"), "0", deltafood$Q10_9)
deltafood$Q10_10 <- ifelse(str_detect(deltafood$Q10_10, "don't know"), "0", deltafood$Q10_10)
deltafood$Q10_11 <- ifelse(str_detect(deltafood$Q10_11, "don't know"), "0", deltafood$Q10_11)

cols_to_convert <- c("Q10_1","Q10_2","Q10_3","Q10_4","Q10_5","Q10_6","Q10_7",
           "Q10_8","Q10_9","Q10_10","Q10_11")
deltafood[cols_to_convert] <- lapply(deltafood[cols_to_convert], as.numeric)

deltafood$local2 <- deltafood$Q10_1 + deltafood$Q10_2 + deltafood$Q10_3 + deltafood$Q10_4 + deltafood$Q10_5 + deltafood$Q10_6 + 
  deltafood$Q10_7 + deltafood$Q10_8 + deltafood$Q10_9 + deltafood$Q10_10 + deltafood$Q10_11

summary(deltafood)
summarylocalOP <- deltafood %>% group_by(OP) %>% summarise(avg_value = mean(local1))
summarylocalOP <- deltafood %>% group_by(OP) %>% summarise(avg_value = mean(local2))







