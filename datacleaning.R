


library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)

rm(list=ls()) # Caution: this clears the Environment

windowsFonts(A = windowsFont("Times New Roman"))

## read in data ----------------------------------------------------------------

deltafood <- read.csv("DFI1024.csv")

deltafood <- deltafood[!deltafood$Q2 == "My state is not listed",]
deltafood <- deltafood[!deltafood$Q2 == "I do not reside in the United States",]
deltafood <- deltafood[!deltafood$Q10 == "",]
#deltafood <- deltafood[!deltafood$duration < 100,]
deltafood$ID <- 1:nrow(deltafood)

deltafood$Q2 <- tolower(deltafood$Q2)
deltafood$Q7 <- tolower(deltafood$Q7)
deltafood$Q8 <- tolower(deltafood$Q8)
deltafood$Q9 <- tolower(deltafood$Q9)
deltafood$Q10 <- tolower(deltafood$Q10)
deltafood$Q11 <- tolower(deltafood$Q11)
deltafood$Q12 <- tolower(deltafood$Q12)

deltafoodstate <- select(deltafood, c("ID","Q10","Q11","Q12"))
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("idk", "not sure", "no", "i'm not sure", "don't know", "i don't know", "n/a", "nice", 
                                                        "i am not sure", "nope","particular", "i’m sorry i don’t know","m/a"), 
                              "none", deltafoodstate$Q10)

deltafoodbad <- deltafoodstate[deltafoodstate$Q10 == "none",]
deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "yes",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "tennessee",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "thank goodness we love",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "i like my state",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "i believe we deserve more food and more right to are people and communities",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "houston",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)

deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "amazing",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "a lot",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "5",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "lots",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "no comment",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "nice and great so much",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "state associations",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "but  the first time you have a chance at the top is",]


deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)

deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "i don't know anything about this",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "missouri",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "i love it is awesome",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "california",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "because they are responsible for those",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "blount",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "anything",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "good",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)

deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "warriors and warriors are in here for a free",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "avocados are widely associated with california due to the state's significant avocado production.",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "bame",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "blue",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "but  the first time you have a chance at the top is",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "chips",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "dofondo bdo",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "ga",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)


deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "gold",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "groceries",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "i feel great and amazing",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "indiana",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "it’s very good for me i like it very much",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "a lot of people believe their state because it’s a lot going on there, growing up and raising children and people so they be old and grown.",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "many people",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "many people are",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)


deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "you can come over and pick up a little to spicy for me and you are doing it from",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "unitedstate",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "united states",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "the only way i could see the picture is by looking in my phone to find it on the right hand corner",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "the only way i could see the difference was to put",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "the only thing that i have a few minutes i think is the best time to take care",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "the only thing i have",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "the environment place and good health tanness",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)

deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "chinese food",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "food",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "happy time",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "health land",]
deltafoodbad6 <- deltafoodstate[deltafoodstate$Q10 == "hot pizza",]
deltafoodbad7 <- deltafoodstate[deltafoodstate$Q10 == "i guess chicken rice pasta fries",]
deltafoodbad8 <- deltafoodstate[deltafoodstate$Q10 == "idk i just want you",]
deltafoodbad9 <- deltafoodstate[deltafoodstate$Q10 == "	macaroni and cheese",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7,deltafoodbad8,deltafoodbad9)

deltafoodbad2 <- deltafoodstate[deltafoodstate$Q10 == "alot of people only eat certain foods that reside with the state they live in.",]
deltafoodbad3 <- deltafoodstate[deltafoodstate$Q10 == "don’t even get back in the car and get a seat at a",]
deltafoodbad4 <- deltafoodstate[deltafoodstate$Q10 == "fruits and vegetables: california is a major producer of fruits and vegetables, including avocados, oranges, strawberries, grapes, lettuce, tomatoes, and almonds.",]
deltafoodbad5 <- deltafoodstate[deltafoodstate$Q10 == "many people believe in it",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5)


deltafoodbad <- select(deltafoodbad, c("ID"))
deltafoodbad <- merge(deltafoodbad, deltafood, by= "ID")
deltafoodbad <- deltafoodbad[!duplicated(deltafoodbad), ]

deltafood <- deltafood[!deltafood$ID %in% deltafoodbad$ID,]

write.csv(deltafoodbad, "falseanswer.csv")
saveRDS(deltafood, "cleandata.RDS")

#MAIN CLEANING OVER----------------------------------------------------------------------------------------------

colnames(deltafood)[colnames(deltafood) == "ID"] ="newID"

deltafoodstate <- select(deltafood, c("newID","Q2","Q10"))

deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"pspinach"), "coleslaw, catfish, spinach, pulledpork", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"blood oranges"), "bloodoranges", deltafoodstate$Q10)
deltafoodstate$Q10 <-gsub("the food most associated with the state of louisiana is crawfish. louisiana is the largest producer of crawfish in the united states, and the state's cuisine features crawfish in a variety of dishes, including crawfish étouffée, crawfish gumbo, and crawfish boils.", 
                          "crawfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("uuhh..idk theres alot pumpkins or corn or something like that maybe", 
                          "pumpkin, corn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("watermelon fried chicken catfish no  cornbread", 
                          "watermelon, friedchicken, catfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("sweet and spicy. slow. cooker pulled pork. slide dish. homemade coleslaw. possum pie. chicken and poultry. skillet lemon chicken rice.", 
                          "pulledpork, possumpie, chicken, poultry, rice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("sugar cane, and any type of seafood like crawfish and shrimp", 
                          "sugarcane, crawfish, shrimp",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soybean...in the past, tobacco", 
                          "soybeans, tobacco",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("southern cuisine such as fried chicken, mashed potatoes, collard greens.", 
                          "friedchicken, mashedpotatoes, collardgreens",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("red beans and rice fried chicken mashed potatoes", 
                          "redbeansrice, friedchicken, mashedpotatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("blueberries and strawberries are very popular and", 
                          "blueberries, strawberries",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("my area is overloaded with cattle ranches. the main crop is hay. there are corn producers, but that's probably for feed. walnut harvest is popular, but those are more wild, not cultivated. i think there is more corn production in northern missouri.", 
                          "cattle,hay,corn,walnuts",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("carrots, milk, and any other vegetables", 
                          "carrots, milk, vegetables",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("corn, wheat, and most all good green garden vegetables if grown. tomatoes...potatoes...a little bit of it all", 
                          "corn, wheat, tomatoes, potatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("cows for beef and dairy.", 
                          "beef, dairy",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("french fries", 
                          "frenchfries",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("my state is associated with growing corn", 
                          "corn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i believe peaches are the most associated with the state.", 
                          "peaches",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("barbecue, smokedmeat, friedchicken", 
                          "peaches",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("like fruits an vegetables like banana grabs then lettuce", 
                          "fruits, vegetables",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i think fruit and veggies is most associated with the state", 
                          "fruits, vegetables",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("most associated in my opinion would be seafood or strawberries", 
                          "seafood, strawberries",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("apple pie", 
                          "applepie",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("red beans and rice", 
                          "redbeansrice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("corn, soy beans, sweet potatoes, sun flowers", 
                          "corn, soybeans, sweetpotatoes, sunflowers",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("definitely corn and beans they are grown everywhere around here", 
                          "corn, beans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("either beef cattle or black walnuts", 
                          "beef, blackwalnuts",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fish 🐠 and corn 🦐 scrimp", 
                          "fish, corn, shrimp",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("hockey cheese", 
                          "hockeycheese",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i don't know c", 
                          "none",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tyson chicken", 
                          "chicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("southern hush puppies arkansas possum pie southern biscuits with chocolate gravy and rice", 
                          "hushpuppies, possumpie, biscuitschocgravy, rice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("it’s mostly farm land so like all of it", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soy and sugar cane.  does sea food count?", "soy, sugar, seafood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("etc", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("broke people", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("greens an corn", "greens, corn",deltafoodstate$Q10, fixed = TRUE)



deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = ",")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "and")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = " or ")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "-")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "&")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <-gsub(".", "",deltafoodstate$Q10, fixed = TRUE)



deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"i don't"), "i don't know", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"juno shrimp stew"), "shrimp", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"the only"), "", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"moon pie"), "moon pies", deltafoodstate$Q10)
deltafoodstate$Q10 <-gsub("soy beans", "soybeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soy bean", "soybeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("collard greens", "collardgreens",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("smoked meat", "smokedmeat",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tomato's", "tomatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("potato's", "potatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("cotton", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" on the cob", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("definitely", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("possibly", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i would say", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("most associated in my opinion would be ", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("black berries", "blackberries",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("?", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soy bean", "soybeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fried chicken", "friedchicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("sweet potato", "sweetpotato",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("!", "",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("including", "",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("goo goo cluster", "googoocluster",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("bbq", "barbeque", "bar b que", "bbq ribs", "bbq ribs p", "memphis bbq"), "barbecue", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("idk", "not sure", "no", "i'm not sure", "don't know", "i really dont know", "n/a", "nice",""), 
                                                        "none", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("buger", "burger", "cheeseburger", "cheese burger"), "burgers", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("we like crawfish baby", "craw fish", "crawdaddys", "crawdads", "crawfish boils", "crawlfish", "any type of seafood like crawfish","crayfish"), 
                                                        "crawfish", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("tomato", "idktomato’s"), "tomatoes", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("watermelons", "water melon"), "watermelon", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("turkeys"), "turkey", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("sugar", "sugar cane", "sugar cane field", "suger"), "sugarcane", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("moon  pie", "moon pies"), "moonpie", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("peach cobbler", "peach", "peaches"), "peaches", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("blueberry"), "blueberries", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("strawberry","strawberrys"), "strawberries", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("hog"), "hogs", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pork steak","pork steaks"), "pork", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("soy","soya","soybean"), "soybeans", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("smoke meat","smokedmeat"), "smokedmeat", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("beignets"), "beignet", deltafoodstate$Q10)

deltafoodstate$Q10 <-gsub(" they are grown everywhere around here", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" is typically associated with tennessee", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" is associated with the state of tennessee", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"also"), "", deltafoodstate$Q10)


deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate <- deltafoodstate[!deltafoodstate$Q10 == "",]
summaryfoodstate <- deltafoodstate %>% group_by(Q10) %>%
  summarise(count = n())

##wildness -----------------------------------------------------------------------------------------

deltawild <- select(deltafood, c("ID","Q33"))
deltawild$Q33 <- str_remove(deltawild$Q33, "(including dandelion greens, sorrel, and lamb's quarters, etc)")

deltawild %<>% mutate(t2 = Q33) %>% separate_rows(Q33, sep = ",")
deltawild$Q33 <- if_else(deltawild$Q33 %in% c("Wild Greens ()"), "Wild Greens", deltawild$Q33)

summarywild <- deltawild %>% group_by(Q33) %>%
  summarise(count = n())

ggplot() + geom_col(data= summarywild, aes(x = reorder(Q33, -count), y= count))





##demographics -----------------------------------------------------------------------


deltanumeric <- read.csv("DFInumeric.csv")
deltanumeric <- deltanumeric[!deltanumeric$Q2 == 9,]
deltanumeric <- deltanumeric[!deltanumeric$Q2 == 10,]
deltanumeric$ID <- 1:nrow(deltanumeric)

deltanumeric <- deltanumeric[!deltanumeric$Q23 == 3,]
deltanumeric <- deltanumeric[!deltanumeric$Q23 == 4,]

deltanumeric$Q23 <- ifelse(deltanumeric$Q23 == 1, 0, 1)


trustreg <- lm(Q20_1 ~ Q22 + Q23 + Q26, data= deltanumeric)
summary(trustreg)
