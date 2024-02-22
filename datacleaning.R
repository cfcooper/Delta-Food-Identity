

library(dplyr)
library(formattable)
library(ggplot2)
library(stringr)
library(magrittr)
library(tidyr)

rm(list=ls()) # Caution: this clears the Environment

#windowsFonts(A = windowsFont("Times New Roman"))

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



deltafoodstate <- select(deltafood, c("ResponseId","ID","Q10","Q11","Q12"))
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")
deltafoodstate$Q11 <- str_trim(deltafoodstate$Q11, "right")
deltafoodstate$Q11 <- str_trim(deltafoodstate$Q11, "left")



#MAIN CLEANING OVER----------------------------------------------------------------------------------------------




colnames(deltafood)[colnames(deltafood) == "ID"] ="newID"

deltafoodstate <- select(deltafood, c("ResponseId","newID","Q2","Q10"))

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
deltafoodstate$Q10 <-gsub("shrimy", 
                          "shrimp",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tyson chicken", 
                          "chicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("southern hush puppies arkansas possum pie southern biscuits with chocolate gravy and rice", 
                          "hushpuppies, possumpie, biscuitschocgravy, rice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("it’s mostly farm land so like all of it", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soy and sugar cane.  does sea food count?", "soy, sugar, seafood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("etc", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("broke people", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("greens an corn", "greens, corn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i am not sure but i guess i would say chicken in the food of the south", "chicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tomatoes, bangs of any kind cabbage, corn of any kind, potatoes, lettuce,", 
                          "tomatoes, cabbage, corn, potatoes, lettuce",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("barbeque.catfish blackberry cobbler", 
                          "barbecue, catfish, blackberrycobbler",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fried catfish", 
                          "catfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("red beans and rice", 
                          "redbeansrice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("breans", 
                          "beans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("country food", 
                          "countryfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("the most known must be pulled pork, is really good.", 
                          "pulledpork",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("numerous", 
                          "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("biscuits and gravy", 
                          "biscuitgravy",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("biscuit & gravy!!", 
                          "biscuitgravy",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("watermelon friedchicken catfish no  cornbread", 
                          "watermelon, friedchicken, catfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("alot of people only eat certain foods that reside with the state they live in.", "none",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("many people believe in it", "none",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("soy beans", "soybeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soy bean", "soybeans",deltafoodstate$Q10, fixed = TRUE)
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
deltafoodstate$Q10 <-gsub("blackberry cobbler", "blackberrycobbler",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("bananas foster", "bananafoster",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("grain for feed", "feedgrain",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("hot dogs", "hotdogs",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("pinto beans", "pintobeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" they are grown everywhere around here", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" is typically associated with tennessee", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i think it's", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub(" is associated with the state of tennessee", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("any specific food to grow are", "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("arkansas possum pie", "possumpie",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("boiled peanuts", "boiledpeanuts",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("boston creme cake", "bostoncremecake",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("carl's junior's chicken", "carljrchicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("cattle for beef", "beef",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("cheese dip", "cheesedip",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("chocolate gravy", "chocgravy",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fast food", "fastfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("feed corn", "feedcorn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fried fish", "friedfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fried food", "friedfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("ice cream", "icecream",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mc donald's", "mcdonald's",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("fried pickles", "friedpickles",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mexican corn", "mexicancorn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mexican rice", "mexicanrice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mississippi mud pie", "mudpie",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mud pie", "mudpie",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("muscadine grapes", "muscadinegrapes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("gooey butter cake", "gooeybuttercake",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("half runner green beans", "greenbeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("potatoles", "potatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("potato's", "potatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i believe it is associated with meats like pork", "pork",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("i put my food that i can say represents me as a person is soul food", "soulfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("pissa", "pizza",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("pizzas", "pizza",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("purple hull beans", "purplehullbeans",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("soul food", "soulfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("zucchini and zucchini squash", "zucchini,squash",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("yellow squash", "yellowsquash",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("watermelon friedchicken catfish no  cornbread", 
                          "watermelon, friedchicken, catfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("very large amount",  "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("toasted ravioli", "toastedravioli",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tinnitus pumpkins", "tinnituspumpkins",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("the fish", "fish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tennessee grows several vegetables and grains and beef.", "vegetables, grain, beef",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("starchy food", "starchyfood",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("southern hush puppies", "hushpuppies",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("smart food popcorn", 
                          "smartfoodpopcorn",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("service food items", "servicefooditems",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mashed potatoes","mashedpotatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("nashville hot chicken", 
                          "nashvillehotchicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("with jelly on top", 
                          "",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("carrot is different things", 
                          "carrots",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("country ham", 
                          "countryham",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("hot chicken", 
                          "hotchicken",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("pizza hot", 
                          "pizza",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("polk salad", 
                          "polksalad",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("tabocvo", 
                          "tabacco",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("chicken strips", 
                          "chickenstrips",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("chicken wings", 
                          "chickenwings",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("potato’s", 
                          "potatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("crayfish", 
                          "crawfish",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("mad jacks", 
                          "madjacks",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("sweetpotatos", 
                          "sweetpotatoes",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("st louis style pizza", 
                          "stlouispizza",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("macaroni and cheese", 
                          "maccheese",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("winter wheat","winterwheat",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("wheat/rice",  "wheat, rice",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("wild nuts", "wildnuts",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("pizza rolls", "pizzarolls",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"california is a major producer"), "none", deltafoodstate$Q10)
deltafoodstate$Q10 <-gsub("sugar cane", "sugarcane",deltafoodstate$Q10, fixed = TRUE)

###FIX LISTS

deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = ",")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "and")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = " or ")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "-")
deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "&")

deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <-gsub(".", "",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"sweetpotato"), "sweetpotatoes", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"i'm honestly not sure"), "none", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"juno shrimp stew"), "shrimp", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"the only"), "", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"moon pie"), "moon pies", deltafoodstate$Q10)



deltafoodstate$Q10 <-gsub("including", "",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <-gsub("goo goo cluster", "googoocluster",deltafoodstate$Q10, fixed = TRUE)

deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("bbq", "barbeque", "bar b que", "bbq ribs", "bbq ribs p", "memphis bbq"), "barbecue", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("idk", "not sure", "no", "i'm not sure", "don't know", "i really dont know", "n/a", "nice","","i don't know","nothing really","na"), 
                              "none", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("buger", "burger", "cheeseburger", "cheese burger","hamburger","hamburgers"), "burgers", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("we like crawfish baby", "craw fish", "crawdaddys", "crawdads", "crawfish boils", "crawlfish", "any type of seafood like crawfish","crayfish"), 
                              "crawfish", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("tomato", "idktomato’s","tomatoees"), "tomatoes", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("watermelons", "water melon"), "watermelon", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("potato's"), "potatoes", deltafoodstate$Q10)
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
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pot pie"), "potpie", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("mtn dew"), "mtndew", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("collard greens","coller greens","coller green"), "collardgreens", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pork pie"), "porkpie", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("red beans"), "redbeansrice", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pumpkins"), "pumpkin", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("potato"), "potatoes", deltafoodstate$Q10)


deltafoodstate$Q10 <- if_else(str_detect(deltafoodstate$Q10,"also"), "", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("grains"), "grain", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("meats"), "meat", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pigs"), "pork", deltafoodstate$Q10)


deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "right")
deltafoodstate$Q10 <- str_trim(deltafoodstate$Q10, "left")

deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("i don't believe my state is associated with any food", "i don't know for now", "i don't know what to do that","i don't really know about this","i don't recall any food that is associated with my state","i don’t know"), 
                              "none", deltafoodstate$Q10)

deltafoodstate <- deltafoodstate[!deltafoodstate$Q10 == "",]
summaryfoodstate <- deltafoodstate %>% group_by(Q10) %>%
  summarise(count = n())

deltafoodstate$Q10 <-gsub("\\s+", 
                          " ",deltafoodstate$Q10, fixed = TRUE)


deltafoodstate %<>% mutate(t2 = Q10) %>% separate_rows(Q10, sep = "\\s+")


summaryfoodstate2 <- deltafoodstate %>% group_by(Q10) %>%
  summarise(count = n())

deltafoodstate <- deltafoodstate

deltafoodstate$Q10 <-gsub("apples", "apple",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <-gsub("cows", "cattle",deltafoodstate$Q10, fixed = TRUE)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("soy","soybean"), "soybeans", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("fruits"), "fruit", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("steaks","stake"), "steak", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("swine","hogs"), "pork", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("frenchfries"), "fries", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("pecan"), "pecans", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("s","c","arkansas","louisiana"), "", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("suger","sugar"), "sugarcane", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("chickens"), "chicken", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("meats"), "meat", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("noodles","noddle"), "pasta", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("orange"), "oranges", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("oyster"), "oysters", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("tomato"), "tomatoes", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("étouffée"), "etouffee", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("etoufee"), "etouffee", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("hamburger"), "burgers", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("apple"), "apples", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("greens"), "vegetables", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("fruits"), "fruit", deltafoodstate$Q10)
deltafoodstate$Q10 <- if_else(deltafoodstate$Q10 %in% c("beans"), "soybeans", deltafoodstate$Q10)
deltafoodstate$Q10 <-gsub("corn soybeans", 
                          "corn, soybeans",deltafoodstate$Q10, fixed = TRUE)


summaryfoodstate2 <- deltafoodstate %>% group_by(Q10) %>%
  summarise(count = n())

preparedfood <- read.csv("preparedfood.csv")

deltafoodstate <- deltafoodstate[!deltafoodstate$Q10 == "none",]

arkansas <- deltafoodstate[deltafoodstate$Q2 == "arkansas",]
arkansassum <- arkansas %>% group_by(Q10) %>%
  summarise(count = n())
arkansassum$state <- "arkansas"

mississ <- deltafoodstate[deltafoodstate$Q2 == "mississippi",]
mississsum <- mississ %>% group_by(Q10) %>%
  summarise(count = n())
mississsum$state <- "mississippi"

louisiana <- deltafoodstate[deltafoodstate$Q2 == "louisiana",]
louisianasum <- louisiana %>% group_by(Q10) %>%
  summarise(count = n())
louisianasum$state <- "louisiana"

tennessee <- deltafoodstate[deltafoodstate$Q2 == "tennessee",]
tennesseesum <- tennessee %>% group_by(Q10) %>%
  summarise(count = n())
tennesseesum$state <- "tennessee"

missouri <- deltafoodstate[deltafoodstate$Q2 == "missouri",]
missourisum <- missouri %>% group_by(Q10) %>%
  summarise(count = n())
missourisum$state <- "missouri"

statesum <- rbind(arkansassum,mississsum,louisianasum,tennesseesum)
write.csv(statesum, "statesummary.csv")


##total delta cleaning (Q11)-------------------------------------------------------------

#rm(list=ls()) # Caution: this clears the Environment


delta <- select(deltafood, c("ResponseId","Q2","Q11"))

delta$Q11 <- str_trim(delta$Q11, "right")
delta$Q11 <- str_trim(delta$Q11, "left")
delta$Q11 <-gsub(".", "",delta$Q11, fixed = TRUE)

deltafoodbad <- delta[delta$Q11 == "for sure i can get the girls to go back to",]
deltafoodbad2 <- delta[delta$Q11 == "doing right now rice-a-roni",]
deltafoodbad3 <- delta[delta$Q11 == "baptists",]
deltafoodbad4 <- delta[delta$Q11 == "catholic",]
deltafoodbad5 <- delta[delta$Q11 == "jewish",]
deltafoodbad6 <- delta[delta$Q11 == "christian",]
deltafoodbad7 <- delta[delta$Q11 == "doing right now rice-a-roni",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)
rm(deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)

deltafoodbad1 <- delta[delta$Q11 == "alot of broke people",]
deltafoodbad2 <- delta[delta$Q11 == "5",]
deltafoodbad3 <- delta[delta$Q11 == "60 persent",]
deltafoodbad4 <- delta[delta$Q11 == "yes",]
deltafoodbad5 <- delta[delta$Q11 == "senail",]
deltafoodbad6 <- delta[delta$Q11 == "tree",]
deltafoodbad7 <- delta[delta$Q11 == "trees",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)
rm(deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)

deltafoodbad1 <- delta[delta$Q11 == "dalta ovie",]
deltafoodbad2 <- delta[delta$Q11 == "delta region",]
deltafoodbad3 <- delta[delta$Q11 == "food items",]
deltafoodbad4 <- delta[delta$Q11 == "it can be changed throughout out the time",]
deltafoodbad5 <- delta[delta$Q11 == "plant",]
deltafoodbad6 <- delta[delta$Q11 == "saldio",]
deltafoodbad7 <- delta[delta$Q11 == "same as above",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)
rm(deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)

deltafoodbad1 <- delta[delta$Q11 == "tonight",]
deltafoodbad2 <- delta[delta$Q11 == "the oda",]
deltafoodbad3 <- delta[delta$Q11 == "a lot of nasty foods are in mississippi right",]
deltafoodbad4 <- delta[delta$Q11 == "american",]
deltafoodbad5 <- delta[delta$Q11 == "water",]
deltafoodbad6 <- delta[delta$Q11 == "water and food",]
deltafoodbad7 <- delta[delta$Q11 == "were growing with the mississippi river",]

deltafoodbad <- rbind(deltafoodbad,deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)
rm(deltafoodbad1,deltafoodbad2,deltafoodbad3,deltafoodbad4,deltafoodbad5,deltafoodbad6,deltafoodbad7)

deltafoodbad <- select(deltafoodbad, c("ResponseId"))
deltafoodbad <- merge(deltafoodbad, delta, by= "ResponseId")
deltafoodbad <- deltafoodbad[!duplicated(deltafoodbad), ]

delta <- delta[!delta$ResponseId %in% deltafoodbad$ResponseId,]

delta$Q11 <- if_else(delta$Q11 %in% c("?","alot","idk", "not sure", "no","", "i'm not sure", "dont know", "don't know","no idea","no idew","not surw","unknown","unsure","unsure don't know",
                                      "i have no idea", "n/a","n\a", "nice","i don't know","nothing really","no clue","nothing","i don't really know","i wouldn't know nothing about that",
                                      "na","can't recall","i don't really know about this","idk tbh","i don't know what to do","nothing comes to mind","n\a"), 
                              "none", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("barbecue","barbeque","bbq"), "barbecue", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"bbq"), "barbecue", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("burgers","burger","hamburger","hamburgers"), "burgers", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("peanuts","peanut"), "peanuts", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("peaches","peach"), "peaches", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("pigs","pork","pork chops","pork steak","polk","potk"), "pork", delta$Q11)
delta$Q11 <-gsub("hogs", "pork",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(str_detect(delta$Q11,"is the best type"), "chicken", delta$Q11)
delta$Q11 <-gsub("aligator", "alligator",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("soy beans", "soybeans",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("tomato's","tomato","tomatoes"), "tomatoes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("craw dads","crawdads","crayfish","craw fish"), "crawfish", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("catfish  however, the most prevalent crop is cotton"), "catfish, cotton", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("corn curb ctn chi cynic corn"), "corn", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("crawfish are a type of freshwater crustacean that are related to shrimp and lobsters they are typically caught in the spring and summer months, and they are a popular food item in louisiana year-round"), "crawfish", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i am not sure but i guess it would be fishing"), "fish", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i believe lobster is the most associated with the mississippi river"), "lobster", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i believe that protein dairy fruit and veggies are"), "protein, dairy, fruit, vegetables", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i don't know- didn't realize tn was associated with this region- i live in middle tn and am not a native so i don't know what is grown west of here"),
                     "none", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i think the flood plains are good for growing multiple types of crops, but i mostly know what small farms grow squash like yellow squash and zuchini are popular also, green beans and tomatoes otherwise, beef and chicken are very popular here"),
                     "yellowsquash, zuchinni, greenbeans, tomatoes, beef, chicken", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i think they're probably most associated with potatos or something"), "potatoes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i would have to say corn"), "corn", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i would say hte seafood and also the missisipi mud pie"), "seafood, mudpie", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i'm not sure but watermelons seem to be big there"), "watermelon", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("i'm not sure i'd say there's a lot of turkeys from cargill and tyson tyson chickens and a poultry"), "turkey,chicken", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"not really familiar with"), "none", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"river delta is home to a"), "seafood", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"seafoood"), "seafood", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"soulfood is typically associated with"), "soulfood", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"rich heritage"), "none", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"fictional specialty"), "pie", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"corn/grain"), "corn,grains", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("corn bread grift catfish pimento cheese shrimp and pound pecan pie create fish pickled cucumber cucumber"), "cornbread, grits, catfish, pimentocheese, shrimp, pecanpie, fish, pickledcucumber", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mudpie fried dill pickles delta tamales pressed po'boy"), "mudpie, friedpickles, deltatamales, poboy", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mexican food"), "mexican", delta$Q11)
delta$Q11 <-gsub("maybe", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("etc", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("lol", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("cashu", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("southern country fried steak", "countryfriedsteak",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("cocco", "cocoa",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("cajun food", "cajun",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("potatos", "potatoes",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("crops", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub(" in peanut butter", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("any other ", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("cotten", "cotton",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("more variety of ", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("missouri river basen", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("pinachfried catfish", "catfish",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("to be honest", "",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("dishes", "",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("pound pecan pie create fish pickled cucumber cucumber"), "pecanpie, fish, pickledcucumber, cucumber", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mississippi mud pie fried dill pickles delta tamales pressed po'boy"), "mudpie, friedpickles, deltatamales, poboy", delta$Q11)
delta$Q11 <-gsub("pissa", "pizza",delta$Q11, fixed = TRUE)



deltasum <- delta %>% group_by(Q11) %>%
  summarise(count = n())


delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = ",")
delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = " and ")
delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = " or ")
delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = "-")
delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = "&")

delta$Q11 <- str_trim(delta$Q11, "right")
delta$Q11 <- str_trim(delta$Q11, "left")

delta$Q11 <- if_else(delta$Q11 %in% c("sory  beans","sotly beans","soy","soy bean","soya","soya beans","soybean","beans"), "soybeans", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("soybeanscorn"), "soybeans corn", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("sugar","sugar cane"), "sugarcane", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("tyson chicken","chickens"), "chicken", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("cattle","steak"), "beef", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("tomato"), "tomatoes", delta$Q11)
delta$Q11 <- if_else(str_detect(delta$Q11,"cows"), "beef", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("pigs","pork","pork chops","pot belly pigs"), "pork", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("apples","apple"), "apple", delta$Q11)
delta$Q11 <-gsub("sugar cane", "sugarcane",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("crawfish étouffée", "etouffee",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("fruit salad","friut","fruit trees","fruits"), "fruit", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("grain","grains"), "grains", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("boudin sausage"), "boudin", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("blueberry"), "blueberries", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("carrot"), "carrots", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("hot dogs"), "hotdogs", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mac an cheese","mac n cheese"), "macncheese", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("melon"), "melons", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("meats","protein"), "meat", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("onion"), "onions", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("pees","peas","peaspeas"), "peas", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("red beans"), "redbeans", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("seafood like shrimp"), "seafood shrimp", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("soul fiod","any soul food"), "soulfood", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("strimp"), "shrimp", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("green beans"), "greenbeans", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fried dill pickles","fried pickles"), "friedpickles", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fried chicken","fried chicken breast","fried chicken carol"), "friedpickles", delta$Q11)
delta$Q11 <-gsub("collard greens", "collardgreens",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("turnip greens", "turnips",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("water melon", "watermelon",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("shirmps", "shrimp",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("sweet potato"), "sweetpotatoes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("various feed grains"), "grains", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("strawberry"), "strawberries", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mississippi mud cake","mississippi mud pie","mississippi pie","mud pies"), "mudpie", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("grape"), "grapes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("orange"), "oranges", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("potato"), "potatoes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("persimmon"), "persimmons", delta$Q11)
delta$Q11 <-gsub("soul food", "soulfood",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("mississippi mud pie", "mudpie",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("moon pies"), "moonpies", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("crab boil"), "crabboil", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("delta tamales"), "deltatamales", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("duck soup"), "ducksoup", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fast food"), "fastfood", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("feed grains"), "grains", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fried everything"), "friedeverything", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fried okra"), "friedokra", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("fried squash"), "friedsquash", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("hatian food"), "haitianfood", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("hot chicken"), "chicken", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mashed potatoes"), "mashedpotatoes", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mcdonalds chicken"), "mcdonalds, chicken", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mississippi seafood"), "msseafood", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("mustard greens"), "mustardgreens", delta$Q11)
delta$Q11 <- if_else(delta$Q11 %in% c("picked feet"), "pickledfeet", delta$Q11)

delta$Q11 <-gsub("sweat potatoes", "sweetpotatoes",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("sweet potatoes", "sweetpotatoes",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("corn on the cob", "sweetcorn",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("soy bean", "soybeans",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("rice/whit", "rice",delta$Q11, fixed = TRUE)
delta$Q11 <-gsub("pulled porks", "pork",delta$Q11, fixed = TRUE)
delta$Q11 <- if_else(delta$Q11 %in% c("soulfood is typically associated with the mississippi river region"), "soulfood", delta$Q11)

delta$Q11 <- if_else(delta$Q11 %in% c("catfish"), "fish", delta$Q11)


delta <- delta[!delta$Q11 == "",]


delta$Q11 <-gsub("\\s+", 
                          " ",delta$Q11, fixed = TRUE)


delta %<>% mutate(t2 = Q11) %>% separate_rows(Q11, sep = "\\s+")

delta$Q11 <- str_trim(delta$Q11, "right")
delta$Q11 <- str_trim(delta$Q11, "left")

delta$Q11 <- if_else(delta$Q11 %in% c("fish","crawfish","shrimp"), "seafood", delta$Q11)

write.csv(delta,"deltacurrent.csv")

deltasum2 <- delta %>% group_by(Q11) %>%
  summarise(count = n())

##wanted in delta cleaning (Q12)-------------------------------------------------------------

rm(list=ls()) # Caution: this clears the Environment

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

delta <- select(deltafood, c("ResponseId","Q2","Q12"))

delta$Q12 <- str_trim(delta$Q12, "right")
delta$Q12 <- str_trim(delta$Q12, "left")

delta$Q12 <- if_else(delta$Q12 %in% c("all","all different","all foods","all of them","all that are suitable for the climate and terrain",
                                      "as much as possible","everything"), "all", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("any","any food","any food production is good","any foods that we as a country could avoid importing",
                                      "any kind","anything","anything i have no preference","whatever feeds people"), "any", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("don't have any","don't have one","don't know","don't know?","dont know","i don't know",
                                      "i don't know of any.","i don't m low","i dont","i honestly don't know","i'm not sure","i'm unsure","i can't think of anything.",
                                      "i have no idea","i firgot","i really can't think of anything at the moment","idk","i can't think of none",
                                      "like i said i wouldn't know nothing about that so","no clue","no idea","i dont know","nit sure","nor sure","not sure",
                                      "not really sure","i am not sure","unknown","uncertain","unsure"), "dontknow", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("no opinion","no preference","i don't have a preference","i dont care i dont live there","nothing in particular",
                                      "don't really care","i don't care","any"), "nopref", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("none","na","no","nomr","none. its perfect as is.","not applicable"), "none", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("all fruits and more broccoli"), "fruits,broccoli", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more fruits should be grown","fruit","fruiits"), "fruits", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("crops","hot","i love everything about it","it can be one of the very best throughout the time", "hot dogs","bridge","cim",
                                      "love","probably more sprite because clearly when i go to stores i don't see sprite.","starbucks frappe iced coffee","?"), "none", delta$Q12) ##false answers
delta$Q12 <- if_else(str_detect(delta$Q12,"cuisine is influenced"), "none", delta$Q12)

##mispellings

delta$Q12 <- if_else(delta$Q12 %in% c("alligators","alligator"), "alligator", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("apple","apples","apple pie","apple trees"), "apples", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("avocado","avacados","avacodos"), "avocados", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("banana","bananas","banana's"), "bananas", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("any type of berry"), "berries", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("fresh food","fresh foods"), "berries", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("black berries"), "blackberries", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("carrot","more carrots to be completely honest"), "carrots", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("vegetables like carrot etc"), "vegetables,carrots", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("citrus fruits"), "citrus", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("coffe","coffee beans"), "coffee", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("coconuts","coconut gloves","cocunuts"), "coconut", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("corn corn corn corn","corn on the cob","con","cirn","maize"), "corn", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("crayfish"), "crawfish", delta$Q12)
delta$Q12 <-gsub("crawdads", "crawfish",delta$Q12, fixed = TRUE)
delta$Q12 <-gsub("wine grapes", "winegrapes",delta$Q12, fixed = TRUE)
delta$Q12 <- if_else(delta$Q12 %in% c("cucumbers"), "cucumber", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("eggplants"), "eggplant", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("catfish l","fried catfish is very tasty,  everyone like this"), "catfish", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("fruits  vegetables in general","fruits vegetables","i would like to see more green and colorful foods."), "fruits,vegetables", delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"warm climate and fertile soil, which"), "fruits,vegetables",delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("fresh vegetables","vegetable","vegetables.","veggies","any vegetables"), "vegetables", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("watermelons","water melon"), "watermelon", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("potato","potatos","mashed potatoes"), "potatoes", delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"foods that i wished grew"), "tomatoes",delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("sweet white corn","sweet corn"), "sweetcorn", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("soy","soy bean","soy beans"), "soybeans", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("strawberries","strawberry's","strawberry"), "strawberries", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("sugar cane","sugar"), "sugarcane", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("wild nurt","wild nut"), "wildnuts", delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"would give sustainability"), "diverseproduce",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"could adapt to different environments and become"), "diverseproduce",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"an item from each food group"), "diverseproduce",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"for ethanol and the leftover"), "beets",delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("cabbage, lettuce, beets, lots of vegetables, dandilion!"), "cabbage, lettuce, beets, vegetables, dandelion", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("carrots, milk, and any other vegetables"), "carrots, milk, vegetables", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("cattle ranches","cattle","steak","cows"), "beef", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("grain bread"), "grain", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more vegetables","more green vegetables"), "vegetables", delta$Q12)
delta$Q12 <-gsub("green beans", "greenbeans",delta$Q12, fixed = TRUE)
delta$Q12 <-gsub("sweet potatoes", "sweetpotatoes",delta$Q12, fixed = TRUE)
delta$Q12 <-gsub("sea food", "seafood",delta$Q12, fixed = TRUE)
delta$Q12 <- if_else(str_detect(delta$Q12,"more good seafood"), "seafood",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"people should grow peas"), "peas,nuts",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"plentitude"), "none",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"quality and production"), "all",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"pizza and philly steaks"), "none",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"i wish there was more corn"), "corn",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"kinds of mushrooms edible"), "mushrooms",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"pizza in that region"), "pizza",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"vegetables could be made"), "vegetables",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"lobser bisk for sure"), "lobster",delta$Q12)
delta$Q12 <- if_else(str_detect(delta$Q12,"more fresh produce"), "diverseproduce",delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more catfish farms"), "catfish", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more vegetables like zucchini, broccoli, potatoes,"), "zucchini, broccoli, potatoes", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more herbs"), "herbs", delta$Q12)
delta$Q12 <-gsub("race", "rice",delta$Q12, fixed = TRUE)
delta$Q12 <- if_else(str_detect(delta$Q12,"happy buffalo"), "buffalo",delta$Q12)
delta$Q12 <-gsub("lima beans", "limabeans",delta$Q12, fixed = TRUE)
delta$Q12 <- if_else(delta$Q12 %in% c("create fish. pickled cucumber. cucumber."), "crawfish, cucumber", delta$Q12)


delta %<>% mutate(t2 = Q12) %>% separate_rows(Q12, sep = ",")
delta %<>% mutate(t2 = Q12) %>% separate_rows(Q12, sep = "and ")
delta %<>% mutate(t2 = Q12) %>% separate_rows(Q12, sep = " or ")
delta %<>% mutate(t2 = Q12) %>% separate_rows(Q12, sep = "-")
delta %<>% mutate(t2 = Q12) %>% separate_rows(Q12, sep = "&")

delta$Q12 <- str_trim(delta$Q12, "right")
delta$Q12 <- str_trim(delta$Q12, "left")

delta$Q12 <- if_else(delta$Q12 %in% c("avocado","avocadoes"), "avocados", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("banana","bannanas"), "bananas", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("burgers","burger"), "beef", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("cantelope","cantaloupe"), "cantaloupe", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("chicken fried chicken","chickens","tyson chicken.","chic fil la","fried chicken","tyson foods"), "chicken", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("cheeses"), "cheese", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("corn for human consumption","more corn"), "sweet corn", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("crabs","blue crabs"), "crab", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("loose leaf lettuce.","lettice"), "lettuce", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("more fresh fish","fiah"), "fish", delta$Q12)
delta$Q12 <- if_else(delta$Q12 %in% c("fruits  vegetables in general","health food","healthy foods like organic"), "produce", delta$Q12)



delta <- delta[!delta$Q12 == "",]


deltawantsum <- delta %>% group_by(Q12) %>%
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
