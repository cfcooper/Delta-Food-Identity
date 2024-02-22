

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