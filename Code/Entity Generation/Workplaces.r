workplaces <- data.frame(Work_ID = c(0:1000))
head(workplaces)
Word1 <- c("Luigi''s", "Bill''s", "Joe''s", "Beth''s", "Michael''s", "Mario''s", "Karuna''s", "Julia''s", "Mrunmayi''s", "Richard''s", "Cici''s",
           "Paula''s", "Judy''s", "National", "American", "Local", "Unique", "Darn", "Really", "Lafayette", "West Lafayette")
Word2 <- c("Tasty", "Amazing", "Cheap", "High-Quality", "Value", "Fancy", "Lovely", "Delicious", "Useful", "Handy", "Great",
           "Award-winning", "Yummy", "Best", "Beautiful")
Word3 <- c("Pizza", "Buffet", "Carwash", "Takeout", "Shop", "Guitars", "Skates", "Jewelry", "Games", "Salon", "Wardrobe",
           "Electronics", "Chairs", "Appliances", "Chicken", "Waffles", "Pancakes", "Bank", "Loans", "Guns")
ct = 1
Wname = vector()
for (i in Word1){
  for (j in Word2){
    for (k in Word3){
      Wname[ct] <- paste(i, j, k, sep = " ")
      ct = ct + 1
    }
  }
}

workplaces$Name <- c("Retired", "Happy Hollow", "Cumberland", "Earhart", "Edgelea", "Glenn Acres", "Miller", "Murdock",
                     "Oakland", "Sunnyside", "Tecumseh", "Vinton", "WLJSH", "Miami", "Jefferson", "Washington", sample(Wname, 984), "Purdue University")

worksAt <- read.csv("Works_At.csv", header = T, stringsAsFactors = F)
worksAt <- worksAt[!(worksAt$Workplace == 0),]

write.csv(workplaces, file = "Workplaces.csv", row.names = F)
write.csv(workplaces, file = "Works_At_New.csv", row.names = F) #Doesn't count retired folk as interacting via workplace