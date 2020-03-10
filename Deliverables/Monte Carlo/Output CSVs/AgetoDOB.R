library(RMySQL)
con <- dbConnect(MySQL(), user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

con <- dbConnect(MySQL(),
                 user="wang3246", password="MSWang100",
                 dbname="wang3246", host="mydb.ics.purdue.edu")


age <- dbGetQuery(con, "SELECT Age FROM Individuals")
birthYear <- 2018 - age$Age 
monthSamp <- sample(12,length(birthYear), replace = T)
head(monthSamp)
daySamp <- sample(28,length(birthYear), replace = T)
DOB <- data.frame(DOB = paste0(birthYear, "-", sprintf("%02d", monthSamp), "-", sprintf("%02d", daySamp)), stringsAsFactors = F)

indvmod <- read.csv("indvmod.csv", header = T, stringsAsFactors = F)
indvmod$DOB <- DOB$DOB

indvmod$Sex <- sample(c("M", "F"), length(indvmod$Person_ID), replace = T, prob = c(0.51, 0.49)) #https://www.census.gov/quickfacts/fact/table/tippecanoecountyindiana/PST045217

indvmod1 <- indvmod[c(1:50000),]
indvmod2 <- indvmod[c(50001:100000),]
indvmod3 <- indvmod[c(100001:length(indvmod$Person_ID)),]

write.csv(indvmod1, file = "IndividualUpload1.csv", row.names = F)
write.csv(indvmod2, file = "IndividualUpload2.csv", row.names = F)
write.csv(indvmod3, file = "IndividualUpload3.csv", row.names = F)
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)