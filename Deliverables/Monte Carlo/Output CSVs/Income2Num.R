library(RMySQL)
con <- dbConnect(MySQL(), user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

df <- dbGetQuery(con, "SELECT Income FROM Household")
df2 <- data.frame(Income = c(1:length(df$Income)))
df2$Income[df$Income == "POV"] <- as.numeric(floor(rnorm(length(df$Income[df$Income == "POV"]), 10000, 2000)))
df2$Income[df$Income == "NO"] <- as.numeric(floor(rnorm(length(df$Income[df$Income == "NO"]), 44734, 10000))) #http://campuspress.yale.edu/sabrinacales/tag/median-household-income/


for ( i in 1:nrow(df2) ) {
  dbSendQuery(con,sprintf("insert into Household (Income) values('%d')",
                          df2[i,1]))
}

houseData <- read.csv("Household.csv", header = T, stringsAsFactors = F)
houseData$Income <- df2$Income 
write.csv(houseData, file = "HouseholdUpload.csv", row.names = F)
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)