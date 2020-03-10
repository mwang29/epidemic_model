P <- read.csv("Individuals.csv", header = T)
P2 <- read.csv("Household.csv", header = T)
con <- dbConnect(MySQL(),
                 user="g1081391", password="W1fihasnomeaning",
                 dbname="g1081391", host="mydb.ics.purdue.edu")
for ( i in 1:nrow(P) ) {
  dbSendQuery(g1081391,sprintf("insert into individuals(Person_ID, Household_ID, School_Name, Work_ID, OLH_ID, Age, Health_Status, Homeless_Shelter, Susceptibility) values('%d','%d','%s','%d','%d','%d','%s','%d')",
                               P[i,1],P[i,2], P[i,5],P[i,7],P[i,8],P[i,3],P[i,6],P[i,8],P[i,9]))
}

for ( i in 1:nrow(P2) ) {
  dbSendQuery(g1081391,sprintf("insert into Household(Household_ID, Region_Num, Household_Type, Race, Number_People, Number_Children, Number_Old, 
                               Number_Males, Number_Females, Income, PubTrans) values('%d','%d','%s','%s','%d','%d','%d','%d', '%d', '%d', '%s')",
                               P2[i,1],P2[i,2], P2[i,3],P2[i,7],P2[i,6],P2[i,5],P2[i,10],P2[i,8],P2[i,9], P2[i,4], P2[i,11]))
}

#Exiting
on.exit(dbDisconnect(MySQL(),
                     user="g1081391", password="W1fihasnomeaning",
                     dbname="g1081391", host="mydb.ics.purdue.edu"))
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)