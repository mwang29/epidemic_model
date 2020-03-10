library(RMySQL)
P <- read.csv("Works_At.csv", header = T)
con <- dbConnect(MySQL(),
                 user="g1081391", password="Wif1hasnomeaning",
                 dbname="g1081391", host="mydb.ics.purdue.edu")

for ( i in 1:nrow(P) ) {
  dbSendQuery(con,sprintf("insert into Works_At (Person_ID, Work_ID) values('%d','%d')",
                               P[i,1],P[i,2]))
}

for (con in all_cons)
  dbDisconnect(con)
