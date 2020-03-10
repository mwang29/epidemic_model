#install.packages("RMySQL")
library(RMySQL)

#P1<-read.csv("Diseases.csv", header = T)
#P2<-read.csv("Region.csv", header = T)
#P3<-read.csv("Schools.csv", header = T)
#P4<-read.csv("Bus_Routes.csv", header = T)
#P5<-read.csv("Homeless_Shelters.csv", header = T)
#P6<-read.csv("Stores.csv", header = T)
#P7<-read.csv("Restaurants.csv", header = T)
#P8<-read.csv("Household.csv", header = T) 
#P9<-read.csv("Workplaces.csv", header = T)
#P10<-read.csv("OldAgeHomes.csv", header = T)
#P11<-read.csv("Parks_Rec.csv", header = T)
#P12<-read.csv("Hospitals.csv", header = T)
#P13<-read.csv("Individuals.csv", header = T) 
#P14<-read.csv("Belongs_To.csv", header = T)

con <- dbConnect(MySQL(),
                 user="g1081391", password="Wif1hasnomeaning",
                 dbname="g1081391", host="mydb.ics.purdue.edu")
for ( i in 1:nrow(P1) ) {
  dbSendQuery(con,sprintf("insert into Diseases(Disease_Type, Mortality, Strength, Incubation) values('%d',	'%d',	'%d',	'%d')",
                          P1[i,1],P1[i,2], P1[i,3], P1[i,4]))
}
for ( i in 1:nrow(P2) ) {
  dbSendQuery(con,sprintf("insert into Region(Region_Num, Population, Pop_Density) values('%d',	'%d',	'%d')",
                          P2[i,1],P2[i,2], P2[i,3]))
}
for ( i in 1:nrow(P3) ) {
  dbSendQuery(con,sprintf("insert into Schools(School_Name, Enrollment, Traffic) values('%s',	'%d',	'%d')",
                          P3[i,1],P3[i,2], P3[i,3]))
}
for ( i in 1:nrow(P4) ) {
  dbSendQuery(con,sprintf("insert into Bus_Routes(Bus_ID, Traffic) values('%s',	'%d')",
                          P4[i,1],P4[i,2]))
}
for ( i in 1:nrow(P5) ) {
  dbSendQuery(con,sprintf("insert into Homeless_Shelters(Shelter_Name, Region_Num, Traffic) values('%s',	'%d',	'%d')",
                          P5[i,1],P5[i,2], P5[i,3]))
}
for ( i in 1:nrow(P6) ) {
  dbSendQuery(con,sprintf("insert into Stores(Store_ID, Store_Name, Size) values('%d',	'%s',	'%d')",
                          P6[i,1],P6[i,2], P6[i,3]))
}
for ( i in 1:nrow(P7) ) {
  dbSendQuery(con,sprintf("insert into Restaurant(R_ID, R_Name, Popularity) values('%d',	'%s',	'%d')",
                          P7[i,1],P7[i,2], P7[i,3]))
}
for ( i in 1:nrow(P8) ) {
  dbSendQuery(con,sprintf("insert into Household(Household_ID, Region_Num, Household_Type, Race, Number_People, Number_Children, Number_Old, Number_Males, Number_Females, Income, PubTrans) values('%d',	'%d', '%s','%s','%d','%d','%d','%d','%d','%s','%s')",
                          P8[i,1],P8[i,2], P8[i,3], P8[i,4],P8[i,5], P8[i,6], P8[i,7],P8[i,8], P8[i,9],P8[i,10], P8[i,11]))
}
for ( i in 1:nrow(P9) ) {
  dbSendQuery(con,sprintf("insert into Workplaces(Work_ID, Work_Name, Region_Num, Traffic) values('%s',	'%s',	'%d','%d')",
                          P9[i,1],P9[i,2], P9[i,3],P9[i,4]))
}
for ( i in 1:nrow(P10) ) {
  dbSendQuery(con,sprintf("insert into OldAgeHomes(OAH_ID, OAH_Name, Capacity, Region_Num) values('%d',	'%s', '%d','%d')",
                          P10[i,1],P10[i,2], P10[i,3], P10[i,4]))
}
for ( i in 1:nrow(P11) ) {
  dbSendQuery(con,sprintf("insert into Parks_Rec(Name, Region_Num, Traffic) values('%s',	'%d', '%d')",
                          P11[i,1],P11[i,2], P11[i,3]))
}
for ( i in 1:nrow(P12) ) {
  dbSendQuery(con,sprintf("insert into Hospitals(Hospital_ID, Hospital_Name, Region_Num, Capacity) values('%d',	'%d', '%d','%d')",
                          P12[i,1],P12[i,2], P12[i,3],P12[i,4]))
}

for ( i in 1:nrow(P13) ) {
  dbSendQuery(con,sprintf("insert into Individuals(Person_ID, Household_ID, School_Name, Work_ID, Child, OAH_ID, Age, Health_Status, Homeless_shelter, Susceptibility) values('%d',	'%d', '%s','%s','%s','%d','%d','%s','%s','%d')",
                          P13[i,1],P13[i,2], P13[i,3], P13[i,4],P13[i,5], P13[i,6], P13[i,7],P13[i,8], P13[i,9],P13[i,10]))
}
for ( i in 1:nrow(P14) ) {
  dbSendQuery(con,sprintf("insert into Belongs_To(Bus_ID,Region_Num) values('%s',	'%d')",
                          P14[i,1],P14[i,2]))
}
#Exiting
on.exit(dbDisconnect(MySQL(),
                     user="g1081391", password="Wif1hasnomeaning",
                     dbname="g1081391", host="mydb.ics.purdue.edu"))
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)