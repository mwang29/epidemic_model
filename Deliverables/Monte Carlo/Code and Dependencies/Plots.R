install.packages("RMySQL")
install.packages("RColorBrewer")
library(RMySQL)
library(RColorBrewer)
cols <- brewer.pal(8, "Pastel1")

con <- dbConnect(MySQL(),user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

Qsql<- paste("SELECT COUNT(*) AS NPeople FROM Individuals")
NPeople <-dbGetQuery(con, Qsql)

#Plots:
#public transport
Qsql<- paste("SELECT COUNT(*) AS Count FROM Household as H, Individuals AS I WHERE I.Household_ID = H.Household_ID AND PubTrans = 'Yes'")
PTransRegion<- dbGetQuery(con, Qsql)

slices<-c(NPeople[1,1]-PTransRegion[1,1],PTransRegion[1,1])
pct <- round(slices/sum(slices)*100)
lbls <-c("Personal Vehicles", "Public Transportation")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col= c(cols[1:2]))
  
#Region vs population density:
Qsql <- paste("SELECT Region_Num, Pop_Density FROM Region ORDER BY Pop_Density")
RegDen <- dbGetQuery(con, Qsql)
RegDen.freq= as.vector(rep(RegDen$Region_Num, RegDen$Pop_Density))
hist(RegDen.freq, breaks = length(RegDen.freq), ylab = "Population Density (People per Sq.mile)", xlab = "Region Number")

#School names and size:
Qsql <- paste("SELECT W.Name, COUNT(*) AS Count FROM Individuals AS I,Workplaces AS W, Works_At AS WA WHERE WA.Person_ID = I.Person_ID AND WA.Work_ID = W.Work_ID AND YEAR(I.DOB) > 2000 GROUP BY W.Work_ID  ORDER BY COUNT(*)")
Schools <- dbGetQuery(con, Qsql)
barplot(Schools$Count, main="Number of Students per School", horiz=TRUE, names.arg= as.vector(Schools$Name), las = 1, col = cols[3],)
par(mai = c(1.02,2.0,0.82,0.42))
Qsql<- paste("SELECT H.Region_Num, Count(*), FROM Household as H, Individuals AS I WHERE I.HouseID = H.Household_ID AND PubTrans = 'Yes' GROUP BY H.Region_Num ORDER BY H.Region_Num")



#Race?
#Age distribution
Qsql <- paste("SELECT YEAR(DOB) FROM Individuals")
Ages <- dbGetQuery(con, Qsql)
Ages <- as.vector(2018 - Ages$`YEAR(DOB)`)
hist(Ages, ylab = "Number of People", col = cols[4])

#Race distribution
Qsql <- paste("SELECT H.Race As Race, COUNT(*) AS Count FROM Household as H, Individuals AS I WHERE I.Household_ID = H.Household_ID GROUP BY H.Race")
Race <- dbGetQuery(con, Qsql)
Race[6,1]<-"Multirace/Other"
Race[6,2]<-Race[6,2]+Race[5,2]
Race<-Race[-5,]

slices<-as.vector(Race$Count)
pct <- round(slices/sum(slices)*100)
lbls <-as.vector(Race$Race)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col= c(cols[1:6],cols[8]))







