library(pbapply)
library(RMySQL)

#This R Script compiles all contacts each individual may have, the associated base probabilities of transmission,
#and the type of contact made into a large list of 140,632 data frames (1 per individual) as .rda.

#Connect to database
con <- dbConnect(MySQL(), user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

#****************************************QUERIES************************************
QEnd <- ") ORDER BY RAND() LIMIT " #ending query fragment to pull out only poisson # of contacts

# Select a poisson number of random Person_ID who belong to same ____ as our infected indiv 
QWork <- "SELECT I.Person_ID AS Person_ID
FROM Individuals I, Works_At W 
WHERE I.Person_ID = W.Person_ID AND W.Work_ID = (SELECT W.Work_ID
FROM Works_At W
WHERE W.Person_ID = "
      
QCommunity <- "SELECT I.Person_ID AS Person_ID 
FROM Individuals I, Household H 
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = (SELECT H.Region_Num
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID
AND I.Person_ID = "
      
QDensity <- "SELECT R.Pop_Density
FROM Individuals I, Household H, Region R
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = R.Region_Num
AND I.Person_ID = "
      
QAvgDensity <- "SELECT AVG(Pop_Density)
FROM Region;"
      
QBus <- "SELECT DISTINCT I.Person_ID AS Person_ID
FROM Individuals I, Household H, Bus_Routes B
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = B.Region_Num AND H.PubTrans = 'YES' AND B.Bus_ID IN (SELECT DISTINCT B.Bus_ID
FROM Individuals I, Household H, Bus_Routes B
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = B.Region_Num AND H.PubTrans = 'YES' AND I.Person_ID = "
      
QHousehold <- "SELECT I.Person_ID AS Person_ID
FROM Individuals I
WHERE I.Household_ID = (SELECT I.Household_ID
FROM Individuals I
WHERE I.Person_ID = "

QWhite <- "SELECT I.Person_ID AS WhiteID
      FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'White';"

QAsian <- "SELECT I.Person_ID AS AsianID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Asian';"

QBlack <- "SELECT I.Person_ID AS BlackID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Black';"

QHispanic <- "SELECT I.Person_ID AS HispanicID
      FROM Individuals I, Household H
      WHERE I.Household_ID = H.Household_ID AND H.Race = 'Hispanic';"

QMultirace <- "SELECT I.Person_ID AS MultiraceID
      FROM Individuals I, Household H
      WHERE I.Household_ID = H.Household_ID AND H.Race = 'Multirace';"

QAmerIndian <- "SELECT I.Person_ID AS AmerIndianID
      FROM Individuals I, Household H
      WHERE I.Household_ID = H.Household_ID AND H.Race = 'Amer Indian';"

QPov <- "SELECT I.Person_ID AS povID
      FROM Individuals I, Household H
      WHERE I.Household_ID = H.Household_ID AND H.Income < 10000;"

Qinfant <- "SELECT I.Person_ID AS infantID
      FROM Individuals I
      WHERE FLOOR(DATEDIFF(CURDATE(),I.DOB) / 365.25) <= 4;"

Qchild <- "SELECT I.Person_ID AS childID
      FROM Individuals I
      WHERE FLOOR(DATEDIFF(CURDATE(),I.DOB) / 365.25) BETWEEN 5 AND 18;"

Qadult <- "SELECT I.Person_ID AS adultID
      FROM Individuals I
      WHERE FLOOR(DATEDIFF(CURDATE(),I.DOB) / 365.25) BETWEEN 19 AND 64;"

Qsenior <- "SELECT I.Person_ID AS seniorID
      FROM Individuals I
      WHERE FLOOR(DATEDIFF(CURDATE(),I.DOB) / 365.25) > 64;;"
#***************************************Functions*********************************************

raceMult <- function(all_edges, asianID, blackID, hispanicID, amerindianID, multiraceID, raceWeight){
  all_edges$Weight[all_edges$Person_ID %in% asianID$AsianID] <-  all_edges$Weight[all_edges$Person_ID %in% asianID$AsianID] * raceWeight[1]
  all_edges$Weight[all_edges$Person_ID %in% blackID$BlackID] <-  all_edges$Weight[all_edges$Person_ID %in% blackID$BlackID] * raceWeight[2]
  all_edges$Weight[all_edges$Person_ID %in% hispanicID$HispanicID] <-  all_edges$Weight[all_edges$Person_ID %in% hispanicID$HispanicID] * raceWeight[3]
  all_edges$Weight[all_edges$Person_ID %in% amerindianID$AmerIndianID] <-  all_edges$Weight[all_edges$Person_ID %in% amerindianID$AmerIndianID] * raceWeight[4]
  all_edges$Weight[all_edges$Person_ID %in% multiraceID$MultiraceID] <-  all_edges$Weight[all_edges$Person_ID %in% multiraceID$MultiraceID] * raceWeight[5]
  return(all_edges)
}

ageMult <- function(all_edges, infantID, childID, adultID, seniorID, ageWeight){
  all_edges$Weight[all_edges$Person_ID %in% infantID$infantID] <- all_edges$Weight[all_edges$Person_ID %in% infantID$infantID] * ageWeight[1]
  all_edges$Weight[all_edges$Person_ID %in% childID$childID] <- all_edges$Weight[all_edges$Person_ID %in% childID$childID] * ageWeight[2]
  all_edges$Weight[all_edges$Person_ID %in% adultID$adultID] <- all_edges$Weight[all_edges$Person_ID %in% adultID$adultID] * ageWeight[3]
  all_edges$Weight[all_edges$Person_ID %in% seniorID$seniorID] <- all_edges$Weight[all_edges$Person_ID %in% seniorID$seniorID] * ageWeight[4]
  return(all_edges)
}


#Returns queried edges by concatenating strings
getEdges <- function(inf_ID, Query, QueryEnd, numContacts){
  SQL <- paste0(Query, inf_ID, QueryEnd, numContacts, ";")
  SQL <- gsub("[\n]", " ", SQL)
  edges <- dbGetQuery(con, SQL) #concatenates inf_ID with query
  return(edges)
}

#Gets multiplier for region density for community contacts
getDenMultiplier <- function(inf_ID, QDensity, meanDensity){
  SQL <- paste0(QDensity, inf_ID, ";")
  SQL <- gsub("[\n]", " ", SQL)
  density <- dbGetQuery(con, SQL) #concatenates inf_ID with query
  multiplier <- density/meanDensity
  return(as.numeric(multiplier))
}

#Randomize dataframe
randomize <- function(df)
{
  df <- df[sample(nrow(df)),]
}

#********************************************************************************************
meanDensity <- dbGetQuery(con, QAvgDensity)
listDf <- list()
numID <- dbGetQuery(con, "SELECT COUNT(*) FROM Individuals")

#Produces vectors the Individual IDs of various demographics
whiteID <- dbGetQuery(con, QWhite)
asianID <- dbGetQuery(con, QAsian)
blackID <- dbGetQuery(con, QBlack)
hispanicID <- dbGetQuery(con, QHispanic)
multiraceID <- dbGetQuery(con, QMultirace)
amerindianID <- dbGetQuery(con, QAmerIndian)
povID <- dbGetQuery(con, QPov)
infantID <- dbGetQuery(con, Qinfant)
childID <- dbGetQuery(con, Qchild)
adultID <- dbGetQuery(con, Qadult)
seniorID <- dbGetQuery(con, Qsenior)

#Age weight: 0-4, 5-18, 19-64, > 65
ageWeight <- c(0.0005775, 0.000575, 0.0003175, 0.000855)#transmission weight based on age and seasonal influenza

for (inf_ID in c(1:numID$`COUNT(*)`)){


work_edges <- getEdges(inf_ID, QWork, QEnd, 100)#Query for work contacts (Poisson) - Returns vector of individual ID
work_edges$Weight <- rep(480,nrow(work_edges)) #Assign time-based weight (8 hr workday * 60 mins)
work_edges$Type <- rep("W",nrow(work_edges))

#Determine if school or no
work_ID <- dbGetQuery(con, sprintf("SELECT Work_ID FROM Works_At WHERE Person_ID = %d",inf_ID))  
if(work_ID <= 16){work_edges$Type <- rep("S", nrow(work_edges))} #Assign edge classified as a school

bus_edges <- getEdges(inf_ID, QBus, QEnd, 100) #Query for bus contacts (Poisson)
bus_edges$Weight <- rep(45,nrow(bus_edges)) #Assign weight based on mean bus commute (45 mins)
bus_edges$Type <- rep("B",nrow(bus_edges))

comm_edges <- getEdges(inf_ID, QCommunity, QEnd, 100)#Query for community contacts (Poisson) 
comm_edges$Weight <- rep(255.9, nrow(comm_edges)) * getDenMultiplier(inf_ID, QDensity, meanDensity) #Assign weight based on avg time (Bureau of Labor Stats) and Pop Density of Region
comm_edges$Type <- rep("C",nrow(comm_edges))
                                                                                                                          
house_edges <- getEdges(inf_ID, QHousehold, QEnd, 100) #Query for household contacts (Limit is all individuals) ()
house_edges = house_edges[house_edges != inf_ID,] #Don't include him/herself
house_edges <- data.frame(Person_ID = house_edges) 
house_edges$Weight <- rep(303.9, nrow(house_edges)) #Assign weight based on BLS data (303.9 mins/day)
house_edges$Type <- rep("H",nrow(house_edges))

merge1 <- merge(work_edges, house_edges, all = T) #Combine across individuals
merge2 <- merge(merge1, bus_edges, all = T)
all_edges <- merge(merge2, comm_edges, all = T) #merge all edges together without repeats

#Individual-specific multipliers
all_edges <- ageMult(all_edges, infantID, childID, adultID, seniorID, ageWeight) #multiplying time spent in minutes
listDf[[length(listDf)+1]] <- all_edges
cat(inf_ID,"\n")
}

#Factor in race of the IDs
listDf <- pblapply(listDf, raceMult, asianID, blackID, hispanicID, amerindianID, multiraceID, raceWeight) #raceMult function
listDf <- pblapply(listDf, randomize) #random order
save(listDf, file = "listDf_Final2.rda")
