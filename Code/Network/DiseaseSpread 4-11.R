library(RMySQL)
library(partitions)
library(animation)
########################################### Population and Database initialization ###################################

indivData <- read.csv("Individuals.csv", header = T, stringsAsFactors = F)
 #Data frame of individuals as rows
#Database Connection
pop <- data.frame(ID = c(1:length(indivData$Person_ID)))
pop$Health_Status <- 0 #everyone healthy
con <- dbConnect(MySQL(),user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

################################################# Queries ###############################################

QEnd <- ") ORDER BY RAND() LIMIT" #ending query fragment to pull out only poisson # of contacts

# Select a poisson number of random Person_ID who belong to same ____ as our infected indiv 
QWork <- "SELECT I.Person_ID AS Person_ID
FROM Individuals I, Works_At W 
WHERE I.Person_ID = W.Person_ID AND W.Work_ID = (SELECT W.Work_ID
FROM Individuals I, Works_At W
WHERE I.Work_ID = W.Work_ID
AND I.Person_ID = "

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

#Multiplier Queries (Attributes of Individuals)

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
WHERE I.Household_ID = H.Household_ID AND H.Income = 'POV';"

Qinfant <- "SELECT I.Person_ID AS infantID
FROM Individuals I
WHERE I.Age <= 4;"

Qchild <- "SELECT I.Person_ID AS childID
FROM Individuals I
WHERE I.Age BETWEEN 5 AND 18;"

Qadult <- "SELECT I.Person_ID AS adultID
FROM Individuals I
WHERE I.Age BETWEEN 19 AND 64;"

Qsenior <- "SELECT I.Person_ID AS seniorID
FROM Individuals I
WHERE I.Age > 64;"

#Decide who to infect! and how many!
Qinf_IDs <- "SELECT I.Person_ID 
FROM Individuals I 
WHERE ___
ORDER BY RAND()
LIMIT ___"
########################################### Parameter intialization ###############################

duration <- 100 #number of days for simulation
meanContactsDay <- 13.4 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2270306/
meanDensity <- dbGetQuery(con, QDensity) #mean density of all regions

#User inputs
severity <- 3 #Severity of disease 1= Influenza, 2 = Smallpox, 3 = measels, 4 = Ebola
inf_IDs <- dbGetQuery(con, Qinf_IDs)  #Initial infected individuals ID by query
k_number <- #k nodes percentage to identify
  
#variable declarations+ initialization:
totalPeople <- length(pop$ID) #total number of people
infected <- rep(length(inf_IDs),1)
recovered <- rep(0,1)
dead <- rep(0,1)
susceptible <- rep((length(totalPeople) - length(inf_IDs)),1) 

#Normalized to White multipliers for each race to infectious disease (CDC)
whiteProp <- 3.0 
raceWeight <- c(8.1/whiteProp, 10.9/whiteProp, 8.2/whiteProp, 4.1/whiteProp,4.1/whiteProp) #asian, black, hispanic, amerindian, multirace
raceWeightDeath <- #unnecessary (i think)
raceWeightRecover <- #unnecessary (i think)?

povWeight <- 1.5 #No exact multiplier can be found, but two biosecurity papers suggest strong correlations between poverty and influenza

#Age weight: 0-4, 5-18, 19-64, > 65
ageWeight <- c(0.0005775, 0.000575, 0.0003175, 0.000855)#transmission weight based on age and seasonal influenza
#Data taken from summing the columns of the transition probability matrix from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/
deathRate <- c(0.00005, 0.00003, 0.001,0.027) #for each age group: 0-4, 5-18, 19-64, > 65 for influenza
recoveryRate <- 1/7 #rate = 1/ average time spent in infected class

#Produces logical vectors of Individual IDs of different demographics
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

################################################ Functions ###########################################


#Determines number of contacts per person per day, and split up into work, bus community
contacts <- function(){
  nContacts <- rpois(1,meanContactsDay)
  x <- rmultinom(n = 1, size = nContacts, prob = rep(1/3, 3))
  x <- colsums(x)
  return(c(nContacts, x[1], x[2], x[3]))
}

#Returns queried edges by concatenating strings
getEdges <- function(inf_ID, Query, QueryEnd, numContacts){
  edges <- dbGetQuery(con, cat(paste0(Query, inf_ID, QueryEnd, numContacts, ";", collapse = "\n"))) #concatenates inf_ID with query
  return(edges)
}

#Gets multiplier for region density for community contacts
getDenMultiplier <- function(inf_ID, QDensity, meanDensity){
  density <- dbGetQuery(con, cat(paste0(QDensity, inf_ID, ";", collapse = "\n"))) #concatenates inf_ID with query
  multiplier <- density/meanDensity
  return(multiplier)
}

raceMult <- function(all_edges, asianID, blackID, hispanicID, amerindianID, multiraceID, raceWeight){
  all_edges$Weight[all_edges$ID %in% asianID] <-  all_edges$Weight[all_edges$ID %in% asianID] * raceWeight[1]
  all_edges$Weight[all_edges$ID %in% blackID] <-  all_edges$Weight[all_edges$ID %in% blackID] * raceWeight[2]
  all_edges$Weight[all_edges$ID %in% hispanicID] <-  all_edges$Weight[all_edges$ID %in% hispanicID] * raceWeight[3]
  all_edges$Weight[all_edges$ID %in% amerindianID] <-  all_edges$Weight[all_edges$ID %in% amerindianID] * raceWeight[4]
  all_edges$Weight[all_edges$ID %in% multiraceID] <-  all_edges$Weight[all_edges$ID %in% multiraceID] * raceWeight[5]
  return(all_edges)
}

ageMult <- function(all_edges, infantID, childID, adultID, seniorID, ageWeight){
  all_edges$Weight[all_edges$ID %in% infantID] <- all_edges$Weight[all_edges$ID %in% infantID] * ageWeight[1]
  all_edges$Weight[all_edges$ID %in% childID] <- all_edges$Weight[all_edges$ID %in% childID] * ageWeight[2]
  all_edges$Weight[all_edges$ID %in% adultID] <- all_edges$Weight[all_edges$ID %in% adultID] * ageWeight[3]
  all_edges$Weight[all_edges$ID %in% seniorID] <- all_edges$Weight[all_edges$ID %in% seniorID] * ageWeight[4]
  return(all_edges)
}

gets_sick <- function(all_edges, severity){
  if (severity == 2){all_edges$weight <- all_edges$weight*2.4} #smallpox https://www.theguardian.com/news/datablog/ng-interactive/2014/oct/15/visualised-how-ebola-compares-to-other-infectious-diseases
  else if (severity == 3){all_edges$weight <- all_edges$weight*6} #measeles
  prob_trans <- 1 - exp(-all_edges$weight) #probability of transmission based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/
  all_edges$weight <- raceMult(all_edges,asianID, blackID, hispanicID, amerindianID, multiraceID, raceWeight) #multiplier function for race
  randNo <- runif(1,0,1)
  sickID <- all_edges$ID[which(all_edges$weight < randNo) ,]
  return(sickID)
}

dead <- function(inf_IDs, severity, all_edges, deathRate){
  deathRate <- 1 #base
  if (severity == 2){deathRate <- deathRate*150} #smallpox
  else if (severity == 3){deathRate <- deathRate*3} #Measels
  else if (severity == 4){deathRate <- deathRate*500} #Ebola
  inf_people <- all_edges[!(all_edges$ID %in% inf_IDs)]
  inf_people$weight <- deathRate
  inf_people <- raceMult(inf_people, asianID, blackID, hispanicID, amerindianID, multiraceID, raceWeightDeath)
  randNo <- runif(length(inf_IDs),0,1)
  dead_ID <- inf_people$ID[which(inf_people$weight < randNo) ,] #need to fix
  return(dead_ID)
}

recovered <- function(inf_IDs, severity, all_edges, recoveryRate, dead_ID){ #idk if this stuff is in the scope...
  if (severity == 2){RecoveryRate <- RecoveryRate*150} #smallpox ? data
  else if (severity == 3){RecoveryRate <- RecoveryRate*3} #Measels ? Data
  else if (severity == 4){RecoveryRate <- RecoveryRate*500} #Ebola ? data
  inf_people <- all_edges[!(all_edges$ID %in% inf_IDs)]
  inf_People$weight <- recoveryRate
  randNo <- runif(length(inf_IDs),0,1)
  randNo <- runif(1,0,1)
  recov_ID <- all_edges$ID[which(all_edges$weight < randNo) ,] #need to fix
  recov_ID <- recov_IDs[!(recov_ID %in% dead_ID)]
  return(recov_ID)
}

###########################################Disease spread model#########################################

for (day in 2:duration){ #time step loop, starting on day 2
  for (inf_ID in inf_IDs){ #infected individuals loop
    
    #Acquire contacted individuals + weight values
    contactVec <- contacts() #function to determine poisson random variable total contacts, split into work, bus, community.
    work_edges <- getEdges(inf_ID, QWork, QEnd, contactVec[2])#Query for work contacts (Poisson) - Returns vector of individual ID
    work_edges$weight <- 480 #Assign time-based weight
    comm_edges <- getEdges(inf_ID, QCommunity, QEnd, contactVec[3])#Query for community contacts (Poisson) 
    comm_edges$weight <- 255.9 *getDenMultiplier(inf_ID, meanDensity) #Assign weight based on avg time (Bureau of Labor Stats) and Pop Density of Region
    bus_edges <- getEdges(inf_ID, QBus, QEnd, contactVec[4]) #Query for bus contacts (Poisson)
    bus_edges$weight <- 45 #Assign weight based on mean bus commute
    house_edges <- getEdges(inf_ID, QHousehold, QEnd, contactVec[1]) #Query for household contacts (Limit is all individuals) ()
    house_edges$weight <- 303.9 #Assign weight based on BLS data
    all_edges <- rbind(work_edges, house_edges, comm_edges, bus_edges) #Combine across individuals
    simplify(all_edges, edge.attr.comb=list(weight="sum")) #Sum weights over repeated edges
    
    #Individual-specific multipliers
    all_edges <- ageMult(all_edges, infantID, childID, adultID, seniorID, ageWeight) #multiplying time spent in minutes by transmission rates based on age
    
    #Do stuff to them
    sickID <- gets_sick(all_edges, severity) #returns IDs of all people who newly get sick
    #gets_sick function (in: data frame of contacted individID and weights, infection rate | out: individID of infected) 
    pop$inf_count[pop$ID = inf_ID] <- pop$inf_count[pop$ID = inf_ID] + nrow(sickID) #Updates # people this individual infected
    nextInf_IDs <- union(nextInf_IDs, sickID) #compiles those who will be sick tomorrow (no repeats)
  }
  #End of Day i! Who recovered? Who died? 
  
  dead_ID <- dead(inf_IDs, severity, all_edges) 
  recov_ID <- recovered(inf_IDs, severity, all_edges)#Recovery/Death function (in: inf_IDs| out: inf_IDs vector with recovered/dead changed)
  inf_IDs <- nextInf_IDs[!(nextInf_IDs %in% c(recov_ID, dead_ID))] #remove those who recovered or died from infected list, forms group of infected for next day
  pop$Health_Status[pop$ID %in% inf_IDs] <- 1
  pop$Health_Status[pop$ID %in% recov_ID] <- 2
  pop$Health_Status[pop$ID %in% dead_ID] <- 3
  
  infected[day] <- nrow(inf_IDs)
  recovered[day] <- nrow(recov_ID) + recovered[day-1]
  dead[day] <- nrow(dead_ID)
  susceptible[day] <- totalPeople - infected[day] - recovered[day] - dead[day]
  nextInf_IDs <- inf_IDs #New live-updating infected group for next day
}

#nextInf_Ids: real-time updating ID vector of currently sick 
#inf_Ids: ID vector of sick at start of day (have chance of recover/die at end of day)
#sickID: ID vector of those who get sick from one inf_ID iteration
#inf_Id : infected individual lcv
plot(0,0,xlim = c(0:duration + 1),ylim = c(0:totalPeople, type = "n"))
lines(1:duration, infected, col = "red")
lines(1:duration, recovered, col = "green")
lines(1:duration, dead, col = "black")
lines(1:duration, susceptible, col = "blue")

#Disconnect from DB
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)
