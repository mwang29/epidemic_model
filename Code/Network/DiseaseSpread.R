library(RMySQL)
library(partitions)
########################################### Population and Database initialization ###################################

indivData <- read.csv("Individuals.csv", header = T, stringsAsFactors = F)
pop <- data.frame(ID = c(1:nrow(indivData))) #Data frame of individuals as rows
#Database Connection
con <- dbConnect(MySQL(),user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

################################################# Queries ###############################################

# Select a poisson number of random Person_ID who belong to same ____ as our infected indiv 
QWork <- "SELECT I.Person_ID AS Person_ID
FROM Individuals I, Works_At W 
WHERE I.Person_ID = W.Person_ID AND W.Work_ID = (SELECT W.Work_ID
FROM Individuals I, Works_At W
WHERE I.Work_ID = W.Work_ID
AND I.Person_ID = INFECTED_ID_HERE)
ORDER BY RAND()
LIMIT POISSON_GOES_HERE;"

QCommunity <- "SELECT I.Person_ID AS Person_ID 
FROM Individuals I, Household H 
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = (SELECT H.Region_Num
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID
AND I.Person_ID = INFECTED_ID_HERE)
ORDER BY RAND()
LIMIT POISSON_GOES_HERE;"

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
WHERE I.Household_ID = H.Household_ID AND H.Region_Num = B.Region_Num AND H.PubTrans = 'YES' AND I.Person_ID = INFECTEDINDV)
ORDER BY RAND()
LIMIT POISSON_GOES_HERE;"

QHousehold <- "SELECT I.Person_ID AS Person_ID
FROM Individuals I
WHERE I.Household_ID = (SELECT I.Household_ID
FROM Individuals I
WHERE I.Person_ID = 1335)
ORDER BY RAND();"

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

########################################### Parameter intialization ###############################

duration <- 100 #number of days for simulation
meanContactsDay <- 13.4 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2270306/
meanDensity <- dbGetQuery(con, QDensity) #mean density of all regions

#User inputs
severity <- 3 #Severity of disease
infectedindvId <- c(232,2583)  #Initial infected individuals
recoveryRate <- 0.3 <- #Recovery Rate
#Other parameters

#Normalized to White multipliers for each race to infectious disease (CDC)
whiteProp <- 3.0 
raceWeight <- c(8.1/whiteProp, 10.9/whiteProp, 8.2/whiteProp, 4.1/whiteProp,4.1/whiteProp) #asian, black, hispanic, amerindian, multirace

povWeight <- 1.5 #No exact multiplier can be found, but two biosecurity papers suggest strong correlations between poverty and influenza

#Age weight: 0-4, 5-18, 19-64, > 65
ageWeight <- c(0.00231, 0.0023, 0.00127, 0.00324) / min(c(0.00231, 0.0023, 0.00127, 0.00324))
#Data taken from summing the columns of the transition probability matrix from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/


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

#EXAMPLE OF 
#Gets multiplier for region density for community contacts
getDenMultiplier <- function(inf_ID, meanDensity){
  density <- dbGetQuery(con, cat(paste0(QDensity, inf_ID, ";", collapse = "\n"))) #concatenates inf_ID with query
  multiplier <- density/meanDensity
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
}
###########################################Disease spread model#########################################

for (day in 2:duration){ #time step loop
  for (inf_ID in inf_IDs){ #infected individuals loop
    
    #Acquire contacted individuals + weight values
    contactVec <- contacts() #function to determine poisson total contacts, split into work, bus, community.
    
    work_edges <- getWork_edges(inf_ID, contactVec[2])#Query for work contacts (Poisson) - Returns vector of individual ID
    work_edges$weight <- 8 #Assign time-based weight
    comm_edges <- getComm_edges(inf_ID, contactVec[3])#Query for community contacts (Poisson) 
    comm_edges$weight <- 4.265 * getDenMultiplier(inf_ID, meanDensity)#Assign weight based on avg time (Bureau of Labor Stats) and Pop Density of Region
    bus_edges <- getBus_edges(inf_ID, contactVec[4]) #Query for bus contacts (Poisson)
    bus_edges$weight <- 0.75 #Assign weight based on mean bus commute
    house_edges <- getHouse_edges(inf_ID) #Query for household contacts (All individuals) ()
    house_edges$weight <- 5.065 #Assign weight based on BLS data
    
    all_edges <- rbind(work_edges, house_edges, comm_edges, bus_edges) #Combine across individuals
    simplify(all_edges, edge.attr.comb=list(weight="sum")) #Sum weights over repeated edges
    
    #Individual-specific multipliers
    all_edges <- raceMult(all_edges) #multiplier function for race
    all_edges <- ageMult(all_edges) #multiplier function for age
    
    #Do stuff to them
    sickID <- gets_sick(all_edges, rate) #returns IDs of all people who newly get sick
    #gets_sick function (in: data frame of contacted individID and weights, infection rate | out: individID of infected) 
    pop$inf_count[pop$ID = inf_ID] <- pop$inf_count[pop$ID = inf_ID] + nrow(sickID) #Updates # people this individual infected
    nextInf_IDs <- union(nextInf_IDs, sickID) #compiles those who will be sick tomorrow (no repeats)
  }
  #End of Day i! Who recovered? Who died? 
  recov_ID <- recovered(inf_IDs)#Recovery/Death function (in: inf_IDs| out: inf_IDs vector with recovered/dead changed)
  dead_ID <- dead(inf_IDs) 
  inf_Ids <- nextInf_Ids[!(nextInf_Ids %in% c(recov_ID, dead_ID))] #remove those who recovered or died from infected list, forms group of infected for next day
  pop <- updateHealth(pop, inf_Ids, recov_ID, dead_ID, day) #updates pop data frame column for the day 
  nextInf_Ids <- inf_Ids #New live-updating infected group for next day
  Infect_Recov_Dead_Vec <- IRD(pop,day) #count infected, recovered, dead for the day in pop 
}

#nextInf_Ids: real-time updating ID vector of currently sick 
#inf_Ids: ID vector of sick at start of day (have chance of recover/die at end of day)
#sickID: ID vector of those who get sick from one inf_ID iteration
#inf_Id : infected individual lcv

#Disconnect from DB
all_cons <- dbListConnections(MySQL())
for (con in all_cons)
  dbDisconnect(con)
