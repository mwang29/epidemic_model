library(RMySQL)
library(DBI)
library(partitions)
library(igraph)
library(pbapply)
########################################### Population and Database initialization ###################################

indivData <- read.csv("Individual.csv", header = T, stringsAsFactors = F)
 #Data frame of individuals as rows
#Database Connection
con <- dbConnect(MySQL(), user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

################################################# Queries ###############################################

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

duration <- 50 #number of days for simulation
meanContactsDay <- 13.4 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2270306/
meanDensity <- dbGetQuery(con, QAvgDensity) #mean density of all regions

##############################################User inputs##############################################
diseaseType <- 1 #Enter type of disease 1= Influenza, 2 = Smallpox, 3 = measels, 4 = Ebola
inf_IDs <- c(58, 2939) #OPTINAL: REPLACE WITH dbGetQuery(con, Qinf_IDs)  #Initial infected individuals ID by query
pop <- data.frame(ID = c(1:length(indivData$Person_ID))) #individual IDs as a column
pop$health_status[pop$ID %in% inf_IDs] <- 1 #initial infected in health_status
kPct <- .15#k nodes percentage to identify

#variable declarations+ initialization:
totalPeople <- length(pop$ID) #total number of people
numInfected <- rep(length(inf_IDs),1)
numRecovered <- rep(0,1)
numDead <- rep(0,1)
susceptible <- rep((totalPeople - length(inf_IDs)),1) 
nextInf_IDs <- vector()
cumDead_ID <- vector()
cumRecov_ID <- vector()
totalEdges <- data.frame(ID1 = inf_IDs, ID2 = NA) #initialize for all contacts in sim
#Disease Rate
avgMult <- mean(c(1,1,2.4,6)) #taken from plot of disease contagiousness
contagious <- 1/avgMult
ifelse(diseaseType == 2, contagious <- 2.4/avgMult, 
       ifelse(diseaseType == 3, contagious <- 6/avgMult, contagious <- 1/avgMult)) #contagious multiplier for disease type

#Normalized to White multipliers for each race to infectious disease (CDC)
raceMean <- mean(c(3.0,8.1,10.9,8.2,4.1,100))
raceWeight <- c(8.1/raceMean, 10.9/raceMean, 8.2/raceMean, 4.1/raceMean,4.1/raceMean) #asian, black, hispanic, amerindian, multirace

povWeight <- 1.5 #No exact multiplier can be found, but two biosecurity papers suggest strong correlations between poverty and influenza

#Age weight: 0-4, 5-18, 19-64, > 65
ageWeight <- c(0.0005775, 0.000575, 0.0003175, 0.000855)#transmission weight based on age and seasonal influenza
#Data taken from summing the columns of the transition probability matrix from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/
deathRate <- c(0.00005, 0.00003, 0.001,0.027) #for each age group: 0-4, 5-18, 19-64, > 65 for influenza
RecoveryRate <- 1/6 #rate = 1/ average time spent in infected class

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

#Population characteristics into pop dataframe
pop$Race[pop$ID %in% whiteID$WhiteID] <- "White"
pop$Race[pop$ID %in% asianID$AsianID] <- "Asian"
pop$Race[pop$ID %in% blackID$BlackID] <- "Black"
pop$Race[pop$ID %in% hispanicID$HispanicID] <- "Hispanic"
pop$Race[pop$ID %in% multiraceID$MultiraceID] <- "Multirace"
pop$Race[pop$ID %in% amerindianID$AmerIndianID] <- "American Indian"
pop$Race[pop$ID %in% asianID$AsianID] <- "Asian"
pop$Age <- dbGetQuery(con, "SELECT AGE FROM Individuals;")
pop$Income[pop$ID %in% povID] <- "Poverty"
pop$Income[!(pop$ID %in% povID)] <- "No Poverty"
#pop$Region <- dbGetQuery(con, "SELECT H.Region_Num FROM Household H, Individuals I WHERE I.Household_ID = H.Household_ID;") FIX THIS REGION ORDERING
pop$health_status <- 0 #everyone healthy
pop$inf_count <- 0 #0 people initially infected
pop$k_node <- 0 #everyone not k_node
################################################ Functions ###########################################
#Disease factor function
f <- function(df, contagious){
  within(df, Weight <- Weight * (contagious)) ## CHANGE DISEASE MULTIPLIER HERE
}

#randomize order of contacts
randomize <- function(df)
{
  df <- df[sample(nrow(df)),]
}

#number of contacts shortened to 50
shorten <- function(df)
{
  df <- df[c(1:50),]
}

#Determines number of contacts per person per day, and split up into work, bus community
contacts <- function(meanContactsDay){
  nContacts <- rpois(1,meanContactsDay)
  if(nContacts >50 ){nContacts = 50} 
  else if (nContacts == 0){nContacts = 1}
  return(nContacts)
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


Weight2Prob <- function(all_edges){
  all_edges$Weight <- 1 - exp(-all_edges$Weight)
  return(all_edges)
}

gets_sick <- function(all_edges){
  randNo <- runif(length(all_edges$Weight),0,1)
  sickID <- all_edges$Person_ID[which(randNo < all_edges$Weight)]
  return(sickID)
}

dead <- function(inf_IDs, diseaseType, infantID, childID, adultID, seniorID, deathRate){
  diseaseWeight <- 1 #base
  if (diseaseType == 2){diseaseWeight <- 150} #smallpox
  else if (diseaseType == 3){diseaseWeight <- 3} #Measels
  else if (diseaseType == 4){diseaseWeight <- 500} #Ebola
  inf <- data.frame(Person_ID = inf_IDs, Weight = diseaseWeight)
  temp <- ageMult(inf, infantID, childID, adultID, seniorID, deathRate) 
  inf$Weight <- temp$Weight #put updated weight onto inf_people
  randNo <- runif(length(inf_IDs),0,1)
  dead_ID <- inf$Person_ID[which(randNo < inf$Weight)]
  return(dead_ID)
}

recovered <- function(inf_IDs, diseaseType, RecoveryRate, dead_ID){ 
  if (diseaseType == 2){RecoveryRate <- RecoveryRate*(6/20)} #smallpox #See project report citations
  else if (diseaseType == 3){RecoveryRate <- RecoveryRate*(6/8)} #Measels 
  else if (diseaseType == 4){RecoveryRate <- RecoveryRate*(6/5)} #Ebola ? data
  inf <- data.frame(Person_ID = inf_IDs, Weight = RecoveryRate)
  randNo <- runif(length(inf_IDs),0,1)
  recov_ID <- inf$Person_ID[which(randNo < inf$Weight)]
  recov_ID <- recov_ID[!(recov_ID %in% dead_ID)]
  return(recov_ID)
}
#
###########################################Disease spread model#########################################
#Loaded edge list of POSSIBLE contacts per person precalculated in EdgelistRetrieve.R
load("listDf2.rda") #Loads into object "listDf"

listDf2 <- pblapply(listDf, f, contagious) #factor in disease type multiplier
listDf2 <- pblapply(listDf2, Weight2Prob) #Converts weight to probability with 1-exp(-time)
samp <- list()
#List of sample indicies for each number of contacts
for (num in c(1:50)){
  samp[[num]] <- sample(50, num)
}

for (day in 2:duration){ #time step loop, starting on day 2
  cat("Current Day:", day, "\n")
  for (inf_ID in inf_IDs){ #loop through all infected individuals on day "day"
    
    #Acquire contacted individuals + weight values
    numContacts <- contacts(meanContactsDay) #function to determine poisson random variable total contacts
    contactsDay <- listDf2[[inf_ID]][samp[[numContacts]],]  #use corresponding random sample from the 50 contacts of this person 
    #compiles all edges for entire simulation (for network viz)
    #tempID <- data.frame(ID1 = rep(inf_ID, nrow(contactsDay)), ID2 = contactsDay$Person_ID)
    #totalEdges <- merge(totalEdges, tempID, all = T) TAKES TOO LONG
    
    #Factor in race and disease
    sickID <- gets_sick(contactsDay) #returns IDs of all people who newly get sick : 
    
    #gets_sick function (in: data frame of contacted individID and weights, infection rate | out: individID of infected) 
    pop$inf_count[inf_ID] <- pop$inf_count[inf_ID] + length(sickID) #Updates # people this individual infected
    nextInf_IDs <- c(nextInf_IDs, sickID) #compiles those who will be sick tomorrow 
  }
  #######End of Day i! Who recovered? Who died? ###############
  nextInf_IDs <- unique(nextInf_IDs) #gets rid of repeated contacts during that day
  nextInf_IDs <- union(nextInf_IDs, inf_IDs) #include those who were infected at start of day with nextInf_IDs 
  dead_ID <- dead(inf_IDs, diseaseType, infantID, childID, adultID, seniorID, deathRate) 
  cumDead_ID <- union(cumDead_ID, dead_ID)
  recov_ID <- recovered(inf_IDs, diseaseType, RecoveryRate, cumDead_ID)
  cumRecov_ID <- union(cumRecov_ID, recov_ID)  #gets cumulative unique IDs of recovered people (for igraph)
  inf_IDs <- nextInf_IDs[!(nextInf_IDs %in% c(cumRecov_ID, cumDead_ID))] #remove those who recovered or died from infected list, forms group of infected for next day
  #Update health status of pop data frame
  pop$health_status[pop$ID %in% inf_IDs] <- 1
  pop$health_status[pop$ID %in% recov_ID] <- 2
  pop$health_status[pop$ID %in% dead_ID] <- 3
  #Get the number of individuals infected, recovered, dead, susceptible for this day into vector index
  numInfected[day] <- length(inf_IDs)
  numRecovered[day] <- length(cumRecov_ID)
  numDead[day] <- length(cumDead_ID)
  susceptible[day] <- totalPeople - numInfected[day] - numRecovered[day] - numDead[day]
  #New live-updating infected group for next day
  nextInf_IDs <- inf_IDs 
  cat(numInfected[day], "Infected as of Day", day, "\n")
}

plot(0,0,xlim = c(0,duration + 1),ylim = c(0,totalPeople), type = "n", xlab = "Day", ylab = "Number of People", main = "SIR Model of West Lafayette and Lafayette Population over Time")
legend(80,120000, c("Infected", "Recovered", "Dead", "Susceptible"), col = c("red", "green", "black", "blue"), lwd = 1)
lines(1:duration, numInfected, col = "red")
lines(1:duration, numRecovered, col = "green")
lines(1:duration, numDead, col = "black")
lines(1:duration, susceptible, col = "blue")
#nextInf_Ids: real-time updating ID vector of currently sick 
#inf_Ids: ID vector of sick at start of day (have chance of recover/die at end of day)
#sickID: ID vector of those who get sick from one inf_ID iteration
#inf_Id : infected individual lcv

#K-Node determination
# orderPop <- pop[order(-pop$inf_count),] #Sort in descending order by scored individuals (inf_count)
# k_number <- kPct * totalPeople #Number of rows to use based on user input of k-node pct
# orderPop[1:k_number,] #Extract rows of K-nodes
# pop$k_node[pop$ID %in% orderPop$ID] <- 1 #Assign K-node status in pop
# #pop is now our dataset for machine learning with pop$k_node as the prediction variable
# 
# #Percentage of SIRD from total population vectors for SIRD plot (input to animate function)
# pctSusc <- (susceptible/totalPeople) * 100
# pctInf <- (infected/totalPeople)*100
# pctRecov <- (recovered/totalPeople)*100
# pctDead <- (dead/totalPeople)*100 
# 
# #igraph visualization
# netEdges$Weight <- NULL #unweighted network (probably too large of network to use weights)
# simplify(netEdges) #remove redundant edges
# g <- graph_from_edgelist(netEdges, directed=F, vertices=nodes)
# #color of nodes
# V(g)$color <- ifelse(V(g)$ID1 %in% inf_Ids | V(g)$ID2 %in% inf_Ids, "red", 
#                      ifelse(V(g)$ID1 %in% cumRecov_ID | V(g)$ID2 %in% cumRecov_ID, "green", "black"))
# 
 #Disconnect from DB
 all_cons <- dbListConnections(MySQL())
 for (con in all_cons)
   dbDisconnect(con)

