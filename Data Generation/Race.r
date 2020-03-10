#Population Generation Parameters
numHouseholds <- 100 #number of households generated

#Region Data Initialization
Data <- read.csv("population.csv", header = T)
regions <- Data[,2]
regionsProb <- regions / sum(regions)

#Probability sampling function
regionDist = function(n) { 
  sample(x = c(1:66), n, replace = T, prob = regionsProb) 
}

#Sampling for Regions
population <- sampleDist(numHouseholds)

##Households
houseData <- read.csv("household.csv")
houseWL <- 


raceData <- read.csv("Race.csv", header = T)
raceData <- raceData[3,seq(4,length(raceData),2)]
raceData <- raceData / sum(raceData)
rmultinom(10, 100000, raceData)

sampleDist = function(n) { 
  sample(x = c("2ormore", "asian","black","hawaiian","latino", "native", "other", "white"), n, replace = T, prob = raceData) 
}

y2 <- sampleDist(7)
