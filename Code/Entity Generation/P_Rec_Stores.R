#This function creates the traffic distribution for parks and recreational centers and writes to a .csv for the Database
parkData <- read.csv("parks_rec.csv", header = T, stringsAsFactors = F) 
indivData <- read.csv("Individuals.csv", header = T, stringsAsFactors = F) #read the population data to get total population
visPyPr <- 29 #visits per year per person, referenced from nrpa.org
meanPPark <- visPyPr * length(indivData$ID) / (length(parkData$Parks)*52) #calculate traffic per week (mean)
visVec <- abs(floor(rnorm(length(parkData$Parks), meanPPark, 1000))) #sample from normal distribution
parkData$Traffic.Week <- visVec
write.csv(parkData, file = "parkData.csv", row.names = F) #write to csv

#This function creates the traffic distribution for shopping centers and writes to a .csv for the Database
storeData <- read.csv("stores.csv", header = T, stringsAsFactors = F)
meanStoreTraffic <- 13500 #referenced from distributech.net/demographics.aspx
stdv <- 3000 #same as above
storeData$Traffic.Week <- abs(floor(rnorm(length(storeData$ï..Name), 13500, 3000))) #sampling from normal distribution
storeData$Traffic.Week[storeData$ï..Name %in% c("Walmart","Walmart WL")] = 20000 #use Walmart data referenced
colnames(storeData) <- c("Name", "Region", "Traffic_Week")
write.csv(storeData, file = "storeData.csv", row.names = F)

