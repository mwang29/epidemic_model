#This function creates the traffic distribution for parks and recreational centers and writes to a .csv for the Database
hospi <- read.csv("Hospitals.csv", header = T, stringsAsFactors = F) 
indivData <- read.csv("individuals.csv", header = T, stringsAsFactors = F) #read the population data to get total population
visPyPr <- 4 #visits per year per person, referenced from https://www.cdc.gov/nchs/pressroom/08newsreleases/visitstodoctor.htm
meanHosp <- visPyPr * length(indivData$ID) / (length(hospi$Hospitals)*52) #calculate traffic per week (mean)
visVec <- rnorm(length(hospi$Hospitals), meanHosp, 1000) #sample from normal distribution
hospi$Traffic.Week <- visVec
write.csv(hospi, file = "Hospitals.csv", row.names = F) #write to csv


