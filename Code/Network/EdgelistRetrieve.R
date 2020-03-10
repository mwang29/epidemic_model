library(pbapply)
library(RMySQL)

listDf <- list()

for (inf_ID in c(1:length(indivData$Person_ID))){
work_edges <- getEdges(inf_ID, QWork, QEnd, 100)#Query for work contacts (Poisson) - Returns vector of individual ID
work_edges$Weight <- rep(480,nrow(work_edges)) #Assign time-based weight
work_edges$Type <- "W"

bus_edges <- getEdges(inf_ID, QBus, QEnd, 100) #Query for bus contacts (Poisson)
bus_edges$Weight <- rep(45,nrow(bus_edges)) #Assign weight based on mean bus commute
bus_edges$Type <- "B"

comm_edges <- getEdges(inf_ID, QCommunity, QEnd, 100)#Query for community contacts (Poisson) 
comm_edges$Weight <- rep(255.9, nrow(comm_edges)) * getDenMultiplier(inf_ID, QDensity, meanDensity) #Assign weight based on avg time (Bureau of Labor Stats) and Pop Density of Region
comm_edges$Type <- "C"
                                                                                                                          
house_edges <- getEdges(inf_ID, QHousehold, QEnd, 100) #Query for household contacts (Limit is all individuals) ()
house_edges = house_edges[house_edges != inf_ID,] #Don't include him/herself
house_edges <- data.frame(Person_ID = house_edges) 
house_edges$Weight <- rep(303.9, nrow(house_edges)) #Assign weight based on BLS data
house_edges$Type <- "H"

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
