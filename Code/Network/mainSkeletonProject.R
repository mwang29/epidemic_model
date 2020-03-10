install.packages("RMySQL")
library(RMySQL)

#connect to database
con <- dbConnect(MySQL(),user="jmonti", password="rockymountainhigh", dbname="jmonti", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))

#get IDs of all initially infected people
Inf_IDs <-getIDs(seedNumber, seedType)

#variable declarations+ initialization:
n_timesteps <- getNtimesteps(dis_type) #assign number of timesteps
totalPeople <-            #total number of people
k_number <-     #number of k nodes to identify


#Each iteration of loop is one timestep:

for (t in c(1:n_timesteps))
{
  #loops through every infected (I) individual
  for (inf_ID in inf_IDs)
  {
    #get all weighted edges of people connected to the infected person
   work_edges <- getWork_edges(inf_ID)
   house_edges <- getHouse_edges(inf_ID)
   comm_edges <- getComm_edges(inf_ID)
   bus_edges <- getBus_edges(inf_ID)
   all_edges <- rbind(work_edges, house_edges, comm_edges, bus_edges)
   simplify(all_edges, edge.attr.comb=list(weight="sum"))
   
   #determine if each individual who contacts this person gets sick
   sickID <- gets_sick(all_edges) #returns IDs of all people who newly get sick
   inf_IDs$inf_count <- inf_IDs$inf_count + nrow(sickID)
   recov_ID <- recovered(inf_ID) #deteremines and returns IDs of the sick people who recover (need to pass the immunity parameter???)
   dead_ID <- dead(inf_ID) #determines and returns IDs of sick people who die
   updateHealth(sickID, recov_ID, dead_ID) #updates the health status of all the individuals that get sick to infected and those who recovered
   
  }
  infected[t] <- nrow(inf_ID) + nrow(sick_ID)
  recovered[t] <- nrow(recov_ID)
  dead[t] <- nrow(dead_ID)
  susceptible[t] <- totalPeople - infected[t] - recovered[t] - dead[t]
}

k_IDs <- head(inf_IDs[order(inf_IDs[,2]), ], k_number) #IDs of the top k infecting people identified

#plot the graph of counts vs time
plot(0,0,xlim = c(0:n_timesteps + 1),ylim = c(0:totalPeople, type = "n"))
lines(1:n_timesteps, infected, col = "red")
lines(1:ntimesteps, recovered, col = "purple")
lines(1:ntimesteps, dead, col = "black")
lines(1:ntimesteps, susceptible, col = "blue")
     
