iter <- 10
for (d in c(2)){ #influenza, smallpox, measles, ebola
  for (n in c(1)){ #number ppl initially infected
    for (k in c(.1,.6)){ #k-node percentage of infected individuals
      library(RMySQL)
      library(DBI)
      library(partitions)
      library(igraph)
      library(pbapply)
      ########################################### Contacts and Database initialization ###################################
      
      #Loads into object "listDf" of all contacts and base transmission probabilities for each individual as a list of data frames
      load("listDf_Final2.rda") 
      
      #Loads k_node IDs from simulation output for rerunning sim
      load("knodes.rda")
      
      #Database Connection
      con <- dbConnect(MySQL(), user="g1081391", password="Wif1hasnomeaning", dbname="g1081391", host="mydb.ics.purdue.edu")
      on.exit(dbDisconnect(con))
      
      ################################################# Queries ###############################################
 
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
      
      #Decide who to infect! and how many!
      Qinf_IDs <- "SELECT I.Person_ID AS ID
      FROM Individuals I 
      ORDER BY RAND()
      LIMIT "
      ########################################### Parameter intialization ###############################
      #Population data frame initialization (k-nodes data set)
      totalPeople <- dbGetQuery(con,"SELECT COUNT(*) FROM Individuals;")
      pop <- data.frame(ID = c(1:totalPeople$`COUNT(*)`)) #individual IDs as a column
      

      duration <- 100 #number of days for simulation
      meanContactsDay <- 13.4 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2270306/
      
      ##############################################User inputs##############################################
      diseaseType <- d #Enter type of disease 1= Influenza, 2 = Smallpox, 3 = Measles, 4 = Ebola
      initialNumber <- n #1, 3, or 10
      kPct <- k#k nodes percentage to identify 0.1, 0.3, 0.6
      ####################################################################################################################
      
      #Initially infected individuals randomly sampled by querying Individuals DB
      tempdf <- dbGetQuery(con, paste0(Qinf_IDs,initialNumber))
      inf_IDs <- tempdf$ID
      
      #Initialization of vectors/data frames used in simulation
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
      contagious <- 1/avgMult #Normalize base contagiousness to mean 
      
      #contagious multiplier for disease type
      ifelse(diseaseType == 2, contagious <- 2.4/avgMult, 
             ifelse(diseaseType == 3, contagious <- 6/avgMult, contagious <- 1/avgMult)) 
      
      #Normalized to mean race infection probabilties for each race to infectious disease (FROM CDC)
      raceMean <- mean(c(3.0,8.1,10.9,8.2,4.1,100))
      raceWeight <- c(8.1/raceMean, 10.9/raceMean, 8.2/raceMean, 4.1/raceMean,4.1/raceMean) #asian, black, hispanic, amerindian, multirace
      
      #Age weight: 0-4, 5-18, 19-64, > 65
      ageWeight <- c(0.0005775, 0.000575, 0.0003175, 0.000855)#transmission weight based on age and seasonal influenza
      
      #Data taken from summing the columns of the transition probability matrix from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/
      deathRate <- c(0.00005, 0.00003, 0.001,0.027) #for each age group: 0-4, 5-18, 19-64, > 65 for influenza
      RecoveryRate <- 1/6 #rate = 1/ average time spent in infected class
      
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
      
      #Population characteristics into pop dataframe
      pop$Race[pop$ID %in% whiteID$WhiteID] <- "White"
      pop$Race[pop$ID %in% asianID$AsianID] <- "Asian"
      pop$Race[pop$ID %in% blackID$BlackID] <- "Black"
      pop$Race[pop$ID %in% hispanicID$HispanicID] <- "Hispanic"
      pop$Race[pop$ID %in% multiraceID$MultiraceID] <- "Multirace"
      pop$Race[pop$ID %in% amerindianID$AmerIndianID] <- "American Indian"
      pop$Race[pop$ID %in% asianID$AsianID] <- "Asian"
      
      age <- dbGetQuery(con, "SELECT FLOOR(DATEDIFF(CURDATE(),I.DOB) / 365.25) AS Age FROM Individuals I;")
      pop$Age <- age$Age
      
      pop$Income[pop$ID %in% povID] <- "Poverty"
      pop$Income[!(pop$ID %in% povID)] <- "No Poverty"
      
      pop$health_status <- 0 #everyone healthy
      pop$health_status[pop$ID %in% inf_IDs] <- 1 #initial infected in health_status
      
      pop$inf_count <- 0 #0 people initially infected
      pop$k_node <- 0 #everyone not k_node
      
      pop$Purdue <- "No"
      purdue <- dbGetQuery(con, "SELECT I.Person_ID AS Person_ID FROM Individuals I, Works_At W WHERE I.Person_ID = W.Person_ID AND W.Work_ID = 1000")
      pop$Purdue[purdue$Person_ID] <- "Yes"
      
      region <- dbGetQuery(con, "SELECT I.Person_ID AS ID, R.Region_Num AS Region FROM Region R, Individuals I, Household H WHERE I.Household_ID = H.Household_ID AND H.Region_Num = R.Region_Num;")
      pop$Region[region$ID] <- region$Region[pop$ID %in% region$ID]
      
      schools <- dbGetQuery(con, "SELECT I.Person_ID AS ID FROM Individuals I, Works_At W WHERE I.Person_ID = W.Person_ID AND W.Work_ID IN (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);")
      pop$School <- "No"
      pop$School[schools$ID] <- "Yes"
      
      bus <- dbGetQuery(con, "SELECT I.Person_ID AS ID FROM Individuals I, Household H WHERE I.Household_ID = H.Household_ID AND H.PubTrans = 'YES';")
      pop$Bus <- "No"
      pop$Bus[bus$ID] <- "Yes"
      ################################################ Functions ###########################################
      #Disease Type factor function
      f <- function(df, contagious){
        within(df, Weight <- Weight * (contagious)) 
      }

      #Determines number of contacts per person per day, and split up into work, bus community
      contacts <- function(meanContactsDay){
        nContacts <- rpois(1,meanContactsDay)
        if(nContacts >50 ){nContacts = 50} 
        else if (nContacts == 0){nContacts = 1}
        return(nContacts)
      }
      
      #Immune system weight for death rate 
      ageMult <- function(all_edges, infantID, childID, adultID, seniorID, ageWeight){
        all_edges$Weight[all_edges$Person_ID %in% infantID$infantID] <- all_edges$Weight[all_edges$Person_ID %in% infantID$infantID] * ageWeight[1]
        all_edges$Weight[all_edges$Person_ID %in% childID$childID] <- all_edges$Weight[all_edges$Person_ID %in% childID$childID] * ageWeight[2]
        all_edges$Weight[all_edges$Person_ID %in% adultID$adultID] <- all_edges$Weight[all_edges$Person_ID %in% adultID$adultID] * ageWeight[3]
        all_edges$Weight[all_edges$Person_ID %in% seniorID$seniorID] <- all_edges$Weight[all_edges$Person_ID %in% seniorID$seniorID] * ageWeight[4]
        return(all_edges)
      }

      #Converts data frame's Weight column to probability
      Weight2Prob <- function(all_edges){
        all_edges$Weight <- 1 - exp(-all_edges$Weight)
        return(all_edges)
      }
      
      #Input data frame and return IDs of those who become sick
      gets_sick <- function(all_edges){
        randNo <- runif(length(all_edges$Weight),0,1)
        sickID <- all_edges$Person_ID[which(randNo < all_edges$Weight)]
        return(sickID)
      }
      
      #Input data frame and return IDs of those who die
      dead <- function(inf_IDs, diseaseType, infantID, childID, adultID, seniorID, deathRate){
        diseaseWeight <- 1 #base
        if (diseaseType == 2){diseaseWeight <- 150 #smallpox
        }else if (diseaseType == 3){diseaseWeight <- 3 #measles
        }else if (diseaseType == 4){diseaseWeight <- 500} #Ebola
        inf <- data.frame(Person_ID = inf_IDs, Weight = diseaseWeight)
        temp <- ageMult(inf, infantID, childID, adultID, seniorID, deathRate) 
        inf$Weight <- temp$Weight #put updated weight onto inf_people
        randNo <- runif(length(inf_IDs),0,1)
        dead_ID <- inf$Person_ID[which(randNo < inf$Weight)]
        return(dead_ID)
      }
      
      #Input data frame and return IDs of those who recover
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
      
      ###########################################Disease spread model#########################################
      #Modify based on parameters loaded edge list of POSSIBLE contacts per person precalculated in EdgelistRetrieve.R
      listDf2 <- pblapply(listDf, f, contagious) #factor in disease type multiplier
      listDf2 <- pblapply(listDf2, Weight2Prob) #Converts time-weight to probability with 1-exp(-time)
      
      #Explanation of dynamic variables within loop:
      
      #inf_Ids: ID vector of sick at start of day (have chance of recover/die at end of day)
      #inf_Id : infected individual lcv
      #sickID: ID vector of those who get sick from one inf_ID iteration, changes per person, per day
      #nextInf_Ids: real-time updating ID vector of currently sick, changes per day

      for (day in 2:duration){ #time step loop, starting on day 2
        cat("Current Day:", day, "\n")
        for (inf_ID in inf_IDs){ #loop through all infected individuals for each day
          
          #Acquire contacted individuals + weight values
          numContacts <- contacts(meanContactsDay) #function to determine poisson random variable total contacts
          contactsDay <- listDf2[[inf_ID]][sample(length(listDf2[[inf_ID]]$Person_ID),numContacts),]  #sample specific number from all possible contacts for that person
          rownames(contactsDay) <- c() #remove row names
          
          ########################################################################################
          #Rerunning with policy enacted (remove contacts of a specific mixing group)
          # S = Shutting down Schools
          # W = Shutting down Businesses/Workplaces (Possible improvement to target specific workplaces)
          # B = Closing bus routes
          # Q = Quarantine (Possible improvement)
          
          contactsDay <- contactsDay[!(contactsDay$Type == "S"),] #closing schools
          
          #Rerunning with vaccination/isolation policy of K-nodes, with K-node contacts removed
          contactsDay <- contactsDay[!(contactsDay$Person_ID %in% knodes[[iter]]),]
          #######################################################################################
          
          #returns IDs of all people who newly get sick 
          sickID <- gets_sick(contactsDay) 
          
          #update k-node score (number of people this specific individual infected during simulation)
          pop$inf_count[inf_ID] <- pop$inf_count[inf_ID] + length(sickID) 
          
          #compiles those who will be sick tomorrow 
          nextInf_IDs <- c(nextInf_IDs, sickID) 
        }
        #######End of Day i! Who recovered? Who died? ###############
        nextInf_IDs <- unique(nextInf_IDs) #gets rid of repeated contacts during that day
        nextInf_IDs <- union(nextInf_IDs, inf_IDs) #include those who were infected at start of day with nextInf_IDs 
        
        #only see who is dead/recovered if nonzero number of ppl are infected
        if (length(inf_IDs) != 0){ 
          dead_ID <- dead(inf_IDs, diseaseType, infantID, childID, adultID, seniorID, deathRate) 
          cumDead_ID <- union(cumDead_ID, dead_ID)
          recov_ID <- recovered(inf_IDs, diseaseType, RecoveryRate, cumDead_ID)
          cumRecov_ID <- union(cumRecov_ID, recov_ID)  #gets cumulative unique IDs of recovered people (for igraph)
        }
        
        #remove those who recovered or died from infected list, forms group of infected for next day
        inf_IDs <- nextInf_IDs[!(nextInf_IDs %in% c(cumRecov_ID, cumDead_ID))] 
        
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
      
      #If no one infected on last day of simulation, extend current counts until end of 100 days. 
      if(numInfected[length(numInfected)] == 0){numInfected[length(numInfected):100] <- 0
      numRecovered[length(numRecovered):100] <- numRecovered[length(numRecovered)]
      numDead[length(numDead):100] <- numDead[length(numDead)]
      susceptible[length(susceptible):100] <- susceptible[length(susceptible)]}
      susceptible <- as.vector(unlist(susceptible))
      
      #Plot SIR Model
      plot(0,0,xlim = c(0,duration + 1),ylim = c(0,totalPeople), type = "n", xlab = "Day", ylab = "Number of People", main = "SIR Model of West Lafayette and Lafayette Population over Time")
      legend(80,120000, c("Infected", "Recovered", "Dead", "Susceptible"), col = c("red", "green", "black", "blue"), lwd = 1)
      lines(1:duration, numInfected, col = "red")
      lines(1:duration, numRecovered, col = "green")
      lines(1:duration, numDead, col = "black")
      lines(1:duration, susceptible, col = "blue")
      
      
      #K-Node determination
      k_number <- floor(kPct * max(numInfected))
      k_nodes <- head(pop[order(-pop$inf_count),],n = k_number) #Sort in descending order by scored individuals (inf_count)
      #Number of rows to use based on user input of k-node pct
      pop$k_node[pop$ID %in% k_nodes$ID] <- 1 #Assign K-node status in pop
      
      #Total infected days - use in cost calculation for policy
      infDays <- sum(numInfected) 
      
      #pop is now our dataset w/ features for machine learning with pop$k_node as the feature for prediction

      #Save objects of interest into .rda
      save(pop,numInfected, numRecovered, numDead, susceptible, infDays, file = sprintf("Case%db.rda", iter))
      all_cons <- dbListConnections(MySQL())
      
      #Disconnect from DB
      for (con in all_cons)
        dbDisconnect(con)
      rm(list=setdiff(ls(), c("iter", "d", "n", "k"))) #clears workspace except for looping variables
      
      #Updates iteration count
      iter <- iter + 2
    }
  }
}