library(reshape) #for unpacking df
library(gsubfn)
library(proto)
library(sqldf) #for executing query for JOIN
library(RMySQL)
detach("package:RMySQL", unload=TRUE)

#Data import
Data <- read.csv("Household.csv", header = T)
schoolData <- read.csv("schools.csv", header = T, stringsAsFactors=FALSE)

#Workplace data import
workData <- read.csv("Workplaces.csv", header=T)
#School data
schData <- workData[1:15,]
#Healthcare services data
healthData <- workData[16:91,]
#Ag business data
farmData <- workData[92:100,]
#Police data
policeData <- workData[101:102,]
#Firefighter data
ffData <- workData[103:104,]
#Remaining work data
remainData <- workData[105:999,]

#Occupation data import
occData <- read.csv("Occupation.csv", header=T)

uniform_sample <-function(n,j,k) sample(j:k,n,replace=T)
#Unwrap our Household Population based on number of people
df1 <- untable(Data[,c(2,6)], num=Data[,6])
IndivID <- rownames(df1)
df1 <- cbind(IndivID, df1)
rownames(df1) <- 1:nrow(df1)

#Unwrap Household Population based on number of children
df2 <- untable(Data[,c(2,6)], num = Data[,5])
IndivID <- rownames(df2)
df2 <- cbind(IndivID, df2)
rownames(df2) <- 1:nrow(df2)
#Label all in df2 as "Child"
df2$Child <- "Child" #label as child

#Assign Age
Age <- uniform_sample(nrow(df2),4,18)
df2 <- cbind(df2, Age)
School <- rep(0,nrow(df2))
temp <- vector('character')
for (x in c(1:nrow(df2))){
  if(df2$Age[x] < 11){
    School[x] <- sample(x = schoolData[df2$Region[x],c(1,2)], size = 1, prob = c(0.5, 0.5))
  } else if (df2$Age[x] < 15) {
    School[x] <- schoolData$Middle.Schools[df2$Region[x]]
  } else if (df2$Age[x] < 19) {
    School[x] <- schoolData$High.Schools[df2$Region[x]]
  }
}
df2 <- cbind(df2, School = unlist(School)) #add School into df2

df2$Region <- NULL #Avoid repeat with df1

#Join all population and children together via IndivID
df3 <- sqldf("SELECT IndivID, Age, Child, School, Region
              FROM df1
              LEFT JOIN df2 USING(IndivID)")

#Assign ages to general population based on exponential distribution (cited)
df3$Age[is.na(df3$Age)] <- floor(rexp(length(df3$Age[df3$Child != "Child"]), rate = 0.05))+18
df3$Age[df3$Age > 100] = 100
df3 <- cbind(ID = c(1:nrow(df3)),df3)

#Floor IndivID to Household ID
df3$IndivID <- floor(as.numeric(df3$IndivID))
colnames(df3)[colnames(df3)=="IndivID"] <- "HouseID"
individualPop <- df3

#Health Status
individualPop$Health_Status <- "Healthy"

#Workplace Categorization
#Assigning school as work for children and Purdue students
for (x in c(1:nrow(individualPop))){  
  if ((individualPop$Age[x] < 18)){  
    individualPop$Workplace[x] <- individualPop$School[x]
  } 
  else {
    #Assigning random job industry to people who are not students
    individualPop$Workplace[x] <- sample(occData[,1], size = 1, prob = occData[,2], replace = TRUE)
    #Determining who is retired and not working
    if (individualPop$Age[x] > 64){
      individualPop$Workplace[x] <- 0
    }
    #Assigning teachers to schools
    else if (identical(individualPop$Workplace[x], "ED")){
      individualPop$Workplace[x] <- sample(schData[,1], size = 1, prob=schData[,2], replace=TRUE)
    }
    #Assigning healthcare professionals to healthcare facilities
    else if (identical(individualPop$Workplace[x], "HEALTH")){
      individualPop$Workplace[x] <- sample(healthData[,1], size = 1, prob = healthData[,2], replace=TRUE)
    }
    #Assigning farming professionals to ag businesses
    else if (identical(individualPop$workplace[x], "FARM")){
      individualPop$workplace[x] <- sample(farmData[,1], size = 1, prob = farmData[,2], replace = TRUE)
    }
    #Assigning police and firefighters
    else if (identical(individualPop$Workplace[x], "LAWENF")){
      individualPop$Workplace[x] <- sample(policeData[,1], size = 1, prob = policeData[,2], replace = TRUE)
    }
    else if (identical(individualPop$Workplace[x], "FFGHT")){
      individualPop$Workplace[x] <- sample(ffData[,1], size = 1, prob = ffData[,2], replace = TRUE)
    }
    #The other 895 businesses have a mixture of the other kinds of occupations, so randomly mix them
    else {
      individualPop$Workplace[x] <- sample(remainData[,1], size = 1, prob = remainData[,2], replace = TRUE)
    }
  }
}

#Change the school names to numbers that correspond to id
for (x in c(1:nrow(individualPop))){
  if (identical(individualPop$Workplace[x], "CL")){
    individualPop$Workplace[x] <- 1
  } 
  else if (identical(individualPop$Workplace[x], "Earhart")){
    individualPop$Workplace[x] <- 2
  }
  else if (identical(individualPop$Workplace[x], "Edgelea")){
    individualPop$Workplace[x] <- 3
  }
  else if (identical(individualPop$Workplace[x], "Glenn Acres")){
    individualPop$Workplace[x] <- 4
  }
  else if (identical(individualPop$Workplace[x], "HH")){
    individualPop$Workplace[x] <- 5
  }
  else if (identical(individualPop$Workplace[x], "Jefferson")){
    individualPop$Workplace[x] <- 6
  }
  else if (identical(individualPop$Workplace[x], "Miami")){
    individualPop$Workplace[x] <- 7
  }
  else if (identical(individualPop$Workplace[x], "Miller")){
    individualPop$Workplace[x] <- 8
  }
  else if (identical(individualPop$Workplace[x], "Murdock")){
    individualPop$Workplace[x] <- 9
  }
  else if (identical(individualPop$Workplace[x], "Oakland")){
    individualPop$Workplace[x] <- 10
  }
  else if (identical(individualPop$Workplace[x], "Sunnyside")){
    individualPop$Workplace[x] <- 11
  }
  else if (identical(individualPop$Workplace[x], "Tecumseh")){
    individualPop$Workplace[x] <- 12
  }
  else if (identical(individualPop$Workplace[x], "Vinton")){
    individualPop$Workplace[x] <- 13
  }
  else if (identical(individualPop$Workplace[x], "WLJSH")){
    individualPop$Workplace[x] <- 14
  }
  else if (identical(individualPop$Workplace[x], "PURDUE")){
    individualPop$Workplace[x] <- 15
  }
}
individualPop$Child <- NULL
individualPop$School <- NULL
individualPop$Health_Status <- NULL
colnames(individualPop)[1]<-"Person_ID"
purdueHouseholds <- read.csv('purdueHouseholds.csv', header = T, stringsAsFactors = F)

df4 <- untable(purdueHouseholds[,c(2,6)], num=purdueHouseholds[,6])
HouseID <- rownames(df4)
df4 <- cbind(HouseID, df4)
rownames(df4) <- 1:nrow(df4) + nrow(individualPop)
df4 <- cbind(rownames(df4), df4)
df4$HouseID <- floor(as.numeric(as.character(df4$HouseID))) + 40490
colnames(df4)[1]<-"Person_ID"
df4$Age <- uniform_sample(nrow(df4),18,26)
df4$Workplace <- "PURDUE"
df4$nPeople<-NULL
df5<-rbind(individualPop, df4)

#Write to csv file
write.csv(df5, file = "Individual.csv",row.names=FALSE)

