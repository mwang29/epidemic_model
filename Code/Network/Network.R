library(RMySQL)

#Normalized to White multipliers for each race to H1N1 influenza per 100,000 people (CDC)
whiteProp <- 3.0 
asianWeight <- 8.1/whiteProp
blackWeight <- 10.9/whiteProp
hispanicWeight <- 8.2/whiteProp
amerindianWeight <- 4.1/whiteProp
multiWeight <- 4.1/whiteProp

#Poverty weight
povWeight <- 1.5 #No exact multiplier can be found, but two biosecurity papers suggest strong correlations between poverty and influenza

#Age weight: 0-4, 5-18, 19-64, > 65
ageVec <- c(0.00231, 0.0023, 0.00127, 0.00324) / min(c(0.00231, 0.0023, 0.00127, 0.00324))
#Data taken from summing the columns of the transition probability matrix from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2725959/

con <- dbConnect(MySQL(),user="jmonti", password="rockymountainhigh", dbname="jmonti", host="mydb.ics.purdue.edu")
on.exit(dbDisconnect(con))


## WEIGHTED EDGE NETWORK CREATION
##############################################################################################
#Creates edge list of indiviudals along same workplace
QWork <- "SELECT A.Person_ID AS ID1, B.Person_ID AS ID2 FROM Individuals A INNER JOIN Individuals B ON A.Work_ID = B.Work_ID WHERE A.Person_ID < B.Person_ID AND A.Person_ID = "

#Edge list of individuals who belong to a household within a region with same bus ID
QBus <- "SELECT A.ID AS ID1, B.ID AS ID2 FROM (SELECT I.Person_ID AS ID, B.Bus_ID AS Bus_ID FROM Individuals I, Belongs_To B, Household H WHERE H.Region_Num = B.Region_Num AND H.Household_ID = I.Household_ID AND H.PubTrans = 'YES') A INNER JOIN (SELECT I.Person_ID AS Person_ID, B.Bus_ID AS Bus_ID FROM Individuals I, Belongs_To B, Household H WHERE H.Region_Num = B.Region_Num AND H.Household_ID = I.Household_ID AND H.PubTrans = 'Yes') B ON A.Bus_ID = B.Bus_ID WHERE A.Person_ID < B.Person_ID AND A.Person_ID = "

#Edge list of individuals who belong to the same household
QHousehold <- "SELECT A.Person_ID AS ID1, B.Person_ID AS ID2 FROM Individuals A INNER JOIN Individuals B ON A.Household_ID = B.Household_ID WHERE A.Person_ID < B.Person_ID AND A.Person_ID = "

#Edge list of individuals that share regions
QCommunity <- "SELECT A.Person_ID AS ID1, B.Person_ID AS ID2 FROM (SELECT I.Person_ID AS Person_ID, H.Region_Num AS Region_Num FROM Individuals I, Household H WHERE I.Household_ID = H.Household_ID) A INNER JOIN Individuals B ON A.Region_Num = B.Region_Num WHERE A.Person_ID < B.Person_ID;"

#Races
QWhite <- "SELECT I.Person_ID AS WhiteID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'White';"
whiteID <- dbGetQuery(con, QWhite)

QAsian <- "SELECT I.Person_ID AS AsianID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Asian';"
asianID <- dbGetQuery(con, QAsian)

QBlack <- "SELECT I.Person_ID AS BlackID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Black';"
blackID <- dbGetQuery(con, QBlack)

QHispanic <- "SELECT I.Person_ID AS HispanicID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Hispanic';"
hispanicID <- dbGetQuery(con, QHispanic)

QMultirace <- "SELECT I.Person_ID AS MultiraceID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Multirace';"
multiraceID <- dbGetQuery(con, QMultirace)

QAmerIndian <- "SELECT I.Person_ID AS AmerIndianID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Race = 'Amer Indian';"
amerindianID <- dbGetQuery(con, QAmerIndian)

QPov <- "SELECT I.Person_ID AS povID
FROM Individuals I, Household H
WHERE I.Household_ID = H.Household_ID AND H.Income = 'POV';"
povID <- dbGetQuery(con, QPov)

#Age
Qinfant <- "SELECT I.Person_ID AS infantID
FROM Individuals I
WHERE I.Age <= 4;"
infantID <- dbGetQuery(con, Qinfant)

Qchild <- "SELECT I.Person_ID AS childID
FROM Individuals I
WHERE I.Age BETWEEN 5 AND 18;"
childID <- dbGetQuery(con, Qchild)

Qadult <- "SELECT I.Person_ID AS adultID
FROM Individuals I
WHERE I.Age BETWEEN 19 AND 64;"
adultID <- dbGetQuery(con, Qadult)

Qsenior <- "SELECT I.Person_ID AS seniorID
FROM Individuals I
WHERE I.Age > 64;"
seniorID <- dbGetQuery(con, Qsenior)

#Shared location effects on disease transmission edge weights
df1 <- dbGetQuery(con, QWork) #Shared work edges
df1$Weight <- 8; #Average 8 hour day according to Bureau of Labor Statistics
df2 <- dbGetQuery(con, QBus)  #Shared bus routes edges
df2$Weight <- 0.75 #Average bus commute is 45 minutes according to Census Bureau
df3 <- dbGetQuery(con, QHousehold)  #Shared household edges
df3$Weight <- (1.82+0.52+0.16 + .5*5.13); #Sum of 1/2 of leisure time (TV), household activities, helping household members, telephone calls from BLS.  
df4 <- dbGetQuery(con, QCommunity) #Shared community edges
df4$Weight <- (.5*5.13+0.32+0.21+1.17) #Sum of 1/2 of leisure time (Sports), caring for nonhousehold members, organizational actvs, eating.
netDf <- rbind(df1, df2, df3, df4) #combines edge list
netDf <- df3
#Multiplies weights based on Race factor
netDf$Weight[netDf$ID1 %in% asianID] <- netDf$Weight[netDf$ID1 %in% asianID] * asianWeight
netDf$Weight[netDf$ID1 %in% blackID] <- netDf$Weight[netDf$ID1 %in% blackID] * blackWeight
netDf$Weight[netDf$ID1 %in% hispanicID] <- netDf$Weight[netDf$ID1 %in% hispanicID] * hispanicWeight
netDf$Weight[netDf$ID1 %in% amerindianID] <- netDf$Weight[netDf$ID1 %in% amerindianID] * amerindianWeight
netDf$Weight[netDf$ID1 %in% multiraceID] <- netDf$Weight[netDf$ID1 %in% multiraceID] * multiraceWeight

netDf$Weight[netDf$ID2 %in% asianID] <- netDf$Weight[netDf$ID2 %in% asianID] * asianWeight
netDf$Weight[netDf$ID2 %in% blackID] <- netDf$Weight[netDf$ID2 %in% blackID] * blackWeight
netDf$Weight[netDf$ID2 %in% hispanicID] <- netDf$Weight[netDf$ID2 %in% hispanicID] * hispanicWeight
netDf$Weight[netDf$ID2 %in% amerindianID] <- netDf$Weight[netDf$ID2 %in% amerindianID] * amerindianWeight
netDf$Weight[netDf$ID2 %in% multiraceID] <- netDf$Weight[netDf$ID2 %in% multiraceID] * multiraceWeight
#Multiplies weights based on poverty factor
#netDf$Weight[netDf$ID1 %in% povID] <- netDf$Weight[netDf$ID1 %in% povID] * povWeight

#Multiplies edge weights based on age factor for first ID, see lines 14-16 for ageVec value rationale
netDf$Weight[netDf$ID1 %in% infantID] <- netDf$Weight[netDf$ID1 %in% infantID] * ageVec[1]
netDf$Weight[netDf$ID1 %in% childID] <- netDf$Weight[netDf$ID1 %in% childID] * ageVec[2]
netDf$Weight[netDf$ID1 %in% adultID] <- netDf$Weight[netDf$ID1 %in% adultID] * ageVec[3]
netDf$Weight[netDf$ID1 %in% seniorID] <- netDf$Weight[netDf$ID1 %in% adultID] * ageVec[4] 
#Same process for second ID in edge
netDf$Weight[netDf$ID2 %in% infantID] <- netDf$Weight[netDf$ID2 %in% infantID] * ageVec[1]
netDf$Weight[netDf$ID2 %in% childID] <- netDf$Weight[netDf$ID2 %in% childID] * ageVec[2]
netDf$Weight[netDf$ID2 %in% adultID] <- netDf$Weight[netDf$ID2 %in% adultID] * ageVec[3]
netDf$Weight[netDf$ID2 %in% seniorID] <- netDf$Weight[netDf$ID2 %in% seniorID] * ageVec[4]

#Weighted igraph network
network <- graph.data.frame(netDf, directed = F) #creates weighted igraph network
simplify(network, edge.attr.comb=list(weight="sum")) #gets rid of multiple edges and sums the weights to combine
#plot(network,layout=layout.drl, vertex.size=1, vertex.label=NA) #plots

write.csv(netDf, file = "networkDf.csv", row.names = F)
#Analytics
d1 <- degree_distribution(netDf, cumulative = T)
###############################################################################################################

all_cons <- dbListConnections(MySQL())
for (con in all_cons){dbDisconnect(con)}