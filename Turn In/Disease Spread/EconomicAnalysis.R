df <- data.frame(matrix(ncol = 6, nrow = 36))
x <- c("TotalInfected", "MaxInfected", "InfectedDays", "TotalRecovered", "TotalDead", "SusceptAtEnd")
colnames(df) <- x
for (a in c(1:36)){
  file <- sprintf("Case%d.rda",a)
  load(file)
  df$TotalInfected[a] <- sum(numInfected)
  df$MaxInfected[a] <- max(numInfected)
  df$InfectedDays[a] <- infDays
  df$TotalRecovered[a] <- sum(numRecovered)
  df$TotalDead[a] <- sum(numDead)
  df$SusceptAtEnd[a] <- susceptible[100]
}
write.csv(df, file = "EconomicAnalysis.csv", row.names = F)
