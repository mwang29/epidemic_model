#This script compiles the IDs of the K-nodes for each of the 36 cases for rerunning sim with policies
knodes <- list()

#Extract from 36 .rda files
for (a in c(1:36)){
  file <- sprintf("Case%d.rda",a)
  load(file)
  knodes[[a]] <- pop$Person_ID[pop$k_node == 1]
}

#Save output to .rda for DiseaseSpread.R to enact vaccination policy
save(knodes, file = "knodes.rda")
