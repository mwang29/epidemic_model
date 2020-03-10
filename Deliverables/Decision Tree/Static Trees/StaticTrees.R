x<-rep(c(0.1,0.3,0.6),12) # Create a vector of the desired percentage of selected k-nodes
form <- as.formula(k_node ~ Income + Purdue + Age + Region +School + Bus) # Formula for decision trees
knodevector<-vector() # Initialization of empty vectors
casevector<-vector()

is.integer0 <- function(x){ # Function to avoid integer(0) error
  is.integer(x) && length(x) == 0L
}


for (i in 1:36) {
  load(paste0("Case",i,".rda")) # Loads "Case[i].rda" 
  print(paste("Iteration: ","[", i,"]")) # Displays iteration of for looop (for readability)
  knodevector<-sample(pop$ID,floor(x[i]*length(pop$ID)),replace=TRUE) # Samples for the k-nodes from the specified Rda file, 
                                                                      # Sampling is done without replacement to ensure there is no covariance
  pop<-pop[pop$ID %in% knodevector,] # Reassigns pop to be all of the sampled k-nodes
  if (is.integer0(which(pop$k_node==1))) { # In the case that all the k-node values of the sample set are equal to 0
    print(paste("There are no k-nodes to examine at the: ","[ ",i," ]","th set.",sep=""))
  }
  else{ # Create the Fancyrpartplots
    tree<-rpart(form,data=pop,control = rpart.control(cp=1e-7,maxdepth = 5))
    saveRDS(tree,paste0("Case", i, ".Rds")) # Creates the "Case.Rds" file to be used in the Dynamic Decision Trees
    fancyRpartPlot(tree,uniform=TRUE, palettes = c("Greys","Oranges")) # Plots the Static Decision Trees
  }
}

