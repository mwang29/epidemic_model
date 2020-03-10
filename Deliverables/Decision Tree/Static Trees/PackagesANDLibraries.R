#-----------------------Packages------------------------------------

install.packages("RMySQL")
install.packages("rpart")
install.packages("fancyRPartPlot")
install.packages("shiny")
install.packages("rpart.plot")
install.packages("rpart.utils")
install.packages("knitr")
install.packages("dplyr")
install.packages("rattle")
install.packages("rpart.tree")
install.packages("caret")
install.packages("party")
install.packages("partykit")
install.packages("RColorBrewer")
install.packages("rio")
install.packages('rstan')
install.packages("V8")
install.packages("shinythemes")
install.packages("shinyBS")
install.packages("stringr")
install.packages("shinyjs")

#------------------------------------------------------------------- 

#-----------------------Libraries-----------------------------------

library(dplyr)
library(rpart.utils)
library(shiny)
library(rpart)				  # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)			# Enhanced tree plots
library(RColorBrewer)		# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
library(rio)            #input/output 
library(rstan)
library(shinythemes)
library(shinyBS)
library(V8)            # Needed by shinyjs; allows server to run javascript
library(shinyjs)
library(stringr)
install_formats()       # Only run this package once to install formats

#------------------------------------------------------------------- 