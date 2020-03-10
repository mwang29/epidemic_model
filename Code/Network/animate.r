indivData <- read.csv("Individual.csv", header = T, stringsAsFactors = F)
totalPeople <- length(indivData$Person_ID)
pctSusc <- (susceptible/totalPeople) * 100
pctInf <- (numInfected/totalPeople)*100
pctRecov <- (numRecovered/totalPeople)*100
pctDead <- (numDead/totalPeople)*100 
#Code is taken and modified from https://www.r-bloggers.com/animations-in-r/ to fit our model.

animate <- function(duration, pctSus, pctInf, pctRecov, pctDead){
#Set delay between frames when replaying
ani.options(interval=.05)
# Set up a vector of colors for use below 
# Begin animation loop
# Note the brackets within the parentheses
saveGIF({
  
  # For the most part, it’s safest to start with graphical settings in 
  # the animation loop, as the loop adds a layer of complexity to 
  # manipulating the graphs. For example, the layout specification needs to 
  # be within animation loop to work properly.
  layout(matrix(c(1, rep(2, 5)), 6, 1))
  # Adjust the margins a little
  par(mar=c(4,4,2,1) + 0.1)
  # Begin the loop that creates the 150 individual graphs
  for (i in 1:duration) {
    # Reset the color of the top chart every time (so that it doesn’t change as the 
    # bottom chart changes)
    par(fg=1)
    # Set up the top chart that keeps track of the current frame/iteration
    plot(-5, xlim = c(1,duration), ylim = c(0, .3), axes = F, xlab = "", ylab = "", main = "Day")
    abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    # Bring back the X axis
    axis(1)
    # Set up the bottom chart
    plot(pctSus[1:i], main = "SIR Plot of West Lafayette/Lafayette Population over 100 days", type = "l", xlab = "Day", ylab = "Percentage of Population", xlim = c(0,duration), ylim = c(0, 100), col = "blue")
    lines(pctInf[1:i], type = "l", col = "red")
    lines(pctRecov[1:i], type = "l", col = "green")
    lines(pctDead[1:i], type = "l", col = "black")
    legend(87.5, 100, legend=c("Suceptible", "Infected", "Recovered", "Dead"),
           col=c("blue", "red", "green", "black"), lwd= 1, cex=0.8)
  }
})
}

animate(100,pctSus, pctSus, pctInf, pctRecov, pctDead)
#Code is taken and modified from https://www.r-bloggers.com/animations-in-r/ to fit our model.
