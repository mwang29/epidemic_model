library(animation)
case <- 1
for (disease in c("Influenza", "Smallpox", "Measles", "Ebola")){
  for (infnum in c(1, 1, 1, 3, 3, 3, 10, 10, 10)){
    file <- sprintf("Case%d.rda", case)
    file2 <- sprintf("Case%db.rda",case)
    gif <- sprintf("Case%d.gif", case)
    graphname <- sprintf("SIR(D) Model of %s within WL/L Population with %d initially infected individuals over 100 days", disease, infnum)
    load(file)
    pctSus <- (susceptible/totalPeople) * 100
    pctInf <- (numInfected/totalPeople)*100
    pctRecov <- (numRecovered/totalPeople)*100
    pctDead <- (numDead/totalPeople)*100 
    
    #After policy implementation
    load(file2)
    pctSus2 <- (susceptible/totalPeople) * 100
    pctInf2 <- (numInfected/totalPeople)*100
    pctRecov2 <- (numRecovered/totalPeople)*100
    pctDead2 <- (numDead/totalPeople)*100 
    
    totalPeople <- length(pop$ID)

    #Code is taken and modified from https://www.r-bloggers.com/animations-in-r/ to fit our model.
    
    animate <- function(duration, pctSus, pctInf, pctRecov, pctDead, pctSus2, pctInf2, pctRecov2, pctDead2){
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
        plot(pctSus[1:i], main = graphname, type = "l", xlab = "Day", ylab = "Percentage of Population", xlim = c(0,duration), ylim = c(0, 100), col = "lightblue")
        lines(pctInf[1:i], type = "l", col = "lightpink")
        lines(pctRecov[1:i], type = "l", col = "palegreen")
        lines(pctDead[1:i], type = "l", col = "lightgrey")
        lines(pctSus2[1:i], type = "l", col = "blue")
        lines(pctInf2[1:i], type = "l", col = "red")
        lines(pctRecov2[1:i], type = "l", col = "green")
        lines(pctDead2[1:i], type = "l", col = "black")
        legend(87.5, 100, legend=c("Suceptible", "Infected", "Recovered", "Dead"),
               col=c("blue", "red", "green", "black"), lwd= 1, cex=0.8)
      }
    }, movie.name = gif)
    }
  
  animate(100,pctSus,pctInf, pctRecov, pctDead, pctSus2, pctInf2, pctRecov2, pctDead2)
  #Animation code is taken from https://www.r-bloggers.com/animations-in-r/ and adapted to fit our model.
  case <- case + 1
  }
}
