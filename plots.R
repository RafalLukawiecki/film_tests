plot.film.test <- function(film.data, 
                           title = "HD Film Plot", 
                           sensitometry = "Tests", 
                           log.e.offset = NULL,
                           df=5) {
  require(ggplot2)
  
  # If no log exposure offset vector provided, assume 0 offsets
  if(is.null(log.e.offset)) {
    log.e.offset <- rep(0, ncol(film.data)-1)
  }
  
  # Prepare the HD plot
  p <- ggplot(data=film.data, aes(x=He), environment=environment()) + 
    coord_equal() + 
    scale_x_continuous("rel log E", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,3,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_colour_discrete(name=sensitometry, 
                          breaks = names(film.data[2:ncol(film.data)]),
                          labels = names(film.data[2:ncol(film.data)])) +
    ggtitle(title)
  
  # Plot individual curves
  smooth.curves <- data.frame(He = seq(min(film.data$He), max(film.data$He), length=50))

  
  for(test in 2:ncol(film.data)) {
    
    # Because ggplot is a bit strange in how it parses variable names in function environments, we need to pass them as aes_string
    test.name <- names(film.data[test])
    rel.log.e <- paste("He + ", log.e.offset[test-1])
    
    # Find and fit a smoothing line using LOESS
    # smooth.fit <- loess(film.data[,test] ~ He, film.data, span=0.75)
    smooth.fit <- lm(film.data[,test] ~ bs(He+log.e.offset[test-1], df = df), data = film.data)
    smooth.curves <- cbind(smooth.curves, predict(smooth.fit, smooth.curves[1]))
   
    # Add the current curve to the plot
    p <- p + 
      
      # Plot the points
      geom_point(aes_string(x=rel.log.e, y=test.name, colour=paste('"', test.name,'"', sep=""))) + 
      
      # Plot the smoothing line
      geom_line(data=smooth.curves, aes_string(x=paste(smooth.curves[1], "+", log.e.offset[test-1]) ,
                                               y=smooth.curves[test], 
                                               colour=paste('"', test.name,'"', sep="")))
      
      # Plot the smoothing line
      #geom_smooth(aes_string(x=rel.log.e, y=test.name, colour=paste('"', test.name,'"', sep="")), 
      #            method="loess", se=FALSE)
  }
  
  summary(p)
  print(p)
}

plot.film.test(min.delta, "Delta 100 4x5 XTOL 1:1 20˚C 10min Tray", "Exposure\nEseco SL-2", log.e.offset=c(0.21,0,0))







plot.film <- function(film.data, title) {
  
  ggplot(film.data, aes(x=He)) + coord_equal() + 
    scale_x_continuous("rel log E", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,3,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    geom_point(aes(y=Green1Dh-min(Green1Dh), colour="A")) + 
    geom_smooth(aes(y=Green1Dh-min(Green1Dh), colour="A"), method="loess", se=FALSE) +
    geom_point(aes(x=He+0.21, y=Green2Dh-min(Green2Dh), colour="B")) + 
    geom_smooth(aes(x=He+0.21, y=Green2Dh-min(Green2Dh), colour="B"), method="loess", se=FALSE) +
    geom_point(aes(y=Blue1Dh-min(Blue1Dh), colour="C")) + 
    geom_smooth(aes(x=He, y=Blue1Dh-min(Blue1Dh), colour="C"), method="loess" , se=FALSE) +
    # geom_line(aes(x=He, y=Blue1Dh, colour="D"), data=d1) +
    scale_colour_discrete(name="Exposure\nEseco SL-2", 
                        breaks = c("A", "B", "C", "D"),
                        labels = c("Green x1", "Green x2", "Blue", "BlueSm")) +
    ggtitle(title)
}














prev.plot.film <- function() {
  
  
  d1 <- delta100.xtol11
  d1$Blue1Dh <- d1$Blue1Dh - min(d1$Blue1Dh)
  smoothBl <- loess(Blue1Dh ~ He, d1)
  
  
  d <- delta100.xtol11
  
  #require(reshape2)
  #d <- melt(delta100.xtol11, id.vars="")
  
  ggplot(d, aes(x=He)) + coord_equal() + 
    scale_x_continuous("rel log E", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,3,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    geom_point(aes(y=Green1Dh-min(Green1Dh), colour="A")) + 
    geom_smooth(aes(y=Green1Dh-min(Green1Dh), se=FALSE, colour="A")) +
    geom_point(aes(x=He+0.21, y=Green2Dh-min(Green2Dh), colour="B")) + 
    geom_smooth(aes(x=He+0.21, y=Green2Dh-min(Green2Dh), se=FALSE, colour="B")) +
    geom_point(aes(y=Blue1Dh-min(Blue1Dh), colour="C")) + 
    geom_smooth(aes(y=Blue1Dh-min(Blue1Dh), se=FALSE, colour="C")) +
    scale_colour_manual(name="Exposure\nEseco SL-2", 
                        breaks = c("A", "B", "C"),
                        labels = c("Green x1", "Green x2", "Blue"),
                        values = c("green3", "forestgreen", "blue")) +
    ggtitle("Delta 100 4x5 XTOL 1:1 20˚C 10min Tray")
}
