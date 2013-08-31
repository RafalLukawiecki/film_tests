# Plot film test characteristic curves and calculate Kodak-style CI (Contrast Index)
# Rafal Lukawiecki photo@rafal.net 2013-08-21

Dd <- function(x, curve.model, log.e.offset=0) 
  predict(curve.model, data.frame(He=c(x-log.e.offset)))[1]


ci.equation <- function(x, curve.model, log.e.offset = 0) {
  
  D <- function(x) 
    predict(curve.model, data.frame(He=c(x-log.e.offset)))[1]
  
  c(
    F1 = (x[2] - x[1])^2 + D(x[2])^2 - 0.2^2,
    F2 = (x[3] - x[2])^2 + (D(x[3]) - D(x[2]))^2 - 2^2,
    F3 = (x[3] - x[1])^2 + D(x[3])^2 - 2.2^2
  )
  
}


contrast.index <- function(curve.model, log.e.offset = 0) {
  
  require(rootSolve)
  
  roots <- multiroot(f=ci.equation, start=c(0.85, 1, 2.63), 
                     positive=TRUE, maxiter=1000, ctol=0.05, curve.model=curve.model, log.e.offset=log.e.offset)
  CI.points <- data.frame(He=c(roots$root[2:3]) - log.e.offset)
  CI.points$D <- predict(curve.model, CI.points[1])
  
  print(roots)
  print(CI.points)
  
  return(
    (CI.points$D[2] - CI.points$D[1]) / (CI.points$He[2] - CI.points$He[1])
  )
  
}


plot.film.test <- function(film.data, 
                           title = "HD Film Plot", 
                           sensitometry = "Tests", 
                           log.e.offset = NULL,
                           df=5) {
  
  require(ggplot2)
  require(splines)
  
  # If no log exposure offset vector provided, assume 0 offsets
  if(is.null(log.e.offset)) {
    log.e.offset <- rep(0, ncol(film.data)-1)
  }
  
  # Prepare the HD plot
  p <- ggplot(data=film.data, aes(x=He), environment=environment()) + 
    coord_equal() + 
    scale_x_continuous("rel log E", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,3,0.30), minor_breaks = seq(0, 4, 0.10))
  
  # Plot individual curves
  
  # Prepare smoothing data, over 50 point samples between the provided rel log E range 
  smooth.curves <- data.frame(He = seq(min(film.data$He), max(film.data$He), length=50))
  CIs <- c()

  for(test in 2:ncol(film.data)) {
    
    # Because ggplot is a bit strange in how it parses variable names in function environments, we need to pass them as aes_string
    test.name <- names(film.data[test])
    rel.log.e <- paste("He + ", log.e.offset[test-1])
    
    # Find and fit a smoothing linr
    #smooth.fit <- loess(film.data[,test] ~ He, film.data, span=0.75)   # LOESS variant
    smooth.fit <- lm(film.data[,test] ~ bs(He+log.e.offset[test-1], df = df), data = film.data)  # Bezier splines, note the default degrees of freedom
    smooth.curves <- cbind(smooth.curves, predict(smooth.fit, smooth.curves[1]))
    
    # Calculate CI for the curve, and store for now
    CIs <- append(CIs, round(contrast.index(smooth.fit, log.e.offset[test-1]), 2))
   
    # Add the current curve to the plot
    p <- p + 
      
      # Plot the points
      geom_point(aes_string(x=rel.log.e, y=test.name, colour=paste('"', test.name,'"', sep=""))) + 
      
      # Plot the smoothing line
      geom_line(data=smooth.curves, aes_string(x=paste(smooth.curves[1], "+", log.e.offset[test-1]) ,
                                               y=smooth.curves[test], 
                                               colour=paste('"', test.name,'"', sep="")))
  }

  
  p <- p + scale_colour_discrete(name=sensitometry, 
                                 breaks = names(film.data[2:ncol(film.data)]),
                                 labels = paste(names(film.data[2:ncol(film.data)]), CIs, sep=" CI=")) +
    ggtitle(title)
  
  
  print(p)
}

#print(contrast.index(d, 0.21))

plot.film.test(min.delta, "Delta 100 4x5 XTOL 1:1 20˚C 10min Tray", "Exposure\nEseco SL-2", log.e.offset=c(0.21,0,0), df=5)

# $x [1] 0.8383949 1.0010792 2.6281244





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
