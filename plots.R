# Plot film test characteristic curves and calculate Kodak-style CI (Contrast Index)
# Rafal Lukawiecki photo@rafal.net 2013-08-21

# Currently, the algorithm fails if it starts the search for a solution at a "wrong" starting point.
# If CI values are not being computed (showing as NA), change the below starting point to a set of
# different rel log E exposure values.
starting.point = c(0.9, 1, 2)

# To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
# column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
# Every remaining column should contain density readings. Name those columns in a way that describes your test, eg. "11 min" etc.
# Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
# in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
# sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

plot.film.test(min.delta, "Delta 100 4x5 XTOL 1:1 20˚C 10min Tray", "Exposure\nEseco SL-2", log.e.offset=c(0.21,0,0), df=7)



# This is the main function, that plots the curves and computes the CIs
plot.film.test <- function(film.data, # data frame as described above
                           title = "HD Film Plot", # Title for the plot
                           sensitometry = "Tests",   # Title for the legend
                           log.e.offset = NULL,  # Exposure offset vector, see above
                           df=5  # Degrees of freedom for controlling how smooth (less) or fitted the Bezier spline ought to be
                           )
  {  
  
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
    
    # Find and fit a smoothing line
    #smooth.fit <- loess(film.data[,test] ~ He, film.data, span=0.75)   # LOESS variant
    smooth.fit <- lm(film.data[,test] ~ bs(He+log.e.offset[test-1], df = df), data = film.data)  # Bezier splines, note the default degrees of freedom
    smooth.curves <- cbind(smooth.curves, predict(smooth.fit, smooth.curves[1]))
    
    # Calculate CI for the curve, and store for now
    # To estimate the starting point of the search for the CI we pass a straight line regression of the data

    straight.line <- lm(y ~ x, data.frame(x=film.data$He+log.e.offset[test-1], y=film.data[,test]))
    CIs <- append(CIs, round(contrast.index(smooth.fit, straight.line$coefficients, log.e.offset[test-1]), 2))
   
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


# Computes the Kodak-style CI Contrast Index
contrast.index <- function(curve.model, straight.line.coeff, log.e.offset = 0) {
  
  require(nleqslv)
  
  # Estimate a good starting point for the search for the CI equation condition, as nleqslv is sensitive to starting parameters.
  # Use a straight-line regression of the curve points as a way to estimate the starting points, as it is known they have to be
  # spaced at 0 - 0.2 - 2.2 distances in 2d space, assuming that the curve is unlikely to be steeper than gradient=1.
  
  # TODO: Automatically vary the starting point if search fails, and try again?
  
  roots <- nleqslv(x=starting.point, fn=ci.equation, curve.model=curve.model, log.e.offset=log.e.offset,
                   control=list(maxit=300))
  
  CI.points <- data.frame(He=c(roots$x[2:3]) - log.e.offset)
  CI.points$D <- predict(curve.model, CI.points[1])
  
  if(roots$termcd == 1 & CI.points$He > 0 & CI.points$D > 0)
    return((CI.points$D[2] - CI.points$D[1]) / (CI.points$He[2] - CI.points$He[1]))
  else {
    print("Error: Cannot compute CI for some or all curves. Consider changing the 'starting.point' as described in the source file comment.")
    return(NA)
  }
}


# Helper function, represents the system of non-linear equations based on Pythagorean triangle relationship
# that describes the conditions of the CI measurement in terms of distances along concentric arcs
# Many thanks to David dpgoldenberg on APUG.org for pointing this out.

ci.equation <- function(x, curve.model, log.e.offset = 0) {
  
  D <- function(x) 
    predict(curve.model, data.frame(He=c(x-log.e.offset)))[1]
  
  c(
    F1 = (x[2] - x[1])^2 + D(x[2])^2 - 0.2^2,
    F2 = (x[3] - x[2])^2 + (D(x[3]) - D(x[2]))^2 - 2^2,
    F3 = (x[3] - x[1])^2 + D(x[3])^2 - 2.2^2
  )
  
}