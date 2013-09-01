# Plot film test characteristic curves and calculate Kodak-style CI (Contrast Index)
 
# Copyright 2013 (c) Rafal Lukawiecki photo@rafal.net

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details at:
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Version 0.2 2013-09-01

# **** START HERE:
# Currently, the algorithm fails if it starts the search for a solution at a "wrong" starting point.
# If CI values are not being computed (showing as NA), change the below starting point to a set of
# different rel log E exposure values.
starting.point = c(0.95, 1, 2)

# **** HOW TO USE:
# To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
# column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
# Every remaining column should contain density readings. Readings can be absolute (ie. including FB+F) or relative, the code below
# will subtract the smallest reading, assuming it is FB+F from each column, respectively, automatically.
# Make sure to name your columns something like "7 min" etc. If you do, and there are at least 2 columns, this program will attempt to plot
# a Dev Time/CI curve. It will also show, as lines on that plot, the target CIs for the different N-values.
# If your data is in a CSV that contains column headers (the first must be called He), or another text file, 
# use read.csv (or another read.table function) to put it in a data frame, for example:
# delta100.ddx1.4 <- read.csv("delta100.ddx1.4.csv")

# Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
# in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
# sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

# plot.film.test(delta100.ddx1.4) # This is the simplest way to use it. Or make it more complex:
# plot.film.etest(delta100.ddx1.4, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", "Exposure\nEseco SL-2\nGreen x2 + x6", 
#               log.e.offset=c(0,.21,0,.21,0,.21,0,.21,0,.21), df=7, dev.time.smoothing=2)

# This is the main function, that plots the curves, computes the CIs, and plots the Dev/CI chart (if enough data was provided)
plot.film.test <- function(film.data, # data frame as described above
                           title = "HD Film Plot", # Title for the plot
                           sensitometry = "Tests",   # Title for the legend
                           target.N.CIs=c(0.4, 0.5, 0.58, 0.7, 0.88), # Target CIs, will be drawn on CI/Dev plot and annotated as N-2,...,N+2
                           log.e.offset = NULL,  # Exposure offset vector, see above
                           df=6,  # Degrees of freedom for controlling how smooth (less) or fitted the Bezier spline ought to be
                           dev.time.smoothing=2 # Order of the smoothing polynomial for the development time/CI chart (use 1 for straight line)
                           )
  {  
  
  require(ggplot2)
  require(splines)
  
  # Assume that the lowest recorded reading in each column is FB+F, and subtract it column-wise, (He column should start at 0)
  film.data <- as.data.frame(apply(film.data, 2, function (x) x-min(x[1])))
  
  # If no log exposure offset vector provided, assume 0 offsets
  if(is.null(log.e.offset)) {
    log.e.offset <- rep(0, ncol(film.data)-1)
  }
  
  # Prepare the HD plot
  p <- ggplot(data=film.data, aes(x=He), environment=environment()) + 
    coord_equal() + 
    scale_x_continuous("rel log E", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10))
  
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
      geom_point(aes_string(x=rel.log.e, y=film.data[test], colour=paste('"', test.name,'"', sep=""))) + 

      # Plot the smoothing line
      geom_line(data=smooth.curves, aes_string(x=paste(smooth.curves[1], "+", log.e.offset[test-1]) ,
                                               y=smooth.curves[test], 
                                               colour=paste('"', test.name,'"', sep="")), alpha=0.7, size=0.7)
  }

  
  p <- p + scale_colour_discrete(name=sensitometry, 
                                 breaks = names(film.data[2:ncol(film.data)]),
                                 labels = paste(names(film.data[2:ncol(film.data)]), CIs, sep=" CI=")) +
    ggtitle(title)
  
  # If the columns represent development times, prepare a data frame of those times and corresponding CIs for a CI/dev plot.
  # If the titles cannot be parsed as having numerical meaning, print a warning, and plot nothing.
  dev.time <- sapply(names(film.data)[-1], function (x) as.numeric(unlist(strsplit(x, "[ \\.X\\-]"))[1]))
  if(length(dev.time) > 2) {
    if(any(is.na(dev.time)))
      print("Cannot extract development times from column names to generate a CI/Dev plot. Rename all your columns so they start with a number of development minutes, e.g. '7 min'")
    else {
      CI.dev <- data.frame(dev.time=dev.time, CI=CIs)
      dp <- ggplot(data=CI.dev, aes(x=dev.time, y=CI), environment=environment()) + 
        #coord_equal() + 
        scale_x_continuous("Dev Time", breaks=seq(0,120,5), minor_breaks=seq(0,120,1)) + 
        scale_y_continuous("CI", breaks=seq(0,3,0.1), minor_breaks=seq(0,3,0.02)) +
        geom_point() + 
        stat_smooth(method="lm", formula=y~poly(x,dev.time.smoothing), se=F) +
        geom_hline(yintercept=target.N.CIs, alpha=0.6, colour="blue", linetype="dashed") + 
        annotate("text", x=max(dev.time), y=target.N.CIs + 0.02, 
                 label=paste(c('N-2', 'N-1', 'N', 'N+1', 'N+2'), "(CI", target.N.CIs, ")"), size=3) +
        ggtitle(paste(title, "Development Time/CI Chart", sep="\n"))
        print(dp)
    }
  }
  
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
  
  if(roots$termcd == 1 & all(CI.points$He > 0) & all(CI.points$D > 0))
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

# plot.film.test(delta100.ddx1.4.x1, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", "Exposure\nEseco SL-2\nGreen x2 + x6", 
#             log.e.offset=c(0, .37, 0, .37, 0, .34, 0, .38, 0, .33, 0, .39), df=6)