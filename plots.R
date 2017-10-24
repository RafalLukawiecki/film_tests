# Plot film & paper test characteristic curves. Calculate Kodak-style CI (Contrast Index) for film curves, and plot zone system style (N, N+1) development times.
 
# Copyright 2017 (c) Rafal Lukawiecki photo@rafal.net

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
# Version 0.2 2013-09-01, Initial.
# Version 0.3 2014-05-18, Paper plotting added, corrected film plot labels.
# Version 0.4 2016-08-22, ggplot bug fixed, added combined film plotting (combines two test series, one with and one without an offset into one).
# Version 0.5 2017-10-24, Updated to use pacman to install package dependencies.

# **** START HERE:
# Currently, the film CI calculation algorithm fails if it starts the search for a solution at a "wrong" starting point.
# If CI values are not being computed (showing as NA), change the below starting point to a set of
# different rel log E exposure values. It is usually enough to nudge them a little by adding or subtracting 0.01 to each of those values.
starting.point = c(0.95, 1, 2)

# **** HOW TO USE FOR FILM:
# To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
# column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
# Every remaining column should contain density readings. Readings can be absolute (ie. including FB+F) or relative, the code below
# will subtract the smallest reading, assuming it is FB+F from each column, respectively, automatically.
# Make sure to name your columns something like "7 min" etc. If you do, and there are at least 2 columns, this program will attempt to plot
# a Dev Time/CI curve. It will also show, as lines on that plot, the target CIs for the different N-values.
# If your data is in a CSV that contains column headers (the first must be called He), or another text file, 
# use read.csv (or another read.table function) to put it in a data frame, for example, using provided sample readings
#
# delta100.ddx1.4 <- read.csv("delta100.ddx1.4.csv")
# plot.film.test(delta100.ddx1.4, title = "Delta 100 4x5 DDX 1+4 20˚C CombiPlan\nAgitation 3 inversions at 30s intervals", 
#                sensitometry = "Exposure\nEseco SL-2\nGreen x2 x6", log.e.offset=0.42, df=6, combined.pairwise.plot = TRUE)

# Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
# in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
# sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

# plot.film.test(delta100.ddx1.4) # This is the simplest way to use it. Or make it more complex:
# plot.film.test(delta100.ddx1.4, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", "Exposure\nEseco SL-2\nGreen x2 + x6", 
#               log.e.offset=c(0,.21,0,.21,0,.21,0,.21,0,.21), df=7, dev.time.smoothing=2)

# This is the main function, that plots the curves, computes the CIs, and plots the Dev/CI chart (if enough data was provided)
plot.film.test <- function(film.data, # data frame as described above
                           title = "HD Film Plot", # Title for the plot
                           sensitometry = "Tests",   # Title for the legend
                           target.N.CIs=c(0.4, 0.5, 0.58, 0.7, 0.88), # Target CIs, will be drawn on CI/Dev plot and annotated as N-2,...,N+2
                           log.e.offset = NULL,  # Exposure offset vector, when providing pairs of series, for example exposed with a longer time, to simulate a longer step tablet
                                                 # Please note that when combined.pairwise.plot is TRUE, then log.e.offset must be specified as a single > 0 value, not a vector.
                           df=6,  # Degrees of freedom for controlling how smooth (less) or fitted the Bezier spline ought to be
                           dev.time.smoothing=2, # Order of the smoothing polynomial for the development time/CI chart (use 1 for straight line)
                           combined.pairwise.plot = FALSE # If a vector of offsets is provided, should the neighboring pairs of series be combined together (TRUE) or plotted separately (FALSE)
                           )
  {  
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot2, splines, dplyr, nleqslv)
  
  # Assume that the lowest recorded reading in each column is FB+F, and subtract it column-wise, (He column should start at 0)
  film.data <- as.data.frame(apply(film.data, 2, function (x) x-min(x[1])))
  
  # If no log exposure offset vector provided, assume 0 offsets
  if(is.null(log.e.offset)) {
    log.e.offset <- rep(0, ncol(film.data)-1)
    combined.pairwise.plot <- FALSE   # Combined plots only make sense when an offset has been specified 
  } else 
    if(combined.pairwise.plot && (length(log.e.offset) != 1 || log.e.offset <= 0 || ((ncol(film.data)-1) %% 2) != 0)) 
      stop("When combined.pairwise.plot is TRUE you need to provide a single, >0 value for log.e.offset parameter, and an even number of test series columns (ie. pairs).")

  if(combined.pairwise.plot) {
    # Combine, pair-wise, the test data series
    film.data.combined <- bind_rows(select(film.data, 1), select(film.data, 1) + log.e.offset)
    for(test.series in 1:((ncol(film.data)-1)/2)) {
        combined.series <- data_frame(c(film.data[,2+2*(test.series-1)], film.data[,3+2*(test.series-1)]))
        names(combined.series) <- names(film.data)[2+2*(test.series-1)]
        film.data.combined <- bind_cols(film.data.combined, combined.series)
    }
    
    film.data <- as.data.frame(film.data.combined)   # Need to coerce this way to avoid later issues with lm
    log.e.offset <- rep(0, ncol(film.data)-1)
  }
  
  # Prepare the HD plot
  # Many thanks to Stephen Benskin from APUG and LFPP for pointing out that the X axis is rel log H and not rel log E (as was in version 0.2)
  # p <- ggplot(data=film.data, aes(x=He), environment=environment()) + 
  p <- ggplot(data=film.data, aes(x=He), environment=environment()) + 
    coord_equal() + 
    scale_x_continuous("rel log H", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density above fb+f", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10))
  
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
      geom_point(aes_string(x=rel.log.e, y=film.data[,test], colour=paste('"', test.name,'"', sep=""))) +

      # Plot the smoothing line
      geom_line(data=smooth.curves, aes_string(x=paste(smooth.curves[1], "+", log.e.offset[test-1]) ,
                                               y=smooth.curves[,test],
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


# **** HOW TO USE FOR PAPER:
# To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
# column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
# Every remaining column should contain density readings. Readings can be absolute (ie. including FB+F) or relative, the code below
# will subtract the smallest reading, assuming it is FB+F from each column, respectively, automatically.
# Make sure to name your columns something like "7 min" etc. If you do, and there are at least 2 columns, this program will attempt to plot
# a Dev Time/CI curve. It will also show, as lines on that plot, the target CIs for the different N-values.
# If your data is in a CSV that contains column headers (the first must be called He), or another text file, 
# use read.csv (or another read.table function) to put it in a data frame, for example:
# mgwt.ds.14r.r.apo.n.150.vcce <- read.csv("mgwt.ds-14r.r-apo-n-150.vcce.csv", row.names=1)

# Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
# in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
# sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

# plot.film.test(delta100.ddx1.4) # This is the simplest way to use it. Or make it more complex:
# plot.film.test(delta100.ddx1.4, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", "Exposure\nEseco SL-2\nGreen x2 + x6", 
#               log.e.offset=c(0,.21,0,.21,0,.21,0,.21,0,.21), df=7, dev.time.smoothing=2)

# This is the main function, that plots the curves, computes the CIs, and plots the Dev/CI chart (if enough data was provided)
plot.paper.test <- function(paper.data, # data frame as described above
                           title = "HD Paper Plot", # Title for the plot
                           sensitometry = "Tests",   # Title for the legend
                           log.e.offset = NULL,  # Exposure offset vector, see above, cannot be used with equaliseHighlights (because that options autocalculates the offsets)
                           equalHighlights = FALSE, # Automatically calculates the exposure offsets so that the given highlight density is matched across all settings
                           referenceHighlight = 0.1, # Density (above base + fog) of a highlight to which curve exposures should be adjusted if above option is used
                           IDmin = NULL, # ISO IDmin, if none provided 0.04 over base + fog will be used
                           IDmax = NULL, # ISO IDmax, if none provided will calculate as 90% of actual Dmax over base + fog, using highest provided reading 
                           method="LOESS", # Smoothing method, select between "LOESS" or "Bezier"
                           span=0.2, # Degree of smoothing when using LOESS for fitting curves
                           df=9  # Degrees of freedom for controlling how smooth (less) or fitted the Bezier spline ought to be
)
{  
  
  if(! is.null(log.e.offset) & equalHighlights)
    stop("You cannot use options log.e.offset and equalHighlights at the same time. Please use either but not both.")
  
  should.print.offsets <- !is.null(log.e.offset) || equalHighlights
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot2, splines)
  
  # Assume that the lowest recorded reading in each test column is PB+F, and subtract it column-wise.
  paper.data[2:ncol(paper.data)] <- as.data.frame(apply(paper.data[2:ncol(paper.data)], 2, function (x) x-min(x)))
  
  # Convert TabletDens column to Rel Paper Exposure (He)
  paper.data$TabletDens <- max(paper.data$TabletDens) - paper.data$TabletDens
  names(paper.data)[1] <- "He"
  
  # Calculate ISO-R style IDmax and set IDmin if not provided
  if(is.null(IDmax))
    IDmax <- 0.9 * max(paper.data[2:ncol(paper.data)])
  if(is.null(IDmin))
    IDmin = 0.04
  maxHe = max(paper.data$He)
  
  # If no log exposure offset vector provided, assume 0 offsets
  if(is.null(log.e.offset)) {
    log.e.offset <- rep(0, ncol(paper.data)-1)
  }
  
  # Prepare the HD plot
  # Many thanks to Stephen Benskin from APUG and LFPP for pointing out that the X axis is rel log H and not rel log E (as was in version 0.2)
  p <- ggplot(data=paper.data, aes(x=He), environment=environment()) + 
    coord_equal() + 
    scale_x_continuous("rel log H", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10)) + 
    scale_y_continuous("Density", breaks=seq(0,4,0.30), minor_breaks = seq(0, 4, 0.10))
  
  # Plot individual curves, calculate ISO-Rs for each curve.
  ISORs <- c()
  
  # Prepare smoothing data, over 50 point samples between the provided rel log E range 
  smooth.curves <- data.frame(He = seq(min(paper.data$He), max(paper.data$He), length=50))
  
  curve.models <- list()
  
  # Build the curves for each set of test data points
  for(test in 2:ncol(paper.data)) {
    
    offset <- log.e.offset[test-1]
    
    # Find and fit a smoothing line
    if(method=="LOESS")
      smooth.fit <- loess(paper.data[,test] ~ I(He+offset), paper.data, span=span)   # LOESS variant, note the default span smoothing value
    else
      smooth.fit <- lm(paper.data[,test] ~ bs(He+offset, df = df), data = paper.data)  # Bezier splines, note the default degrees of freedom
    
    # Store the model
    curve.models <- append(curve.models, list(smooth.fit))
    
    # Calculate a smooth curve using its model
    smooth.curves <- cbind(smooth.curves, predict(smooth.fit, smooth.curves[1]))
    
    # Calculate ISO-R for the curve
    ISORs <- append(ISORs, round(ISO.R(smooth.fit, offset, IDmin, IDmax, maxHe)))
  }
  
  # If option parameter equalHighlights is set to TRUE, process all curves to find out the exposure offsets so that they print the referenceHighlight the same density
  if(equalHighlights) {
    # First, calculate the exposures that yield the target highlight density for each curve
    highlightExposures <- sapply(curve.models, exposure.for.density, referenceHighlight, maxHe)
    
    # Find the calculated target highlight exposure (aka reference exposure) of the first curve
    referenceExposure <- highlightExposures[1]

    # For each curve, calculate the additional exposure needed (offset) from the reference exposure
    log.e.offset <- -(highlightExposures - referenceExposure)
  }
  
  # Plot the curves
  for(test in 2:ncol(paper.data)) {
    # Because ggplot is a bit strange in how it parses variable names in function environments, we need to pass them as aes_string
    test.name <- names(paper.data[test])
    rel.log.e <- paste("He + ", log.e.offset[test-1])
    
    # Add each curve to the plot
    p <- p + 
      
      # Plot the points
      geom_point(aes_string(x=rel.log.e, y=paper.data[test], colour=paste('"', test.name,'"', sep=""))) + 
      
      # Plot the smoothing line
      geom_line(data=smooth.curves, aes_string(x=paste(smooth.curves[1], "+", log.e.offset[test-1]) ,
                                               y=smooth.curves[test], 
                                               colour=paste('"', test.name,'"', sep="")
                                               ), linetype=1+(substring(test.name,1,1)=="D"), alpha=0.7, size=0.7)
  }
  
  # Convert ISO-Rs to ISO Paper Grade Numbers
  ISOGrades <- ISORtoGrade(ISORs)
  
  # Calculate the exposure adjustment from preceding curve as an f-stop adjustments in fractions of 1/12
  exp.adjustments <- fractions(round(-(log.e.offset - c(0,log.e.offset[1:length(log.e.offset)-1])) / 0.025, 0) / 12, # 0.025 log density is 1/12 f-stop
                              max.denominator=13)
  
  if(should.print.offsets) {
    p <- p + scale_colour_discrete(name=sensitometry, 
                                   breaks = names(paper.data[2:ncol(paper.data)]),
                                   labels = paste(names(paper.data[2:ncol(paper.data)]), " Total exp offset ", round(log.e.offset, 2), " or ",
                                                  ifelse(as.numeric(exp.adjustments) >= 0, "+", ""), exp.adjustments, " f of above.",
                                                  " ISO-R=", ISORs, " Grade=", ISOGrades, sep="")) +
      ggtitle(title)
  } else {
    p <- p + scale_colour_discrete(name=sensitometry, 
                                   breaks = names(paper.data[2:ncol(paper.data)]),
                                   labels = paste(names(paper.data[2:ncol(paper.data)]), " ISO-R=", ISORs, " Grade=", ISOGrades, sep="")) +
      ggtitle(title)
  }
    
  print(p)
  
}


# Computes the Kodak-style CI Contrast Index
contrast.index <- function(curve.model, straight.line.coeff, log.e.offset = 0) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(nleqslv)
  
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


# Compute ISO-R contrast range
ISO.R <- function(curve.model, log.e.offset = 0, IDmin, IDmax, maxHe) {
  # Find Ht and Hs from the curve
  fHt <- function(x)
    predict(curve.model, data.frame(He=c(x-log.e.offset)))[1] - IDmin
  
  fHs <- function(x)
    predict(curve.model, data.frame(He=c(x-log.e.offset)))[1] - IDmax
  
  Ht <- uniroot(fHt, lower=0.01+log.e.offset, upper=maxHe+log.e.offset, extendInt="upX", tol=0.01)$root
  Hs <- uniroot(fHs, lower=0.01+log.e.offset, upper=maxHe+log.e.offset, extendInt="upX", tol=0.01)$root
  
  return(100*(Hs-Ht))
}


# Find exposure needed to print a given density using a curve
exposure.for.density <- function(curve.model, density, maxHe) {
  fDensity <- function(x)
    predict(curve.model, data.frame(He=c(x)))[1] - density
  
  return(uniroot(fDensity, lower=0.01, upper=maxHe, extendInt="upX", tol=0.01)$root)
}

# Convert the calculated ISO-R into an ANSI/ISO grade (halves, which are not in ISO standard, selected at interval midpoints) 
ISORtoGrade <- function(ISOR) {
  return(cut(ISOR, breaks=c(0,35,42,50,57,65,72,80,87,95,104,115,127,140,154,170,500), labels=c(7,6.5,6,5.5,5,4.5,4,3.5,3,2.5,2,1.5,1,0.5,0,-1)))
}


# Helper function, used to splice together two sets of readings, whereas one is "normal exposure" and the other one represents an extended (multiple) exposure
# which is useful for testing ranges longer than the step tablet allows, for example when analysing shoulder of the film curve. To use, you need to find an
# exposure.offset value that makes the extended curve sit "on top" of the normal range one. You can do that visually, by using different offset values passed
# as a parameter to plot.film.test function.


# plot.film.test(delta100.ddx1.4.x1, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", "Exposure\nEseco SL-2\nGreen x2 + x6", 
#             log.e.offset=c(0, .37, 0, .37, 0, .34, 0, .38, 0, .33, 0, .39), df=6)