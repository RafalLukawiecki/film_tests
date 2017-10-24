# Film and Paper Sensitometric Test Curve Calculator and Plotter
## Calculate and plot film and paper characteristic curves and compute N, N+1 etc Zone System development times 

The main purpose of this code is to help plot and calculate film and paper [sensitometric](https://en.wikipedia.org/wiki/Sensitometry) 
characteristic (HD) curves. This code will also interpret the curves
to find the optimal development time, based on Kodak Contrast Index (CI) values mentioned in ["Way Beyond Monochrome"](https://www.amazon.co.uk/Way-Beyond-Monochrome-Traditional-Photography/dp/1138297372) 
2nd Ed by Ralph Lambrecht and Chris Woodhouse. There are additional functions that paper-related functions provide, including the
ability to calculate a table of offsets to calibrate VCCE (variable contrast constant exposure) type of enlarger heads, such as the
LPL VCCE. A benefit of such a calibration table is that you know how much to adjust the exposure duration as you increase or decrease
the grade while maintaining identical tone of the selected density.

For more information about the origin of this code and a discussion about it, please see the following thread 
on [APUG/Photrio](https://www.photrio.com/forum/threads/algorithm-numerical-approach-for-computing-ci-contrast-index.106414/), or submit an issue using GitHub. Comments 
and pull requests are welcome.

To see photography that has been improved thanks to this code, please [visit my site](http://rafal.net).

### How to use for film testing
To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
Every remaining column should contain density readings. Readings can be absolute (ie. including FB+F) or relative, the code below
will subtract the smallest reading, assuming it is FB+F from each column, respectively, automatically.
Make sure to name your columns something like "7 min" etc. If you do, and there are at least 2 columns, this program will attempt to plot
a Dev Time/CI curve. It will also show, as lines on that plot, the target CIs for the different N-values.
If your data is in a CSV that contains column headers (the first must be called He), or another text file, 
use read.csv (or another read.table function) to put it in a data frame.

Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

```
delta100.ddx1.4 <- read.csv("delta100.ddx1.4.csv")
plot.film.test(delta100.ddx1.4) # This is the simplest way to use it. Or make it more complex:
plot.film.test(delta100.ddx1.4, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s", 
              "Exposure\nEseco SL-2\nGreen x2 + x6", 
              log.e.offset=c(0,.21,0,.21,0,.21,0,.21,0,.21), df=7, dev.time.smoothing=2)
plot.film.test(delta100.ddx1.4, title = "Delta 100 4x5 DDX 1+4 20˚C CombiPlan\nAgitation 3 inversions at 30s intervals", 
               sensitometry = "Exposure\nEseco SL-2\nGreen x2 x6", 
               log.e.offset=0.42, df=6, combined.pairwise.plot = TRUE)
```

### How to use for paper testing
To use, call as below, providing your densitometer readings in the first parameter, as data frame, which must contain as the first
column, named "He" your relative log E values (from your sensitometer etc), starting at 0 and increasing towards the right.
Every remaining column should contain density readings. Readings can be absolute (ie. including FB+F) or relative, the code below
will subtract the smallest reading, assuming it is FB+F from each column, respectively, automatically.
Make sure to name your columns something like "7 min" etc. If you do, and there are at least 2 columns, this program will attempt to plot
a Dev Time/CI curve. It will also show, as lines on that plot, the target CIs for the different N-values.
If your data is in a CSV that contains column headers (the first must be called He), or another text file, 
use read.csv (or another read.table function) to put it in a data frame, for example:
```
mgwt.ds.14r.r.apo.n.150.vcce <- read.csv("mgwt.ds-14r.r-apo-n-150.vcce.csv", row.names=1)
```

Optionally, provide a vector of exposure offsets as the log.e.offset parameter. This is used to indicate that a series of readings
in the corresponding column represents exposures greater by the value of the offset. This is useful if you want to take a second
sensitometric exposure which is larger, by that offset, to effectively compute a longer curve, without using a longer step tablet.

```
plot.film.test(delta100.ddx1.4) # This is the simplest way to use it. Or make it more complex:
plot.film.test(delta100.ddx1.4, "Delta 100 4x5 DDX 1+4 20˚C CombiPlan 30s/3inv.5s@30s",
              "Exposure\nEseco SL-2\nGreen x2 + x6", 
              log.e.offset=c(0,.21,0,.21,0,.21,0,.21,0,.21), df=7, dev.time.smoothing=2)
```
