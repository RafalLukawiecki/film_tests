plot.film <- function() {
  
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

require(ggplot2)
print(plot.film())
