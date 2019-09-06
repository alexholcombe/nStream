#Polar angular values for the different spacings and offsets

library(ggplot2)

streamPositions <- function(nStreams, spacing, offset){
  positions <- numeric(nStreams)
  for(stream in 1:nStreams){
    angle <- spacing*(stream-1)+offset
    positions[stream] <- angle
  }
  return(positions)
}

plotPoints <- expand.grid(
  nStream = c(2,6),
  stream = 1:6,
  spacing = c(60,30,20),
  offset = c(0,60,120,180,240,300),
  x = -999,
  y = -999
)

for(nStream in c(2,6)){
  for(spacing in c(60,30,20)){
    for(offset in seq(0, 360,60)){
      if(offset != 360){
        positions <- streamPositions(nStream,spacing,offset)
        x <- cos(positions/180*pi)*6
        y <- sin(positions/180*pi)*6
        
        if(nStream == 2){
          x <- c(x, NA,NA,NA,NA)
          y <- c(y,NA,NA,NA,NA)
        }
        
        plotPoints$x[plotPoints$nStream == nStream & plotPoints$spacing==spacing & plotPoints$offset==offset] <- x
        plotPoints$y[plotPoints$nStream == nStream & plotPoints$spacing==spacing & plotPoints$offset==offset] <- y
      }
    }
  }
}

ggplot(plotPoints, aes(x = x, y=y))+
  geom_point(aes(colour = factor(offset)))+
  facet_wrap(~nStream+spacing)
