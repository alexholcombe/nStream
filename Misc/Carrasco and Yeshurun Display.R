###Carrasco and Yeshurun display###

library(ggplot2)
library(dplyr)
library(magrittr)

possibleCoord <- c(
  -1, 
  -3,
  -5,
  1,
  3,
  5
)

thesePoints <- expand.grid(
  x = possibleCoord,
  y = possibleCoord
)

thesePoints %<>% mutate(distance = sqrt(x^2+y^2))
thesePoints %<>% mutate(distance = round(distance, digits = 1))

ggplot(thesePoints, aes(x=x,y=y))+
  geom_text(aes(label = paste0(distance,'º')))+
  geom_point(x=0,y=0)+
  scale_x_continuous(breaks = seq(-6,6,2), limits =  c(-6,6))+
  scale_y_continuous(breaks = seq(-6,6,2), limits = c(-6,6))

