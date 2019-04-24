library(manipulate)
library(ggplot2)

manipulate({
  data <- data.frame(
    x = seq(0,10,.1),
    y = dgamma(seq(0,10,.1), shape = s, rate = r)
  )
  ggplot(data = data, aes(x = x, y =y))+
    geom_line()+
    scale_x_continuous(breaks = 1:10)+
    theme_classic()},
  s=slider(2,10,step=.1),
  r=slider(0,2,step=.1)
  )
