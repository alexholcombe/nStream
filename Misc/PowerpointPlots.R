rm(list=ls())
library(ggplot2)
library(papaja)
library(magrittr)
library(dplyr)
library(truncnorm)

PowerPointSize <- c(29.21, 12.09)
PowerPointUnit <- 'cm'

xlims <- c(-6,10)

plotOne <- ggplot()+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  scale_x_continuous(breaks = 0, labels = c('cue'),limits = xlims)+
  labs(x = 'Time')+
  theme_apa()
plotOne
 

plotTwoData <- data.frame(x = seq(-6,10,.05))

plotTwoData %<>% mutate(logDensity = dlnorm(x, meanlog = .5, sdlog = .6))
logSample <- data.frame(x=c(rlnorm(200, meanlog = .5, sdlog = 1.3), rep(0, times= 13)))

plotTwo <- ggplot(data = logSample, aes(x = x))+
  geom_histogram()+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  scale_x_continuous(breaks = 0, labels = c('cue'),limits = xlims)+
  labs(x = 'Time')+
  theme_apa()
plotTwo

plotThreeData <- data.frame(x = seq(-6,10,.05))

plotThreeData %<>% mutate(
  uniform = dunif(x, -6,10),
  normal = dnorm(x, 90/83.33, 76/83.33)
)

plotThree <- ggplot(data = plotThreeData, aes(x = x))+
  geom_area(aes(y = uniform), fill = '#628093')+
  geom_area(aes(y = normal), fill = '#dca951')+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  lims(x = c(-3, 6))+
  theme_apa()
plotThree

ggsave(plotThree,filename = '~/PlotThree.png', width = PowerPointSize[1], height = PowerPointSize[2], units = PowerPointUnit)


plotFour<- ggplot(data = plotThreeData, aes(x = x))+
  geom_area(aes(y = uniform), fill = '#628093')+
  geom_area(aes(y = normal), fill = '#dca951')+
  lims(x = c(-3, 6))+
  theme_apa()
plotFour

ggsave(plotFour,filename = '~/PlotFour.png', width = PowerPointSize[1], height = PowerPointSize[2], units = PowerPointUnit)


plotFiveData <- data.frame(x = seq(-6,10,.05))

plotFiveData %<>% mutate(
  uniform = dunif(x, -6,10),
  normal = dnorm(x, 90/83.33, 76/83.33),
  trunNormal = dtruncnorm(x, 90/83.33, 76/83.33,a = 14/83.33, b = Inf)
)

plotFive <- ggplot(data = plotFiveData, aes(x = x))+
  geom_area(aes(y = uniform), fill = '#628093')+
  geom_area(aes(y = trunNormal), fill = '#dca951')+
  lims(x = c(-3, 6))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  theme_apa()

plotFive

ggsave(plotThree,filename = '~/PlotThreeHalfWidth.png', width = PowerPointSize[1]/2, height = PowerPointSize[2], units = PowerPointUnit)
ggsave(plotFive,filename = '~/PlotFiveHalfWidth.png', width = PowerPointSize[1]/2, height = PowerPointSize[2], units = PowerPointUnit)


##Pseudouniform 

maxSPETheseData <- 10
minSPETheseData <- 6
minSPE <- 1 - maxSPETheseData
maxSPE <- 24 - minSPETheseData

SPs <- rep(6:10, each = 40) #For the guessingDist function

guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                           maxSPE = maxSPE,
                                           targetSP = SPs,
                                           numItemsInStream = 24)

plotSixData <- data.frame(x = minSPE:maxSPE,
                          guessingDist = guessingDist)

plotSix <- ggplot(plotSixData, aes(x = x, y = guessingDist/sum(guessingDist)))+
  geom_bar(stat = 'identity', fill = '#628093')+
  labs(x = 'SPE', y = NULL)
plotSix

ggsave(plotSix,filename = '~/PlotSix.png', width = PowerPointSize[1], height = PowerPointSize[2], units = PowerPointUnit)
