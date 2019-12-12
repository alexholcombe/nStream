###Plot some samples for the explanation plot###
library(magrittr)
library(dplyr)
library(ggplot2)
library(mixRSVP)

rm(list=ls())
setwd('~/gitCode/nStream/')

allErrors <- read.csv('Analysis/allErrors18Streams.txt', sep = ',', header = T)
theseData <- allErrors %>% filter(ID == '18JG', condition == 2)

SPEToTest <- -1

nTrials <- nrow(theseData)

thisEfficacy <- sum(theseData$SPE == 0)/nrow(theseData) #Efficacy

minSPE <- 1 - max(theseData$targetSP)#guessing distribution arguments
maxSPE <- 24 - min(theseData$targetSP)

thisXDomain <- minSPE:maxSPE

guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$targetSP, 24)
guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities

thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy

thisPvalue <- pbinom(sum(theseData$SPE == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count

facetLabels = function(x){ #Dummy function for empty facet labels
  return('')
}

empiricalHist <- ggplot(theseData, aes(x = SPE))+
  geom_histogram(binwidth = 1, aes(fill = SPE == -1))+
  scale_fill_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
  labs(y = 'Count', x = 'SPE')+
  scale_y_continuous(breaks = c(0,5,10,20,30))+
  theme_apa()


pseudoUniform <- data.frame(xDomain = thisXDomain, guessingDist)

pseudoUnifHist <- ggplot(pseudoUniform, aes(x = xDomain, y = guessingDist))+
  geom_bar(width = 1, stat = 'identity', aes(fill = xDomain == -1))+
  scale_fill_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
  labs(y = '', x = 'SPE')+
  scale_x_continuous(breaks = seq(minSPE,maxSPE,2))+
  theme_apa()

binomialMass <- data.frame(count = 0:10, mass = dbinom(0:10, nrow(theseData), thisProb))
binomialSamples <- data.frame(count = rbinom(1000, nrow(theseData), thisProb))
binomialSamples %<>% filter(count<11) #because I have to specify the colouring manually, defining the max count makes things easier. The breaks on the histogram are the same for every random sample now

binomialPlot <- ggplot(binomialMass, aes(x = count, y = mass))+
  #geom_histogram(data = binomialSamples, aes( x= count, y = ..density..), breaks = -.5:10.5, fill = c('#628093','#628093','#628093','#628093','#628093','#ffa951','#ffa951','#ffa951','#ffa951','#ffa951','#ffa951'), inherit.aes = F)+
  geom_point(aes(colour = count>4), size = 7)+
  geom_line()+
  scale_colour_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
  scale_x_continuous(breaks = seq(0,10,2))+
  theme_apa()

binomialPlot

ggsave(plot = binomialPlot, filename = 'manuscripts_etc/Manuscript Figures/Binomial Explainer/binomialDistribution.png', height = 5, width = 5, units = 'in')
ggsave(plot = pseudoUnifHist, filename = 'manuscripts_etc/Manuscript Figures/Binomial Explainer/pseudoUnifrom.png', height = 5, width = 5, units = 'in')
ggsave(plot = empiricalHist, filename = 'manuscripts_etc/Manuscript Figures/Binomial Explainer/empiricalHistogram.png', height = 5, width = 5, units = 'in')
