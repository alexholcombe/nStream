library(dplyr)
library(ggplot2)
library(BayesFactor)
library(magrittr)

setwd('~/gitCode/nStream/')

modelRecovery <- read.csv('modellingScripts/Model recovery/EstimatesAndTruth/TruncNorm/estimatesAndTruthTruncNorm.csv', header = F)

propBeforeCue <- modelRecovery[,c(2:4,8:10)]

propBeforeCue %<>% mutate(propBefore = NA)

for(row in 1:nrow(propBeforeCue)){
  thisLatency <- propBeforeCue[row,5]
  thisPrecision <- propBeforeCue[row,6]
  thisProp <- pnorm(0, thisLatency, thisPrecision)
  propBeforeCue$propBefore[row] <- thisProp
}

ggplot(propBeforeCue,aes(x=factor(V3), y = propBefore))+
  geom_jitter(aes(colour = factor(V4)))+
  stat_summary(fun.y = mean, geom='point', size = 5, aes(group = factor(V4), colour = factor(V4)))

anovaBF(propBefore~factor(V3)*factor(V4), data = propBeforeCue[complete.cases(propBeforeCue),])


