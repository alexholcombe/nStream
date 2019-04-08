####################################
###Check SPE = 0 against extremes###
####################################
library(magrittr)
library(ggplot2)
library(dplyr)
library(BayesFactor)
library(mixRSVP)
rm(list=ls())

numItemsInStream <- 24
allErrors <- read.table('Analysis/allErrors.txt', sep ='\t', header = T, stringsAsFactors = F)
allErrors %<>% rename(targetSP = cuePos0, SPE = error)


extremesAndZero <- expand.grid(ID = unique(allErrors$ID),
                               condition = unique(allErrors$condition),
                               extreme = numeric(1),
                               extremeCount = numeric(1),
                               zeroCount = numeric(1),
                               nTrials = numeric(1),
                               stringsAsFactors = F)

for(thisParticipant in unique(allErrors$ID)){
  theseConditions <- allErrors %>% filter(ID == thisParticipant & !fixationReject) %>% pull(condition) %>% unique()
  for(thisCondition in theseConditions){
    theseErrors <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject)
    
    countZero <- length(which(theseErrors$SPE == 0)) #They set a slamhound on Turner's trail in New Delhi
    
    maxSPETheseData <- allErrors %>% pull(targetSP) %>% max
    minSPETheseData <- allErrors %>% pull(targetSP) %>% min
    minSPE <- 1 - maxSPETheseData
    maxSPE <- numItemsInStream - minSPETheseData
    
    xDomain <- minSPE:maxSPE
    
    guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                               maxSPE = maxSPE,
                                               targetSP = unique(theseErrors$targetSP),
                                               numItemsInStream = numItemsInStream)
    
    #Extreme SPEs possible on every trial
    positiveExtreme <- which(guessingDist == max(guessingDist)) %>% max()
    negativeExtreme <- which(guessingDist == max(guessingDist)) %>% min()
    positiveExtremeSPE <- xDomain[positiveExtreme]
    negativeExtremeSPE <- xDomain[negativeExtreme]
  
    extremeSPE <- ifelse(positiveExtremeSPE>abs(negativeExtremeSPE), positiveExtremeSPE, negativeExtremeSPE ) #The most extreme SPE possible on every trial
    
    countExtreme <- length(which(theseErrors$SPE == extremeSPE))
    
    extremesAndZero %<>% mutate(
      zeroCount = replace(zeroCount, ID == thisParticipant & condition == thisCondition, countZero),
      extreme = replace(extreme, ID == thisParticipant & condition == thisCondition, extremeSPE),
      extremeCount = replace(extremeCount, ID == thisParticipant & condition == thisCondition, countExtreme),
      nTrials = replace(nTrials, ID == thisParticipant & condition == thisCondition, nrow(theseErrors))
    )
  }
}

extremesAndZero %<>% mutate(zeroCount = zeroCount/nTrials, extremeCount = extremeCount/nTrials) %>% melt(id.vars = c('ID', 'condition'), measure.vars = c('zeroCount','extremeCount'), value.name = 'Count')

extremesAndZero %<>% mutate(
  ID = factor(ID),
  condition = factor(condition),
  variable = factor(variable)
)

extremesAndZero %>% ggplot(aes(x = condition, y = Count))+
  geom_point(aes(colour = variable))+
  stat_summary(fun.y = mean, aes(shape = variable), geom = 'point', size = 4)+
  stat_summary(fun.data = mean_se, aes(group = variable), geom = 'errorbar', width = .2)+
  scale_shape_manual(values = c('zeroCount' = 5, 'extremeCount' = 0))

anovaBF(Count~condition*variable+ID, whichRandom = 'ID', data = extremesAndZero) %>% max()
