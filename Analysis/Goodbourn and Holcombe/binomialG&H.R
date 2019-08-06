#library(mixRSVP)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(purrr)
devtools::load_all(path = '~/gitCode/mixRSVP/')

rm(list=ls())

setwd('~/gitCode/nStream/Analysis/Goodbourn and Holcombe/')

allErrors <- read.csv('Data and Materials/allData.csv', stringsAsFactors = F)
twoStreamsOneTarget <- allErrors %>% filter(condition == 2, (exp == 'Exp2' & pool == 'Experienced Observers') | (exp == 'Exp1' & pool == 'SONA'))

xDomain = -1
nReps = 5000
numItemsInStream <- 24


twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #
  dcast(ID+trial+block+condition~stream, value.var = 'SPE') %>%
  rename('One' = '1', 'Two' = '2') %>%
  mutate(SPE = ifelse(is.na(One), Two, One)) 

twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #Add targetSP
  dcast(ID+trial+block+condition~stream, value.var = 'targetSP') %>%
  rename('One' = '1', 'Two' = '2') %>%
  mutate(targetSP = ifelse(is.na(One), Two, One)) %>%
  select(ID,trial, block, targetSP) %>%
  inner_join(twoStreamsOneTargetWide, by = c('ID','trial', 'block'))


ps <- expand.grid(xDomain = xDomain,
                  p = -1,
                  participant = unique(twoStreamsOneTargetWide$ID),
                  condition = unique(twoStreamsOneTargetWide$condition))

SPEToTest <- -1

for(thisParticipant in unique(twoStreamsOneTargetWide$ID)){
  print(thisParticipant)
  for(thisCondition in unique(twoStreamsOneTargetWide$condition)){
    print(thisCondition)
    theseData <- twoStreamsOneTargetWide %>% filter(ID == thisParticipant & condition == thisCondition)
    for(whichSPE in xDomain){
      print(whichSPE)
      thisEfficacy <- sum(theseData$SPE == 0)/nrow(theseData) #Efficacy from SPE = 0
      
      minSPE <- 1 - max(theseData$targetSP)#guessing distribution arguments
      maxSPE <- 24 - min(theseData$targetSP)
      
      thisXDomain <- minSPE:maxSPE
      
      guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$targetSP, 24)
      guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities
      
      thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy
      
      thisPvalue <- pbinom(sum(theseData$SPE == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count
      
      thisCount <- length(which(theseData$SPE == 0))
      ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisPvalue))
    }
  }
}
