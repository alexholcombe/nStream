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


bootstrapPValue <- function(theseData, numItemsInStream, whichSPE, nReps){
  nTrials <- nrow(theseData)
  
  #############################
  ###Generate pseudo-uniform###
  #############################
  
  maxSPETheseData <- theseData %>% pull(targetSP) %>% max
  minSPETheseData <- theseData %>% pull(targetSP) %>% min
  minSPE <- 1 - maxSPETheseData
  maxSPE <- numItemsInStream - minSPETheseData
  
  
  thisAccuracy <- length(which(theseData$SPE == 0))/nTrials
  
  thisGuessingRate <- 1 - thisAccuracy
  print(thisAccuracy)
  print(thisGuessingRate)
  
  guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                             maxSPE = maxSPE,
                                             targetSP = theseData$targetSP,
                                             numItemsInStream = numItemsInStream)
  
  pseudoUniform <- data.frame(xDomain = minSPE:maxSPE, guessingDist = guessingDist, prob = guessingDist/sum(guessingDist)) #give guessing dist values labels
  
  
  
  
  nWhichSPE <- theseData %>% filter(SPE == whichSPE) %>% nrow() #How many observations at SPE = whichSPE?
  
  bootstraps <- data.frame(rep = 1:nReps, #store the counts of whichSPE sampled from the pseudouniform here.
                           count = -999)
  
  for(i in 1:nReps){
    thisSample <- sample(pseudoUniform$xDomain, prob = pseudoUniform$prob, replace = T, size =  nTrials*thisGuessingRate) #sample nTrials many trials from the pseudouniform over the range of possible SPEs in this experiment
    nThisSPE<- which(thisSample == whichSPE) %>% length #How many of the trials had an SPE == whichSPE
    bootstraps %<>% mutate(count = replace(count, rep == i, nThisSPE)) #Put the count nThisSPE in the dataframe
  }
  
  return(length(which(bootstraps$count>=nWhichSPE))/nReps) #a p.value. The proportion of bootstrapped samples that had a count of whichSPE at least as great as the observed count
  
}

ps <- expand.grid(xDomain = xDomain,
                  p = -1,
                  participant = unique(twoStreamsOneTargetWide$ID),
                  condition = unique(twoStreamsOneTargetWide$condition))
for(thisParticipant in unique(twoStreamsOneTargetWide$ID)){
  print(thisParticipant)
  for(thisCondition in unique(twoStreamsOneTargetWide$condition)){
    print(thisCondition)
    theseData <- twoStreamsOneTargetWide %>% filter(ID == thisParticipant & condition == thisCondition)
    for(whichSPE in xDomain){
      print(whichSPE)
      thisP <- bootstrapPValue(theseData,numItemsInStream,whichSPE,nReps)
      ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisP))
    }
  }
}
