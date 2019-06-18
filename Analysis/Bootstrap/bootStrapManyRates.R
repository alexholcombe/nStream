rm(list=ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(mixRSVP)
library(reshape2)
library(papaja)
setwd('~/gitCode/nStream/')

theme_set(theme_apa())

allErrors <- read.table('Analysis/allData_29-04-2019_21-25.csv', header = T, stringsAsFactors = F, sep =',')

runAnyway <- TRUE
xDomain = -1
numItemsInStream <- 24

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
                  participant = unique(allErrors$ID),
                  condition = unique(allErrors$condition))
for(thisParticipant in unique(allErrors$ID)){
  print(thisParticipant)
  for(thisCondition in unique(allErrors$condition)){
    print(thisCondition)
    theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
    for(whichSPE in xDomain){
      print(whichSPE)
      thisP <- bootstrapPValue(theseData,numItemsInStream,whichSPE,5000)
      ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisP))
    }
  }
}

ps %>% group_by(condition) %>% summarise(nSig = sum(p < .05/length(unique(ID))))


