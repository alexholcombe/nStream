library(mixRSVP)
library(magrittr)
library(dplyr)

rm(list=ls())

setwd('~/gitCode/nStream/')

allErrors <- read.csv('Analysis/allErrors.csv', sep = ',', header = T, stringsAsFactors = F)

allErrors %<>% filter(subject !='AH ')


allErrors$cuePos <- -999

for(thisTrial in 1:nrow(allErrors)){
  thisData <- allErrors[thisTrial,]
  
  thisCuedStream <- thisData$whichStream0
  
  thisSequence <- strsplit(thisData[,paste0('streamLtrSequence', thisCuedStream)], split = '')
  thisSequence <- unlist(thisSequence)
  
  thisCorrect <- gsub(pattern = ' ', replacement = '', thisData$answer0)
  
  thisCuePos <- which(thisSequence == thisCorrect)-1
  
  allErrors$cuePos[thisTrial] <- thisCuePos
}

binomialPs <- expand.grid(xDomain = -1,
                          p = -1,
                          participant = unique(allErrors$subject),
                          condition = unique(allErrors$condition),
                          pTwo = -1,
                          pEight = -1,
                          z = -999,
                          p_z = -1)
SPEToTest = -1


for(thisParticipant in unique(allErrors$subject)){
  print(thisParticipant)
  for(thisCondition in unique(allErrors$condition)){
    print(thisCondition)
    theseData <- allErrors %>% filter(subject == thisParticipant & condition == thisCondition) #Get data for this participant and this condition
    
    thisEfficacy <- sum(theseData$responsePosRelative0 == 0)/nrow(theseData) #Efficacy
    
    minSPE <- 1 - max(theseData$cuePos)#guessing distribution arguments
    maxSPE <- 24 - min(theseData$cuePos)
    
    thisXDomain <- minSPE:maxSPE
    
    guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$cuePos, 24)
    guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities
    
    thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy
    
    thisPvalue <- pbinom(sum(theseData$responsePosRelative0 == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count
    
    binomialPs %<>% mutate(p = replace(p, xDomain == SPEToTest & participant == thisParticipant & condition == thisCondition, thisPvalue))
    
  }
}

binomialPs %>% 
  group_by(condition) %>%
  summarise(nSig = sum(p<.05))
