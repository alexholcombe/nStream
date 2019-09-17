library(mixRSVP)
library(magrittr)
library(dplyr)

rm(list=ls())

setwd('~/gitCode/nStream/')

allErrors <- read.csv('Analysis/allData_30-04-2019_13-28.csv', sep = ',', header = T)

binomialPs <- expand.grid(xDomain = -1,
                          p = -1,
                          participant = unique(allErrors$ID),
                          condition = unique(allErrors$condition),
                          stream = c(1,2),
                          pTwo = -1,
                          pEight = -1,
                          z = -999,
                          p_z = -1)
SPEToTest = -1


for(thisParticipant in unique(allErrors$ID)){
  print(thisParticipant)
  for(thisCondition in unique(allErrors$condition)){
    for(thisStream in c(1,2)){
      print(thisCondition)
      theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & stream == thisStream) #Get data for this participant and this condition
      
      thisEfficacy <- sum(theseData$SPE == 0)/nrow(theseData) #Efficacy
      
      minSPE <- 1 - max(theseData$targetSP)#guessing distribution arguments
      maxSPE <- 24 - min(theseData$targetSP)
      
      thisXDomain <- minSPE:maxSPE
      
      guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$targetSP, 24)
      guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities
      
      thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy
      
      thisPvalue <- pbinom(sum(theseData$SPE == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count
      
      binomialPs %<>% mutate(p = replace(p, xDomain == SPEToTest & participant == thisParticipant & condition == thisCondition & stream == thisStream, thisPvalue))
    }
  }
}

binomialPs %>% 
  group_by(condition, stream) %>%
  summarise(nSig = sum(p<.05))
