library(mixRSVP)
library(magrittr)
library(dplyr)

rm(list=ls())

setwd('~/gitCode/nStream/')

allErrors <- read.csv('Analysis/allErrors.txt', sep = '\t', header = T)

binomialPs <- expand.grid(xDomain = -1,
                          p = -1,
                          participant = unique(allErrors$ID),
                          condition = unique(allErrors$condition),
                          pTwo = -1,
                          pEight = -1,
                          z = -999,
                          p_z = -1)
SPEToTest = -1


for(thisParticipant in unique(allErrors$ID)){
  print(thisParticipant)
  for(thisCondition in unique(allErrors$condition)){
    print(thisCondition)
    theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject) #Get data for this participant and this condition
    
    thisEfficacy <- sum(theseData$error == 0)/nrow(theseData) #Efficacy
    
    minSPE <- 1 - max(theseData$cuePos0)#guessing distribution arguments
    maxSPE <- 24 - min(theseData$cuePos0)
    
    thisXDomain <- minSPE:maxSPE
    
    guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$cuePos0, 24)
    guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities
    
    thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy
    
    thisPvalue <- pbinom(sum(theseData$error == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count
    
    binomialPs %<>% mutate(p = replace(p, xDomain == SPEToTest & participant == thisParticipant & condition == thisCondition, thisPvalue))
    
  }
}

binomialPs %>% 
  group_by(condition) %>%
  summarise(nSig = sum(p<.05))

#####################################
###Compare proportions with z-test###
#####################################


Zs <- expand.grid(
  ID = unique(allData$ID),
  pTwo = -999,
  pEight = -999,
  z = -999
)

for(thisParticipant in unique(allErrors$ID)){
  two <- allErrors %>% filter(condition == 'twoStreams' & ID == thisParticipant & !fixationReject)
  eight <- allErrors %>% filter(condition == 'eightStreams' & ID == thisParticipant & !fixationReject)
  
  proportionTwo <- sum(two$error == -1)/nrow(two)
  proportionEight <- sum(eight$error == -1)/nrow(eight)
  
  pHat <- (sum(two$error == -1) + sum(eight$error == -1))/(nrow(two) + nrow(eight))
  
  seProp <- sqrt(pHat*(1-pHat) * (1/nrow(two) + 1/nrow(eight))) #SE of the proportion
  
  thisZ <- (proportionTwo - proportionEight)/seProp
  
  binomialPs %<>% mutate(
    pTwo = ifelse(participant == thisParticipant, proportionTwo, pTwo),
    pEight = ifelse(participant == thisParticipant, proportionEight, pEight),
    z = ifelse(participant == thisParticipant, thisZ,z),
    p_z = ifelse(participant == thisParticipant, pnorm(thisZ, lower.tail = F), p_z)
  )
}






