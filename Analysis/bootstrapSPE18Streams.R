library(ggplot2)
library(dplyr)
library(magrittr)
library(mixRSVP)

allErrors <- read.csv('Analysis/allErrors18Streams.txt', header = T, stringsAsFactors = F)

bootstrapPValue <- function(theseData, numItemsInStream, whichSPE, nReps){
  nTrials <- nrow(theseData)
  
  maxSPETheseData <- theseData %>% pull(targetSP) %>% max
  minSPETheseData <- theseData %>% pull(targetSP) %>% min
  minSPE <- 1 - maxSPETheseData
  maxSPE <- numItemsInStream - minSPETheseData
  
  guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                             maxSPE = maxSPE,
                                             targetSP = theseData$targetSP,
                                             numItemsInStream = numItemsInStream)
  
  pseudoUniform <- data.frame(xDomain = minSPE:maxSPE, guessingDist = guessingDist, prob = guessingDist/sum(guessingDist)) #give guessing dist values labels
  
  
  
  
  nWhichSPE <- theseData %>% filter(SPE == whichSPE) %>% nrow() #How many observations at SPE = whichSPE?
  
  bootstraps <- data.frame(rep = 1:nReps,
                           count = -999)
  
  for(i in 1:nReps){
    thisSample <- sample(pseudoUniform$xDomain, prob = pseudoUniform$prob, replace = T, size = nTrials)
    nThisSPE<- which(thisSample == whichSPE) %>% length
    bootstraps %<>% mutate(count = replace(count, rep == i, nThisSPE))
  }
  
  return(length(which(bootstraps$count>=nWhichSPE))/nReps) #a p.value.
  
}


numItemsInStream = 24
whichSPE = -6

theseData <- data.frame(SPE = c(10, 8, 1, -1, -4, -2, 1, 1, -1, 1, -6, 0, 1, 1, -1, 11, 0, -1, -1, 0, -1, -2, 1, 0, -2, -6, 0, 0, 1, 1, 4, 0, 13, 1, 0, 0, -9, -2, 1, 1, 0, -1, 1, -1, 0, -2, 1, -3, -4, 0, 0, -5, 2, 0, 0, -1, 1, -2, 14, -1, 2, -3, -4, -1, -1, -5, 0, -1, 10, -1, -2, 1, -10, 2, 1, -2, 7, -2, -2, 0, 1, 1, -1),
                        targetSP = c(6, 9, 6, 10, 9, 8, 6, 7, 9, 8, 9, 8, 6, 7, 9, 10, 9, 8, 7, 7, 9, 6, 6, 8, 9, 10, 10, 10, 8, 6, 6, 9, 10, 10, 7, 7, 9, 10, 7, 10, 6, 8, 10, 8, 9, 8, 9, 8, 8, 7, 10, 7, 6, 7, 9, 7, 9, 7, 8, 10, 7, 6, 7, 7, 8, 10, 9, 8, 7, 8, 9, 6, 10, 9, 10, 6, 6, 10, 9, 7, 6, 6, 6))

ps <- expand.grid(xDomain = -9:18, 
                  p = -1, 
                  participant = unique(allErrors$ID), 
                  condition = unique(allErrors$condition))

for(thisParticipant in unique(allErrors$ID)[1]){
  print(thisParticipant)
  for(thisCondition in unique(allErrors$condition)){
    print(thisCondition)
    theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
    for(whichSPE in -9:9){
      print(whichSPE)
      thisP <- bootstrapPValue(theseData,numItemsInStream,whichSPE,10000)
      ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisP))
    }
  }
}

ps %>% filter(p != -1) %>% ggplot(., aes(x = xDomain, y = p))+
  geom_line(aes(colour = factor(condition)))+
  facet_wrap(~participant,nrow = 3)+
  geom_hline(yintercept = .05, linetype = 'dashed')+
  geom_vline(xintercept = -1, linetype = 'dashed')+
  labs(x = 'SPE', y = 'p', colour = 'nStream')

