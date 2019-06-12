rm(list=ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(mixRSVP)
library(reshape2)
library(papaja)
setwd('~/gitCode/nStream/')

theme_set(theme_apa())

allErrors <- read.table('Analysis/allErrors.txt', header = T, stringsAsFactors = F, sep ='\t')

colnames(allErrors)[c(3,7)] <- c('SPE', 'targetSP')

runAnyway <- TRUE
xDomain = -1

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


numItemsInStream = 24

pFiles <- list.files(pattern = 'bootstrapPValues8Streams.*\\.csv', 
                     path = 'Analysis/Bootstrap', 
                     full.names = T)

if(length(pFiles)>0 & !runAnyway){
  splits <- pFiles %>% strsplit(x = .,
                                split = 'Streams|PValues|\\.csv')
  
  
  dates <- lapply(splits,
                  FUN = function(x){
                    x[3] %>% as.POSIXct(., format = "%d-%m-%Y_%H-%M-%S")
                  }) %>% unlist
  
  whichLatestDate <- which(dates == max(dates))
  
  ps <- read.csv(pFiles[whichLatestDate])
} else{
  ps <- expand.grid(xDomain = xDomain,
                    p = -1,
                    participant = unique(allErrors$ID),
                    condition = unique(allErrors$condition))
  for(thisParticipant in unique(allErrors$ID)){
    print(thisParticipant)
    for(thisCondition in unique(allErrors$condition)){
      print(thisCondition)
      theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject)
      for(whichSPE in xDomain){
        print(whichSPE)
        thisP <- bootstrapPValue(theseData,numItemsInStream,whichSPE,5000)
        ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisP))
      }
    }
  }
  write.csv(ps, paste0('Analysis/bootstrapPValues8Streams',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.csv'),row.names = F)
}

ps %<>% filter(participant != '18TR1')

ps %>% filter(p != -1) %>% ggplot(., aes(x = xDomain, y = p))+
  geom_line(aes(colour = factor(condition)))+
  facet_wrap(~participant,nrow = 3)+
  geom_hline(yintercept = .05, linetype = 'dashed')+
  geom_vline(xintercept = -1, linetype = 'dashed')+
  labs(x = 'SPE', y = 'p', colour = 'nStream')

table <- ps %>% group_by(condition, xDomain) %>% summarise(nSig = length(which(p<(.05/10))))

table %>% dcast(.,
                xDomain ~ condition,
                value.var = 'nSig')

bootstrapPlot <- table %>% ggplot(., aes(x=xDomain, y = nSig))+
  geom_line(aes(linetype = factor(condition)),size = 1)+
  scale_x_continuous(breaks = (-4):4)+
  scale_y_continuous(breaks = seq(0,10,1))+
  labs(x = 'SPE', y = 'Deviations from Guessing',linetype = 'nStreams')

bootstrapPlot

ggsave(filename = 'modelOutput/8Streams/bootstrapPlot.png',
       plot = bootstrapPlot,
       height=12.09, 
       width=29.21,
       units='cm')

