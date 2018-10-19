library(mixRSVP)
library(dplyr)
library(magrittr)

rm(list=ls())


setwd('~/gitCode/nStream/Analysis/Pilot 18 str/')

pilotData <- read.table('18TR1_18Oct2018_09-35.txt', sep = '\t', header = T)

colnames(pilotData)[which(colnames(pilotData) == 'cuePos0')] <- 'targetSP'
colnames(pilotData)[which(colnames(pilotData) == 'responsePosRelative0')] <- 'SPE'

nStreams <-  c(2, 6,18)

params <- expand.grid(nStreams = nStreams, efficacy = 999, latency = 999, precision = 999, pLRtest = 999)

for(thisNStream in nStreams){
  theseParams <- pilotData %>% filter(nStreams == thisNStream) %>% analyzeOneCondition(., 24, parameterBounds(), 100)
  
  params %<>% mutate(efficacy = replace(efficacy, nStreams == thisNStream, theseParams$efficacy))
  params %<>% mutate(latency = replace(latency, nStreams == thisNStream, theseParams$latency))
  params %<>% mutate(precision = replace(precision, nStreams == thisNStream, theseParams$precision))
  params %<>% mutate(pLRtest = replace(pLRtest, nStreams == thisNStream, theseParams$pLRtest))
}

write.csv(x = params,
          file = 'pilotParams.csv')
