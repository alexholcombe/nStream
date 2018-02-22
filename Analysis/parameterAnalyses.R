###Parameter Analyses###
rm(list=ls())
library(ggplot2)
library(mixRSVP)
library(dplyr)

setwd('~/gitCode/nStream/')

allErrors <- read.table('Analysis/allErrors.txt', sep='\t', stringsAsFactors = F, header = T)

params <- expand.grid(
  ID = unique(allErrors$ID),
  crowded = unique(allErrors$crowded),
  ring = unique(allErrors$ring),
  efficacy = numeric(1),
  latency = numeric(1),
  precision = numeric(1),
  stringsAsFactors = F
)

for(thisParticipant in unique(allErrors$ID)){
  for(thisCondition in unique(allErrors$crowded)){
    for(thisRing in unique(allErrors$ring)){
      print(thisParticipant)
      print(thisCondition)
      print(thisRing)
      theseParams <- allErrors %>% filter(., crowded == thisCondition, ring == thisRing, ID == thisParticipant) %>% analyzeOneCondition(., 24, parameterBounds(), 100)
      
      params %<>%v
        mutate(efficacy=replace(efficacy, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$efficacy)) %>%
        as.data.frame()
      
      params %<>%
        mutate(latency=replace(latency, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$latency)) %>%
        as.data.frame()
      
      params %<>%
        mutate(precision=replace(precision, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$precision)) %>%
        as.data.frame()
    }
  }
}


ggplot(params, aes(x=crowded, y = efficacy))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))

ggplot(params, aes(x=crowded, y = latency))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))

ggplot(params, aes(x=crowded, y = precision))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))