###Parameter Analyses###
rm(list=ls())
library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(BayesFactor)

setwd('~/gitCode/nStream/')

inclusionBF <- function(priorProbs, variable){
  
  ###https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp###
  
  
  if(typeof(priorProbs) == 'S4') priorProbs <- as.vector(priorProbs)
  
  
  theseNames <- names(priorProbs)
  nProbs <- 1:length(priorProbs)
  variableMatches <- grep(variable, theseNames)
  
  if(grepl(':', variable)){
    subordinateVariables <- variable %>% strsplit(':') %>% unlist()

    thisRegex <- paste0(subordinateVariables,collapse = '.*\\+.*')
    
    subordinateEffects <- grep(thisRegex, theseNames, perl = T)
    subordinateEffects <- subordinateEffects[!subordinateEffects %in% variableMatches]
    
    
    sum(priorProbs[variableMatches])/sum(priorProbs[subordinateEffects])
  } else {
    interactionMatches <- grep(paste0(variable,'(?=:)|(?<=:)',variable), theseNames, perl = T)
    
    variableMainEffects <- variableMatches[!variableMatches %in% interactionMatches]
    
    
    otherMainEffects <- nProbs[!nProbs %in% c(variableMainEffects,interactionMatches)]
    
    
    sum(priorProbs[variableMainEffects])/sum(priorProbs[otherMainEffects])
  }
}



allErrors <- read.table('Analysis/allErrors.txt', sep='\t', stringsAsFactors = F, header = T)

params <- expand.grid(
  ID = factor(unique(allErrors$ID)),
  crowded = factor(unique(allErrors$crowded)),
  ring = factor(unique(allErrors$ring)),
  efficacy = numeric(1),
  latency = numeric(1),
  precision = numeric(1),
  stringsAsFactors = F
)
if(!file.exists('modelOutput/parameterEstimates.csv')){
  for(thisParticipant in unique(allErrors$ID)){
    for(thisCondition in unique(allErrors$crowded)){
      for(thisRing in unique(allErrors$ring)){
        theseParams <- allErrors %>% filter(., crowded == thisCondition, ring == thisRing, ID == thisParticipant) %>% analyzeOneCondition(., 24, parameterBounds(), 100)
        
        params %<>%
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
  write.csv(params, 'modelOutput/parameterEstimates.csv')
} else {
  params <- read.csv('modelOutput/parameterEstimates.csv')
}

params$ring %<>% as.factor
#######################
###Efficacy Analyses###
#######################

efficacyBF <- anovaBF(efficacy ~ ring * crowded + ID, 
                      data=params,
                      whichRandom = 'ID'
                      ) 

efficacyPriorProbs <- efficacyBF %>% newPriorOdds() %>% `*`(efficacyBF) %>% as.BFprobability() %>% as.vector() #extract P(M|Data) from BF object

efficacyInclusionBFs <- rep(-999, times = 3)

names(efficacyInclusionBFs) <- c('crowded','ring','crowded:ring')

for(name in names(efficacyInclusionBFs)){ #calculate inclusion BFs for main effects and interactions
  thisInclusionBF <- inclusionBF(efficacyPriorProbs, name)
  efficacyInclusionBFs[name] <- thisInclusionBF
}

print(efficacyInclusionBFs)

#Only evidence for an effect of ring
### Looks like there is drop between the innermost ring and the middle, but no changes outside that.
### I wonder if that is bec

ggplot(params, aes(x=crowded, y = efficacy))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))


latencyBF <- anovaBF(latency ~ ring * crowded + ID, 
                      data=params,
                      whichRandom = 'ID'
)

latencyPriorProbs <- latencyBF %>% newPriorOdds() %>% `*`(latencyBF) %>% as.BFprobability() %>% as.vector() #that nested newPriorOdds() function call is the shittiest hack. Prior odds are 1 by default. I shouldn't need it, but S4 is a mystery to me

latencyInclusionBFs <- rep(-999, times = 3)

names(latencyInclusionBFs) <- c('crowded','ring','crowded:ring')

for(name in names(latencyInclusionBFs)){
  thisInclusionBF <- inclusionBF(latencyPriorProbs, name)
  latencyInclusionBFs[name] <- thisInclusionBF
}

ggplot(params, aes(x=crowded, y = latency))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))

precisionBF <- anovaBF(precision ~ ring * crowded + ID, 
                     data=params,
                     whichRandom = 'ID'
)

precisionPriorProbs <- precisionBF %>% newPriorOdds() %>% `*`(precisionBF) %>% as.BFprobability() %>% as.vector() #that nested newPriorOdds() function call is the shittiest hack. Prior odds are 1 by default. I shouldn't need it, but S4 is a mystery to me

precisionInclusionBFs <- rep(-999, times = 3)

names(precisionInclusionBFs) <- c('crowded','ring','crowded:ring')

for(name in names(precisionInclusionBFs)){
  thisInclusionBF <- inclusionBF(precisionPriorProbs, name)
  precisionInclusionBFs[name] <- thisInclusionBF
}

ggplot(params, aes(x=crowded, y = precision))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))
