###Parameter Analyses###
rm(list=ls())
library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(BayesFactor)

setwd('~/gitCode/nStream/')

participantPlots = TRUE #plot histograms with density?

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
  efficacy = 999,
  latency = 999,
  precision = 999,
  val = 999,
  valGuessing = 999,
  pLRtest = 999,
  stringsAsFactors = F
)

nReps <- 100

bounds <- parameterBounds()

bounds['precision','upper'] <- 3

runAnyway <- FALSE #If TRUE, fit models regardless of the presence of a parameter file. 

nParamFiles <- length(list.files(path = 'modelOutput',pattern ='parameterEstimates.*\\.csv',full.names = T)) #How many parameter DFs are saved?

if(nParamFiles==0 | runAnyway){
  for(thisParticipant in unique(allErrors$ID)){
    for(thisCondition in unique(allErrors$crowded)){
      for(thisRing in unique(allErrors$ring)){
        print(paste0('Participant: ', thisParticipant, '. Ring: ', thisRing, '. Condition: ', thisCondition))
        theseParams <- allErrors %>% filter(., crowded == thisCondition, ring == thisRing, ID == thisParticipant) %>% analyzeOneCondition(., 24, bounds, nReps)
        
        if(theseParams$pLRtest<.05){
          params %<>%
            mutate(efficacy=replace(efficacy, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$efficacy)) %>%
            as.data.frame()
          
          params %<>%
            mutate(latency=replace(latency, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$latency)) %>%
            as.data.frame()
          
          params %<>%
            mutate(precision=replace(precision, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$precision)) %>%
            as.data.frame()
        } else {
          params %<>%
            mutate(efficacy=replace(efficacy, ID == thisParticipant & crowded == thisCondition & ring == thisRing, 0)) %>%
            as.data.frame()
          
          params %<>%
            mutate(latency=replace(latency, ID == thisParticipant & crowded == thisCondition & ring == thisRing, NaN)) %>%
            as.data.frame()
          
          params %<>%
            mutate(precision=replace(precision, ID == thisParticipant & crowded == thisCondition & ring == thisRing, NaN)) %>%
            as.data.frame()
        }
        
        params %<>%
          mutate(val=replace(val, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$val)) %>%
          as.data.frame()
        
        params %<>%
          mutate(valGuessing=replace(valGuessing, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$valGuessing)) %>%
          as.data.frame()
        
        params %<>%
          mutate(pLRtest=replace(pLRtest, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$pLRtest)) %>%
          as.data.frame()
      }
    }
  }
  write.csv(params, paste0('modelOutput/parameterEstimates',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.csv'))
} else {
  paramFiles <- list.files(path = 'modelOutput',pattern ='parameterEstimates.*\\.csv',full.names = T) #What are the saved param files?
  
  splits <- strsplit(paramFiles, 'Estimates|(?<=[0-9][0-9])_|\\.csv',perl = T) #split out the date stamps from the param files names
  
  theseDates <- lapply(splits,FUN =function(x) paste(x[2],x[3])) %>% 
    unlist %>% 
    as.POSIXct(.,format='%d-%m-%Y %H-%M-%S') #Format them as POSIXct (datetime) 
  
  whichMaxDate <- which(theseDates == max(theseDates)) #which is the oldest?
  params <- read.csv(paramFiles[whichMaxDate]) #load the oldest
}

params$ring %<>% as.factor

paramsForAnalysis <- params %>% filter(efficacy>.1 & ID != 'CH')
#######################
###Efficacy Analyses###
#######################

efficacyBF <- anovaBF(efficacy ~ ring * crowded + ID, 
                      data=paramsForAnalysis,
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

ggplot(paramsForAnalysis, aes(x=crowded, y = efficacy))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))


latencyBF <- anovaBF(latency ~ ring * crowded + ID, 
                      data=paramsForAnalysis,
                      whichRandom = 'ID'
)

latencyPriorProbs <- latencyBF %>% newPriorOdds() %>% `*`(latencyBF) %>% as.BFprobability() %>% as.vector() #that nested newPriorOdds() function call is the shittiest hack. Prior odds are 1 by default. I shouldn't need it, but S4 is a mystery to me

latencyInclusionBFs <- rep(-999, times = 3)

names(latencyInclusionBFs) <- c('crowded','ring','crowded:ring')

for(name in names(latencyInclusionBFs)){
  thisInclusionBF <- inclusionBF(latencyPriorProbs, name)
  latencyInclusionBFs[name] <- thisInclusionBF
}

print(latencyInclusionBFs)

ggplot(paramsForAnalysis, aes(x=crowded, y = latency))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))

precisionBF <- anovaBF(precision ~ ring * crowded + ID, 
                     data=paramsForAnalysis,
                     whichRandom = 'ID'
)

precisionPriorProbs <- precisionBF %>% newPriorOdds() %>% `*`(precisionBF) %>% as.BFprobability() %>% as.vector() #that nested newPriorOdds() function call is the shittiest hack. Prior odds are 1 by default. I shouldn't need it, but S4 is a mystery to me

precisionInclusionBFs <- rep(-999, times = 3)

names(precisionInclusionBFs) <- c('crowded','ring','crowded:ring')

for(name in names(precisionInclusionBFs)){
  thisInclusionBF <- inclusionBF(precisionPriorProbs, name)
  precisionInclusionBFs[name] <- thisInclusionBF
}

print(precisionInclusionBFs)

ggplot(paramsForAnalysis, aes(x=crowded, y = precision))+
  geom_violin(aes(fill = factor(ring)), position = position_dodge(.9))+
  stat_summary(geom = 'point', aes(group = factor(ring)),fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', aes(group = factor(ring)), fun.data = mean_se, position = position_dodge(.9))
########################
###Plots with Density###
########################

if(participantPlots){
  for(thisParticipant in unique(allErrors$ID)){
    for(thisCondition in unique(allErrors$crowded)){
      for(thisRing in unique(allErrors$ring)){
        thisEfficacy <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(efficacy)
        thisLatency <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(latency)
        thisPrecision <- thisEfficacy <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(precision)
        
        theseErrors <- allErrors %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing)
        print(paste0('Participant: ', thisParticipant, '. Ring: ', thisRing, '. Condition: ', thisCondition,'. N = ', nrow(theseErrors)))
        minError <- theseErrors %>% pull(SPE) %>% min
        maxError <- theseErrors %>% pull(SPE) %>% max
        thisRange <- seq(minError,maxError,.1)
        
        theseDensities <- data.frame(SPE = thisRange, density = dnorm(thisRange, thisLatency, thisPrecision))
        if(any(is.nan(theseDensities$density))){
          print(theseDensities)
        }
        
        thisPlot <- ggplot(theseErrors, aes(x=SPE))+
          geom_histogram(binwidth = 1)+
          geom_line(data = theseDensities, aes(x = SPE, y=density*nrow(theseErrors)))+ #scale density to histogram with density * N * binwidth
          scale_y_continuous(sec.axis = sec_axis(~./nrow(theseErrors), name = 'Density'))+
          labs(y = 'Frequency')
        
        thisFileName <- paste0('modelOutput/Plots/',thisCondition,'/',thisRing,'/',thisParticipant,'-',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.png')
        ggsave(filename = thisFileName, thisPlot,width = 16, height = 9)
      }
    }
  }
}
