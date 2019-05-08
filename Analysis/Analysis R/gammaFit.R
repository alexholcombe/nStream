###############################
###Date: 2019-04-25 14:18:37###
###Author: Charlie Ludowici####
###############################
#Eighteen Streams Gamma fitting
library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
library(R.matlab)
library(BayesFactor)
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

guessingDistributionBIC <- function(df){
  columnsPresent <- all(c('val','valGuessing') %in% colnames(df))
  if(!columnsPresent){
    missingCols <- c('val','valGuessing')[!c('val','valGuessing') %in% colnames(df)]
    stop('Missing ', missingCols, ' from df')
  }
  
  BICVal <- log(100)*3+(2*df$val)
  BICValGuessing <- log(100)*0+(2*df$valGuessing)
  
  deltaBIC <- BICValGuessing - BICVal
  
  data.frame(BFGuessVsMix = exp(deltaBIC/2))
}

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


timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')


matlabDataFiles <- list.files(path = 'Data', pattern = '.mat', full.names = T)

IDs <- matlabDataFiles %>% 
  gsub(pattern = 'Data/|(?<=[A-Z][A-Z])_.*',replacement = '', x = ., perl = T) %>% 
  unique()

conditions <-  as.character(c(6,8,12,24))

stimuli <- LETTERS[!LETTERS %in% c('N', 'V')]

nReplications = 100
numLettersInStream <- 24

paramsDF <- expand.grid(
  participant = IDs,
  condition = conditions,
  stream = 1:2,
  efficacy = numeric(1),
  latency = numeric(1),
  precision = numeric(1),
  val = numeric(1),
  valGuessing = numeric(1),
  pLRtest = 999,
  model = c('Gamma','Normal')
)

allData <- data.frame(
  ID = character(4800),
  trial = numeric(4800), 
  condition = numeric(4800),
  stream = numeric(4800),
  SPE = numeric(4800),
  targetSP = numeric(4800),
  response = character(4800),
  stimuli = character(4800),
  stringsAsFactors = F
)

parameterBoundsGamma <- data.frame(
  lower = c(0,0.1,.1),
  upper = c(1,7,3)
)

parameterBoundsNormal <- data.frame(
  lower = c(0, -4, .001),
  upper = c(1, 4, 3)
)

nObs <- allData %>%
  group_by(ID, condition, stream) %>%
  summarise(n = n()) %>%
  rename(participant = ID)

nParams <- c('Gamma' = 3, 'Normal' = 3)

lastID <- 'None'
lastCondition <- 'None'

startRow <- 1

dataFiles <- list.files(
  path = 'Analysis',
  pattern = 'allData',
  full.names = T
  )

runParamAnyway <- FALSE #even if there are parameter files, do the fit again
runDataAnyway <- FALSE #even if there are data files, wrangle the data again

if(length(dataFiles)>1 & !runDataAnyway){
  allData <- read.csv(dataFiles[1], stringsAsFactors = F)
} else {
  for(thisFile in matlabDataFiles){
    thisMatlab <- readMat(thisFile)
    thisID <- thisMatlab$participantID[[1]]
    thisCondition <- thisMatlab$itemRate[[1]]
    if(thisID==lastID & thisCondition == lastCondition){
      trialN <- (lastTrialN+1):(lastTrialN+25)
    }else{
      trialN = 1:25
    }
    for(thisStream in 1:2){
      endRow <- startRow + 24
      theseStimuli <- apply(thisMatlab$allLetterOrder[,thisStream,],2,function(i) stimuli[i]) %>% 
        apply(., 1, paste0, collapse = '')
      
      
      theseTargets <- thisMatlab$allTargets[,thisStream]
      
      theseResponseSPs <- c()
      theseResponses <- c()
      
      for(thisTrial in 1:25){
        actualTrialN <- trialN[thisTrial]
        thisResponse <- thisMatlab$allResponses[thisTrial, thisStream]
        thisLetterOrder <- thisMatlab$allLetterOrder[thisTrial, thisStream,]
        thisSP <- which(thisLetterOrder == thisResponse)
        theseResponseSPs <- c(theseResponseSPs, thisSP)
        theseResponses <- c(theseResponses, stimuli[thisResponse])
      }
      
      theseSPEs <- theseResponseSPs - theseTargets
      
      
      temp <- data.frame(
        ID = thisID,
        trial = trialN,
        condition = thisCondition,
        stream = thisStream,
        SPE = theseSPEs,
        targetSP = theseTargets,
        response = theseResponses,
        stimuli = theseStimuli,
        stringsAsFactors = F
      )
      
      allData[startRow:endRow,] <- temp
      
      startRow <- endRow + 1
    }
    lastTrialN <- max(trialN)
    lastID <- thisID
    lastCondition <- thisCondition
  }
  allDataFile <- paste0('Analysis/allData_', timeStamp, '.csv')
  
  write.csv(file = allDataFile,
            x = allData, 
            row.names = F)
}


paramFiles <- list.files(path = 'Analysis/Gamma Fits',
                         pattern = 'params.*csv',
                         full.name = T)

if(length(paramFiles)>0 & !runParamAnyway){
  times <- gsub('.*paramsDF_|\\.csv','', paramFiles) 
  timesFormated <- times %>% as.POSIXct(format = '%d-%m-%Y_%H-%M')
  maxDate <- which(timesFormated == max(timesFormated))
  paramsDF <- read.csv(paramFiles[maxDate], stringsAsFactors = F)
} else{
  for(thisParticipant in IDs){
    for(thisCondition in conditions){
      for(thisStream in 1:2){
        theseData <- allData %>% filter(ID == thisParticipant, condition == thisCondition, stream == thisStream)
        
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Stream: ', thisStream ,'. Fitting gamma model                                  \r', sep = '')
        paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Stream: ', thisStream , '. Fitting normal model                                  \r', sep = '')
        paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$efficacy),
          latency = replace(latency, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$latency),
          precision = replace(precision, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$precision),
          val = replace(val, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Normal', paramsN$pLRtest)
        )
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$efficacy),
          latency = replace(latency, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$latency),
          precision = replace(precision, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$precision),
          val = replace(val, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & stream == thisStream & model == 'Gamma', paramsGamma$pLRtest)
        )
      }
    }
  }
  BICs <- paramsDF %>%
    compareMixtureModels(params = .,
                         nParam = nParams,
                         nObs = 100,
                         variables = c('condition', 'participant', 'stream'))
  
  
  paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
    mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
    mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
    select(participant, condition, stream, BF, favouredModel) %>%
    left_join(paramsDF, ., by = c('participant', 'condition', 'stream')) #Add all this information to the paramDF 
  
  paramsFile <- paste0('Analysis/Gamma Fits/paramsDF_', timeStamp, '.csv')
  
  write.csv(file = paramsFile,
            x = paramsDF, 
            row.names = F)
}

#########################################################
###Compute BFs comparing the mixtture to guessing only###
#########################################################

paramsDF %<>% 
  group_by(participant, condition, stream, model) %>% 
  do(guessingDistributionBIC(.)) %>% 
  mutate(
    guessOrMixture = ifelse(BFGuessVsMix < 1/3, 'Guessing Only', 
                            ifelse(BFGuessVsMix > 3, 'Mixture', 'Neither')
    )
  ) %>% 
  left_join(paramsDF, ., by = c('participant', 'condition', 'stream','model'))

paramsDF %<>%  mutate(
  latency = replace(latency, guessOrMixture == 'Guessing Only', NA),
  precision = replace(precision, guessOrMixture == 'Guessing Only', NA),
  efficacy = replace(efficacy, guessOrMixture == 'Guessing Only', 0)
)

#################################################################
###Create shape and scale variables. Compute gamma mean and sd###
#################################################################

paramsDF %<>% mutate(
  shape = ifelse(model == 'Gamma', latency, NA),
  scale = ifelse(model == 'Gamma', precision, NA),
) %>% mutate(
  latency = ifelse(model == 'Gamma', shape*scale, latency),
  precision = ifelse(model == 'Gamma', sqrt(shape*(scale^2)), precision)
)

plotData <- expand.grid(
  shape = c(.1,1:10),
  observation = numeric(10000)
)

for(thisShape in c(.1,1:10)){
  theseObs <- rgamma(10000, shape = thisShape, scale = 2)
  plotData %<>% mutate(observation = ifelse(shape == thisShape, theseObs, observation))
}

plotData %<>% mutate(ordered(shape))

ggplot(plotData, aes(x = observation))+
  geom_histogram()+
  facet_grid(cols = vars(shape))

#############
###Density###
#############


densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, stream, efficacy, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(participant, condition, stream, efficacy, latency, shape, scale, precision, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-5,10,.1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(efficacy > 0){
      if(favouredModel == 'Gamma'){
        if(model == 'Gamma'){
          density =dgamma(SPE, shape = shape, scale = scale)
          data.frame(ID = participant,
                     condition = condition,
                     stream = stream,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        } 
      } else if(favouredModel == 'Normal'){
        if(model == 'Normal'){
          density = dnorm(SPE, latency, precision)
          data.frame(ID = participant,
                     condition = condition,
                     stream = stream,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        }
      } else {
        if(model == 'Gamma'){
          density =dgamma(SPE, shape = shape, scale = scale)
          data.frame(ID = participant,
                     condition = condition,
                     stream = stream,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        } else if(model == 'Normal'){
          density = dnorm(SPE, latency, precision)
          data.frame(ID = participant,
                     condition = condition,
                     stream = stream,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        }
      }
    }
  }
)



for(thisID in IDs){
    theseObservations <- allData %>% filter(ID == thisID)
    
    theseDensities <- densities %>% filter(ID == thisID)
    
    thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
    
    thisPlot <- ggplot(theseObservations, aes(x = SPE))+
      geom_histogram(binwidth = 1)+
      geom_line(data = theseDensities, aes(x = SPE, y = density*100, colour = model)) +
      labs(title = paste0('Participant: ', thisIDNoSpace))+
      geom_vline(xintercept = 0, linetype = 'dashed') +
      facet_grid(rows = vars(condition), cols = vars(stream), labeller = 'label_both')
    
    ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'.png'),
           plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
    )
} 

#################################
###Latencies relative to onset###
#################################

paramsDF %<>% mutate(
  latency = latency * (1000/condition),
  latencyRelativeOnset = latency + (1000/condition)*.3
)


test <- paramsDF %>% filter(guessOrMixture == 'Mixture' & favouredModel == model) %>%
  mutate(participant = factor(participant),
         condition = factor(condition),
         stream = factor(stream),
         model = factor(model)) %>%
  anovaBF(latencyRelativeOnset ~ condition*stream*participant, whichRandom = 'participant', data = .)

paramsDF %>% filter(guessOrMixture == 'Mixture' & favouredModel == model) %>%
ggplot(aes(x = factor(condition), y = latencyRelativeOnset))+
  geom_point(aes(colour = participant, shape = model))+
  stat_summary(fun.y = mean, geom = 'point', size = 3, shape = 23)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = .2)+
  facet_wrap(~stream)

paramsDF %>% filter(guessOrMixture == 'Mixture') %>%
  ggplot(aes(x = factor(condition), y = latency))+
  geom_point(aes(colour = participant))+
  stat_summary(fun.y = mean, geom = 'point', size = 3, shape = 23)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = .2)+
  facet_wrap(~stream+model)


latencyMeasuresPlot <- paramsDF %>% 
  melt(id.vars = c('participant', 'condition', 'stream', 'model'), measure.vars = c('latency', 'latencyRelativeOnset')) %>%
  mutate(condition = factor(condition))%>%
  ggplot(aes(x = condition, y = value))+
  geom_point(alpha = .3, size = 2)+
  stat_summary(geom = 'point', fun.y = mean, shape = 23, size = 2)+
  stat_summary(geom = 'errorbar', fun.data = mean_se, width = .3)+
  facet_grid(rows = vars(model), cols = vars(stream, variable), labeller = 'label_both')+
  theme_apa()

ggsave(filename = 'Analysis/Gamma Fits/latencyMeasuresPlot.png',plot = latencyMeasuresPlot, width = 32, height = 18, units = 'cm')



