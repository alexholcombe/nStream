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
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')


matlabDataFiles <- list.files(path = 'Data', pattern = '.mat', full.names = T)

IDs <- matlabDataFiles %>% 
  gsub(pattern = 'Data/|(?<=[A-Z][A-Z])_.*',replacement = '', x = ., perl = T) %>% 
  unique()

conditions <-  as.character(c(6,8,12,24))

stimuli <- LETTERS[!LETTERS %in% c('N', 'V')]

nReplications = 20
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
  lower = c(0,0.1,0.1),
  upper = c(1,8,5)
)

parameterBoundsNormal <- data.frame(
  lower = c(0, -4, .001),
  upper = c(1, 4, 3)
)

nObs <- allData %>%
  filter(!fixationReject) %>%
  group_by(ID, condition, stream) %>%
  summarise(n = n()) %>%
  rename(participant = ID)

nParams <- c('Gamma' = 3, 'Normal' = 3)

lastID <- 'None'
lastCondition <- 'None'

startRow <- 1

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
    theseStimuli <- apply(thisMatlab$allLetterOrder[,thisStream,],2,function(i) stimuli[i]) %>% apply(., 1, paste0, collapse = '')
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


allData %>% filter(condition == 6) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~ID + stream)

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

densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, stream, latency, precision, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, condition, stream, precision, participant, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,1000,1)/(1000/12)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        shape = latency
        rate = precision
        density =dgamma(SPE, shape = shape, scale = rate)
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
        shape = latency
        rate = precision
        density =dgamma(SPE, shape = shape, scale = rate)
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
  )


for(thisID in IDs){
  for(thisCondition in conditions){
    for(thisStream in 1:2){
      theseObservations <- allData %>% filter(ID == thisID, condition == thisCondition, stream == thisStream)
      
      theseDensities <- densities %>% filter(ID == thisID, condition == thisCondition, stream == thisStream)
      
      thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
      
      thisPlot <- ggplot(theseObservations, aes(x = SPE))+
        geom_histogram(binwidth = 1)+
        geom_line(data = theseDensities, aes(x = SPE, y = density*50, colour = model)) +
        labs(title = paste0('Participant: ', thisIDNoSpace,'. Condition: ', thisCondition))
        
      ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'_', thisStream, '_',thisCondition,'.png'),
             plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
      )
    }
  } 
}


