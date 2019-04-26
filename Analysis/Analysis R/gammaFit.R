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
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.csv('Analysis/allErrors18Streams.txt', stringsAsFactors = F)


IDs <- allData %>% pull(ID) %>% unique()
conditions <-  allData %>% pull(condition) %>% unique()

nReplications = 20
numLettersInStream <- 24

paramsDF <- expand.grid(
  participant = IDs,
  condition = conditions,
  efficacy = numeric(1),
  latency = numeric(1),
  precision = numeric(1),
  val = numeric(1),
  valGuessing = numeric(1),
  pLRtest = 999,
  model = c('Gamma','Normal')
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
  group_by(ID, condition) %>%
  summarise(n = n()) %>%
  rename(participant = ID)

nParams <- c('Gamma' = 3, 'Normal' = 3)

for(thisParticipant in IDs){
  for(thisCondition in conditions){
    theseData <- allData %>% filter(ID == thisParticipant, condition == thisCondition)
    
    cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting gamma model                                  \r', sep = '')
    paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
    cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting normal model                                  \r', sep = '')
    paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
    
    paramsDF %<>% mutate(
      efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$efficacy),
      latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$latency),
      precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$precision),
      val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$val),
      valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition  & model == 'Normal', paramsN$valGuessing),
      pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$pLRtest)
    )
    
    paramsDF %<>% mutate(
      efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$efficacy),
      latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$latency),
      precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$precision),
      val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$val),
      valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$valGuessing),
      pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$pLRtest)
    )
  }
}


BICs <- paramsDF %>%
  compareMixtureModels(params = .,
                       nParam = nParams,
                       nObs = nObs)


paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
  mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
  mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
  select(participant, condition, BF, favouredModel) %>%
  left_join(paramsDF, ., by = c('participant', 'condition')) #Add all this information to the paramDF 

paramsFile <- paste0('Analysis/Gamma Fits/paramsDF_', timeStamp, '.csv')

write.csv(file = paramsFile,
          x = paramsDF, 
          row.names = F)

densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, latency, precision, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, condition, precision, participant, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,1000,1)/(1000/12)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        shape = latency
        rate = precision
        density =dgamma(SPE, shape = shape, scale = rate)
        data.frame(ID = participant,
                   condition = condition,
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
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } else if(model == 'Normal'){
        density = dnorm(SPE, latency, precision)
        data.frame(ID = participant,
                   condition = condition,
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
    theseObservations <- allData %>% filter(ID == thisID, condition == thisCondition)
    
    theseDensities <- densities %>% filter(ID == thisID, condition == thisCondition)
    
    thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
    
    thisPlot <- ggplot(theseObservations, aes(x = SPE))+
      geom_histogram(binwidth = 1)+
      geom_line(data = theseDensities, aes(x = SPE, y = density*50, colour = model)) +
      labs(title = paste0('Participant: ', thisIDNoSpace,'. Condition: ', thisCondition))
      
    ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'_',thisCondition,'.png'),
           plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
    )
  } 
}

allPlot <- allData %>% 
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  geom_line(data = densities, aes(x = SPE, y = density*50, colour = model)) +
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~ID+condition)

ggsave(filename = 'allData.png',
       plot = allPlot,
       height = 18,
       width = 32,
       units = 'cm')

