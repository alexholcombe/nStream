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


timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.table('Analysis/allErrors.txt', sep = '\t', stringsAsFactors = F, header = T)

allData %<>% rename(SPE = error, targetSP=cuePos0)


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

paramFiles <- list.files(path = 'Analysis/Gamma Fits',
                         pattern = 'paramsDF8.*csv',
                         full.name = T)

if(length(paramFiles)>0){
  paramsDF <- read.csv(paramFiles[1], stringsAsFactors = F)
} else{
  for(thisParticipant in IDs){
    for(thisCondition in conditions){
        theseData <- allData %>% filter(ID == thisParticipant, condition == thisCondition, !fixationReject)
        
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition,'. Fitting gamma model                                  \r', sep = '')
        paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting normal model                                  \r', sep = '')
        paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$efficacy),
          latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$latency),
          precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$precision),
          val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$valGuessing),
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
                         nObs = 100,
                         variables = c('condition', 'participant'))
  
  
  paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
    mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
    mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
    select(participant, condition, BF, favouredModel) %>%
    left_join(paramsDF, ., by = c('participant', 'condition')) #Add all this information to the paramDF 
  
  paramsFile <- paste0('Analysis/Gamma Fits/paramsDF8_', timeStamp, '.csv')
  
  write.csv(file = paramsFile,
            x = paramsDF, 
            row.names = F)
}

#########################################################
###Compute BFs comparing the mixtture to guessing only###
#########################################################

paramsDF %<>% 
  group_by(participant, condition, model) %>% 
  do(guessingDistributionBIC(.)) %>% 
  mutate(
    guessOrMixture = ifelse(BFGuessVsMix < 1/3, 'Guessing Only', 
                            ifelse(BFGuessVsMix > 3, 'Mixture', 'Neither')
    )
  ) %>% 
  left_join(paramsDF, ., by = c('participant', 'condition', 'model'))

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




#############
###Density###
#############


densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, efficacy, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(participant, condition, efficacy, latency, shape, scale, precision, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-5,10,.1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(efficacy > 0){
      if(favouredModel == 'Gamma'){
        if(model == 'Gamma'){
          density =dgamma(SPE, shape = shape, scale = scale)
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
          density =dgamma(SPE, shape = shape, scale = scale)
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
    geom_vline(xintercept = 0, linetype = 'dashed')+
    facet_grid(cols = vars(condition))
  
  ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/8/',thisIDNoSpace,'.png'),
         plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
  )
} 

#################################
###Latencies relative to onset###
#################################

paramsDF %<>% mutate(
  latency = latency * (1000/condition),
  latencyRelativeOnset = latency + (1000/condition)*.6
) %>% mutate(
  latencyRelativeOnset = replace(latencyRelativeOnset, model == 'Gamma', NA)
)

