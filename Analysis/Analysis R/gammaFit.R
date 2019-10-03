###############################
###Date: 2019-04-25 14:18:37###
###Author: Charlie Ludowici####
###############################
library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

source('Analysis/Analysis R/GammaAnalysis.R')

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.csv('Analysis/allErrors.csv', stringsAsFactors = F)


allData %<>% rename(targetSP = correctPos, SPE = responsePosRelative0)

allData %<>% filter(subject != 'AH ')

IDs <- allData %>% pull(subject) %>% unique()
conditions <-  allData %>% pull(condition) %>% unique()

nReplications = 50
numLettersInStream <- 26

allFiles <- list.files(path = 'Analysis/Gamma Fits', full.names = T)
paramFiles <- allFiles[grep(pattern = 'params(?!.*EndoExo)',x = allFiles, perl = T)]


if(length(paramFiles)>0){ #If there are parameter estimates already saved, read in the newest one
  times <- as.POSIXct(gsub('^.*paramsDF_|.csv','',paramFiles), format = '%d-%m-%Y_%H-%M')
  paramsDF <- read.csv(paramFiles[times==max(times)]) 
  noticedTheFrameIssue <- strptime("01-08-2019_00-01", format = '%d-%m-%Y_%H-%M') #This is when I realised that the experimental program presents in terms of monitor frames
  
  paramsDF %<>% mutate(
    shape = ifelse(model == 'Gamma', latency, NA),
    scale = ifelse(model == 'Gamma', precision, NA),
    latency = ifelse(model == 'Gamma', shape*scale, latency),
    precision = ifelse(model == 'Gamma', sqrt(shape)*scale, precision)
  )
  
  if(max(times)<noticedTheFrameIssue){
    rate <- 4000/60 #Rate is actually 4 refreshes
    paramsDF %<>% mutate(
      latency = latency*rate,
      precision = precision*rate
    )
  }
} else {
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
    upper = c(1, 9, 3)
  )
  
  nObs <- allData %>% 
    group_by(subject, condition) %>%
    summarise(n = n()) %>%
    rename(participant = subject)
  
  nParams <- c('Gamma' = 3, 'Normal' = 3)
  
  for(thisParticipant in IDs){
    for(thisCondition in conditions){
      theseData <- allData %>% filter(subject == thisParticipant, condition == thisCondition)
      
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
  
}


densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, condition, precision, shape, scale, participant, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,1000,1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        density =dgamma(SPE, shape = shape, scale = scale*(4000/60))
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
        density =dgamma(SPE, shape = shape, scale = scale*(4000/60))
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
    theseObservations <- allData %>% filter(subject == thisID, condition == thisCondition)
    theseObservations %<>% rename(ID = subject)
    theseDensities <- densities %>% filter(ID == thisID, condition == thisCondition)
    
    thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
    
    thisPlot <- ggplot(theseObservations, aes(x = SPE))+
      geom_histogram(binwidth = 1)+
      geom_line(data = theseDensities, aes(x = SPE, y = density*50*66.67, colour = model)) +
      labs(title = paste0('Participant: ', thisIDNoSpace,'. Condition: ', thisCondition))
      
    ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'_',thisCondition,'.png'),
           plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
    )
  } 
}

normalAnalysis <- analyses(paramsDF, modelKind = 'Normal')

paramsDF %>% filter(model == 'Normal') %>% xtabs(~favouredModel+condition, data = .)

densities %<>% rename(Cue = condition)

allPlot <- allData %>% rename(ID = subject, Cue = condition) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  geom_line(data = densities, aes(x = SPE/rate, y = density*100*66.67, colour = model), size = 1.5) +
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(~ID+Cue, labeller = 'label_both')+
  labs(colour = 'Model')+
  theme_apa()

allPlot

ggsave(filename = 'Analysis/Gamma Fits/GammaPlots/allData.png',
       plot = allPlot,
       height = 18,
       width = 32,
       units = 'cm')

plotHeight <- 19*(9/16)
plotWidth <- 19

ggsave('~/gitCode/nStream/Analysis/Gamma Fits/efficacyEightStream.png', plot = normalAnalysis$Efficacy$Plot,height = plotHeight, width = plotWidth, units = 'cm')
ggsave('~/gitCode/nStream/Analysis/Gamma Fits/latencyEightStream.png', plot = normalAnalysis$Latency$Plot,height = plotHeight, width = plotWidth, units = 'cm')
ggsave('~/gitCode/nStream/Analysis/Gamma Fits/precisionEightStream.png', plot = normalAnalysis$Precision$Plot,height = plotHeight, width = plotWidth, units = 'cm')


