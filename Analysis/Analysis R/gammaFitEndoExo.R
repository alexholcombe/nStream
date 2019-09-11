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

allData <- read.table('Analysis/allErrorsEndoExoNstream.txt', sep='\t', stringsAsFactors = F, header = T)


allData %<>% rename(targetSP = cuePos0, SPE = responsePosRelative0)

IDs <- allData %>% pull(ID) %>% unique()
cueTypes <-  allData %>% pull(cueType) %>% unique()
nStreams <- c(2,6)

nReplications = 25
numLettersInStream <- 26

paramFiles <- list.files(path = 'Analysis/Gamma Fits', pattern = 'paramsDFEndoExoNStream', full.names = T)

rate <- 6000/90

if(length(paramFiles)>0){ #If there are parameter estimates already saved, read in the newest one
  times <- as.POSIXct(gsub('^.*paramsDFEndoExoNStream_|.csv','',paramFiles), format = '%d-%m-%Y_%H-%M')
  paramsDF <- read.csv(paramFiles[times==max(times)]) 
} else {
  paramsDF <- expand.grid(
    participant = IDs,
    cueType = cueTypes,
    nStreams = nStreams,
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
    group_by(ID, cueType, nStreams) %>%
    summarise(n = n()) %>%
    rename(participant = ID)
  
  nParams <- c('Gamma' = 3, 'Normal' = 3)
  
  for(thisParticipant in IDs){
    for(thisCueType in cueTypes){
      for(thisNStream in nStreams){
        theseData <- allData %>% filter(ID == thisParticipant, cueType == thisCueType, nStreams == thisNStream)
        
        cat('Participant: ', thisParticipant, '. Cue: ', thisCueType, '. nStream: ', thisNStream, '. Fitting gamma model                                  \r', sep = '')
        paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
        cat('Participant: ', thisParticipant, '. Cue: ', thisCueType, '. nStream: ', thisNStream, '. Fitting normal model                                  \r', sep = '')
        paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Normal', paramsN$efficacy),
          latency = replace(latency, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Normal', paramsN$latency),
          precision = replace(precision, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Normal', paramsN$precision),
          val = replace(val, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Normal', paramsN$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream  & model == 'Normal', paramsN$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Normal', paramsN$pLRtest)
        )
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$efficacy),
          latency = replace(latency, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$latency),
          precision = replace(precision, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$precision),
          val = replace(val, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & cueType == thisCueType & nStreams == thisNStream & model == 'Gamma', paramsGamma$pLRtest)
        )
        
        print(paramsDF)
      }
    }
  }
  
  
  
  paramsDF %<>% mutate(
    shape = ifelse(model == 'Gamma', latency, NA),
    scale = ifelse(model == 'Gamma', precision, NA),
    latency = ifelse(model == 'Gamma', shape*scale, latency),
    precision = ifelse(model == 'Gamma', sqrt(shape)*scale, precision)
  )
  
  rate <- 6000/90 #Rate is actually 6 refreshes
  paramsDF %<>% mutate(
    latency = latency*rate,
    precision = precision*rate
  )
  
  BFs <- paramsDF %>% 
    dcast(cueType+nStreams+participant~model, value.var = 'val') %>% 
    mutate(BF = exp(-Normal)/exp(-Gamma))
  
  BFs$favouredModel <- 'Neither'
  
  paramsDF <- BFs %>% mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
    mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
    select(participant, cueType, nStreams, BF, favouredModel) %>%
    left_join(paramsDF, ., by = c('cueType', 'nStreams', 'participant')) #Add all this information to the paramDF 
  
  
  paramsFile <- paste0('Analysis/Gamma Fits/paramsDFEndoExoNStream_', timeStamp, '.csv')

  write.csv(file = paramsFile,
            x = paramsDF,
            row.names = F)
  
}


densities <- paramsDF %>% #Purrr is new to me
  select(participant, cueType, nStreams, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, cueType, nStreams, precision, shape, scale, participant, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,1000,1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        density =dgamma(SPE, shape = shape, scale = scale*rate)
        data.frame(ID = participant,
                   cueType = cueType,
                   nStreams = nStreams,
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
                   cueType = cueType,
                   nStreams = nStreams,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    } else {
      if(model == 'Gamma'){
        density =dgamma(SPE, shape = shape, scale = scale*rate)
        data.frame(ID = participant,
                   cueType = cueType,
                   nStreams = nStreams,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } else if(model == 'Normal'){
        density = dnorm(SPE, latency, precision)
        data.frame(ID = participant,
                   cueType = cueType,
                   nStreams = nStreams,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    }
  }
  )

densities$SPE <- densities$SPE/(6000/90)

for(thisID in IDs){
  for(thisCueType in cueTypes){
      theseObservations <- allData %>% filter(ID == thisID, cueType == thisCueType)
      theseDensities <- densities %>% filter(ID == thisID, cueType == thisCueType)
      
      thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
      
      thisPlot <- ggplot(theseObservations, aes(x = SPE))+
        geom_histogram(binwidth = 1)+
        geom_line(data = theseDensities, aes(x = SPE, y = density*100*66.67, colour = model)) +
        geom_vline(xintercept = 0, linetype = 'dashed')+
        labs(title = paste0('Participant: ', thisIDNoSpace,'. Cue Type: ', thisCueType), x = 'SPE')+
        facet_wrap(~nStreams)
      
      show(thisPlot)
        
      ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/EndoExoN6/',thisIDNoSpace,'_',thisCueType,'.png'),
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


