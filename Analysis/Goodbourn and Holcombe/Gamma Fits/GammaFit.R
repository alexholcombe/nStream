#library(mixRSVP)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(purrr)
devtools::load_all(path = '~/gitCode/mixRSVP/')

rm(list=ls())

setwd('~/gitCode/nStream/Analysis/Goodbourn and Holcombe/')

timeStamp <- format(Sys.time(), format = '%Y-%m-%d %R')

allData <- read.csv('Data and Materials/allData.csv', stringsAsFactors = F)
twoStreamsOneTarget <- allData %>% filter(condition != 1, (exp == 'Exp2' & pool == 'Experienced Observers') | (exp == 'Exp1' & pool == 'SONA'))

nReplications <- 10

nParams <- c('Gamma' = 3, 'Normal' = 3)

areaOfGaussianBin<- function(binStart,binWidth,latency,precision, modelKind = 'Normal') {
  #Calculate area under the unit curve for that bin
  if(modelKind == 'Normal'){
    area <- pnorm(binStart+binWidth,latency,precision,) - pnorm(binStart,latency,precision)
  } else if(modelKind == 'Gamma'){
    area <- pgamma(binStart+binWidth,shape = latency,scale = precision) - pgamma(binStart,shape = latency,scale = precision)
  }
  
  return (area)
}

#######################
###Collapse the SPEs###
#######################

twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #
  dcast(ID+pool+trial+block+condition~stream, value.var = 'SPE') %>%
  rename('One' = '1', 'Two' = '2') %>%
  mutate(SPE = ifelse(is.na(One), Two, One)) 

twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #Add targetSP
  dcast(ID+pool+trial+block+condition~stream, value.var = 'targetSP') %>%
  rename('One' = '1', 'Two' = '2') %>%
  mutate(targetSP = ifelse(is.na(One), Two, One)) %>%
  select(ID,trial, block, targetSP) %>%
  inner_join(twoStreamsOneTargetWide, by = c('ID','trial', 'block'))


nObs <- twoStreamsOneTargetWide %>%
  group_by(ID, condition) %>% 
  rename(participant = ID) %>%
  summarise(n = n())

IDs <- unique(twoStreamsOneTargetWide$ID)

conditions <-  unique(twoStreamsOneTargetWide$condition)

paramFiles <- list.files('Gamma Fits', pattern = '^.*paramsGamma.*csv$',full.names = T)
if(length(paramFiles)>0){#Load params if they're there
  
  times <- gsub(x = paramFiles, pattern = "^.*paramsGamma_|\\.csv$", replacement = '') %>% as.POSIXct(format = '%Y-%m-%d %R')
  mostRecent <- which(times == max(times))
  paramsDF <- read.csv(paramFiles[mostRecent])
  
}else{ #Fit models
  paramsDF <- expand.grid(
    participant = IDs,
    pool = character(1),
    condition = conditions,
    efficacy = numeric(1),
    latency = numeric(1),
    precision = numeric(1),
    val = numeric(1),
    valGuessing = numeric(1),
    pLRtest = 999,
    model = c('Gamma','Normal'),
    stringsAsFactors = F
  )
  
  
  for(thisParticipant in IDs){
    theseParticipantSPEs <- twoStreamsOneTargetWide %>%
      filter(ID == thisParticipant)
    for(thisCondition in unique(theseParticipantSPEs$condition)){
      
      theseData <-  filter(theseParticipantSPEs, condition == thisCondition)
      
      parameterBoundsGamma <- data.frame(
        lower = c(0,0.1,0.1),
        upper = c(1,5,5)
      )
      
      
      
      cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting gamma model                                  \r', sep = '')
      paramsGamma <- theseData %>% analyzeOneConditionDF(24, parameterBounds(modelKind = 'Gamma'), nReplicates = nReplications, modelKind = 'Gamma')
      cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting normal model                                  \r', sep = '')
      paramsN <- theseData %>% analyzeOneConditionDF(24, parameterBounds(modelKind = 'Normal'), nReplicates = nReplications, modelKind = 'Normal')
      
      paramsDF %<>% mutate(
        pool = replace(pool, participant == thisParticipant & condition == thisCondition & model == 'Normal', theseData$pool[1]),
        efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$efficacy),
        latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$latency),
        precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$precision),
        val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$val),
        valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition  & model == 'Normal', paramsN$valGuessing),
        pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$pLRtest)
      )
      
      paramsDF %<>% mutate(
        pool = replace(pool, participant == thisParticipant & condition == thisCondition & model == 'Gamma', theseData$pool[1]),
        efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$efficacy),
        latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$latency),
        precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$precision),
        val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$val),
        valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$valGuessing),
        pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$pLRtest)
      )
    }
  }
  
  paramsDF %<>% filter(pLRtest != 999)
  
  
  paramsFileName <- paste0('Gamma Fits/paramsGamma_', timeStamp, '.csv')
  
  write.csv(file = paramsFileName, x = paramsDF,row.names = F)
}



paramsDF %<>% mutate(
  efficacy = ifelse(pLRtest >=.05, 0, efficacy),
  latency = ifelse(pLRtest >=.05, NA, latency),
  precision = ifelse(pLRtest >=.05, NA, precision)
) %>% filter(pLRtest<.05)



BICs <- paramsDF %>% 
  dcast(participant+condition~model, value.var = 'val') %>%
  mutate(
    BF = exp(-Normal)/exp(-Gamma),
    BICgam = (2*Gamma)+3*log(100),
    BICnorm = (2*Normal)+3*log(100),
  )


paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
  mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
  mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
  select(participant, condition, BF, favouredModel) %>%
  left_join(paramsDF, ., by = c('participant', 'condition')) #Add all this information to the paramDF 
  


densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, latency, precision, efficacy, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(condition, latency, precision, participant, efficacy, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-17,17,.1)
    
    ###Guessing Distribution bounds###
    minSP <- twoStreamsOneTargetWide %>% filter(ID == participant, condition == condition) %>% pull(targetSP) %>% min
    maxSP <- twoStreamsOneTargetWide %>% filter(ID == participant, condition == condition) %>% pull(targetSP) %>% max
    targetSP <- twoStreamsOneTargetWide %>% filter(ID == participant, condition == condition) %>% pull(targetSP)
    
    minSPE <- 1 - maxSP
    maxSPE <- 24 - minSP
    print(minSPE)
    print(maxSPE)
    
    ###Guessing Probs###
    guessingDist <- createGuessingDistribution(minSPE, maxSPE, targetSP,24)
    guessingDist <- guessingDist/sum(guessingDist)
    
    guessingDist <- data.frame(SPE=minSPE:maxSPE, prob = guessingDist)
    
    
    ##Bin the SPEs because guessing is the same within a bin##
    guessingSPE <- c((-min(SPE)):max(SPE))[cut(SPE,breaks = (-min(SPE)):max(SPE) , include.lowest = T, labels = F)]
    
    ##assign a probability to an SPE based on its bin##
    guessingFreqs <- sapply(guessingSPE,FUN = function(x){guessingDist$prob[guessingDist$SPE == x]})
    
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        shape = latency
        rate = precision
        
        binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,latency,precision, favouredModel)
  
        mixtureFreq = binAreasEfficacy*efficacy + guessingFreqs * (1-efficacy)
        
        data.frame(ID = participant,
                   SPE = SPE,
                   Freq = mixtureFreq,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } 
    } else if(favouredModel == 'Normal'){
      if(model == 'Normal'){
        binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,latency,precision, favouredModel)
        
        mixtureFreq = binAreasEfficacy*efficacy + guessingFreqs * (1-efficacy)
        
        data.frame(ID = participant,
                   SPE = SPE,
                   Freq = mixtureFreq,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    } else {
      if(model == 'Gamma'){
        shape = latency
        rate = precision
        binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,latency,precision, 'Gamma')
        
        mixtureFreq = binAreasEfficacy*efficacy + guessingFreqs * (1-efficacy)
        
        data.frame(ID = participant,
                   SPE = SPE,
                   Freq = mixtureFreq,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } else if(model == 'Normal'){
        binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,latency,precision, 'Normal')
        
        mixtureFreq = binAreasEfficacy*efficacy + guessingFreqs * (1-efficacy)
        
        data.frame(ID = participant,
                   SPE = SPE,
                   Freq = mixtureFreq,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    }
  }
  )


for(thisID in IDs){
  for(condition in 2:4)
  theseObservations <- twoStreamsOneTargetWide %>% filter(ID == thisID.)
  theseDensities <- densities %>% filter(ID == thisID)
  
  thisPlot <- ggplot(theseObservations, aes(x = SPE))+
    geom_histogram(binwidth = 1)+
    geom_line(data = theseDensities, aes(x = SPE, y = density*50, colour = model)) +
    theme_apa
    facet_wrap(~ID,labeller = 'label_both')
  
  show(thisPlot)
  
  ggsave(filename = paste0('Gamma Fits/GammaPlots/',thisID,'.png'),
         plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
  )
  
}


if(IDs == 'PG'){
  #Print pat diagnostic plots
  
  gammaPlot <- plot_hist_with_fit(
    df = theseData, 
    minSPE=-17, 
    maxSPE = 17, 
    targetSP = theseData$targetSP,
    numItemsInStream = 24, 
    TRUE, 
    TRUE, 
    TRUE, 
    modelKind = 'Gamma', 
    params = paramsDF[1,]
    )
  
  normalPlot <- plot_hist_with_fit(
    df = theseData, 
    minSPE=-17, 
    maxSPE = 17, 
    targetSP = theseData$targetSP,
    numItemsInStream = 24, 
    TRUE, 
    TRUE, 
    TRUE, 
    modelKind = 'Normal', 
    params = paramsDF[2,]
  )
  
  ggsave(filename = 'gammaPlot.png',
         plot = gammaPlot,
         width = 32,
         height = 18,
         units = 'cm')
  
  ggsave(filename = 'normalPlot.png',
         plot = normalPlot,
         width = 32,
         height = 18,
         units = 'cm')
    
}

# 
# params %<>% mutate(xMin = ifelse((latency - precision) < 0, 0, latency - precision ))
# 
# 
# densities <- dtruncnorm(seq(-10,15, .01), a = params$xMin, b = Inf, mean = params$latency, sd = params$precision)
# 
# densityDF <- data_frame(
#   SPE = seq(-10,15, .01)-.5,
#   dens = densities
# )
# 
# testData %<>% mutate(Densities = dtruncnorm(SPE, a = params$xMin, b = Inf, mean = params$latency, sd = params$precision))
# 
# x <- hist(testData$SPE,breaks = -10:19)
# 
# freqData <- data.frame(
#   SPE = x$breaks[1:29],
#   counts = x$counts
# )
# 
# freqData %<>% mutate(
#   Density = dtruncnorm(SPE, a = params$xMin, b = Inf, mean = params$latency, sd = params$precision)
# )  
# 
# ggplot(freqData, aes(x = SPE, y = counts))+
#   geom_bar(stat = 'identity', aes(fill = Density))+
#   scale_fill_gradient(low = 'blue', high = 'red')
# 
# formulaLHS <- paste0(c(IDvars, 'nParams'), collapse = '+')
# 
# formulaText <- paste0(formulaLHS, '~ model')
# 
# castFormula <- as.formula(formulaText)
# 
# paramsWide <- dcast(data = paramsDF, formula = castFormula, value.var = 'val', margins = c('nParams'))
# 
# 
