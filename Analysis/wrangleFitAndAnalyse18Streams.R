###Parameter Analyses###
rm(list=ls())
library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(BayesFactor)

setwd('~/gitCode/nStream/')  #Charlie-specific

source('ggplotElements.R')
theme_set(theme_apa(base_size = 20)) 


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

testPositions = T

plots <- F #if true, create plots
if(plots){
  savePlots <- F #save plots?
}

saveIndividualTSV <- F #Save data files?
saveAllErrorsTSV <- F

participantPlots <- T

bootstrapPredictions <- F #Should we bootstrap means and CIs for model predictions?
nRepetitions <- 1000 #number of bootstrap repetitions

dataPath <- 'rawData/18Streams' #'rawData/' for 2 vs 8


pracTrials <- 1:20
maxTrials <- 270 #one participant had the eyetracker crash and ended up doing ~325 trials

group = 'SONA/18Streams'

if(plots){
  if(savePlots){
    if(!'plots' %in% list.dirs(full.names=F)){
      dir.create('plots')
    }
    if(!group %in% list.dirs(path = 'plots', full.names = F)){
      dir.create(paste0('plots/',group))
    }
  }
}
 
files <- list.files(pattern = '^18[A-Z][A-Z].*\\.txt$', path = dataPath, full.names = T)
print(files)


widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 42 #cm
pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180)

rate = 1000/12

eyetrackerFiles <- list.files(path = 'rawData/18Streams/Eyetracker',full.names = T)

criterion = 1 #fixation reject criterion

skew <- function(x){
  denom <- (sd(x,na.rm=T)^3)*(length(which(!is.na(x)))-1)*(length(which(!is.na(x)))-2)
  deviation <- x[!is.na(x)] - mean(x,na.rm=T)
  numer <- sum(deviation^3)*length(which(!is.na(x)))
  numer/denom
}

totalRows <- length(files)*250

allErrors <- data.frame(
  exp = character(totalRows), 
  condition = character(totalRows), 
  SPE = numeric(totalRows), 
  targetSP = numeric(totalRows),
  ID = character(totalRows), 
  fixationReject = logical(totalRows), 
  button = numeric(totalRows), 
  ring = numeric(totalRows), 
  stringsAsFactors = F
  )

startRow <- 1

for(dataset in files){
      temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)
      
      if(testPositions){
        print(xtabs(~ring+nStreams, data = temp))
        print(xtabs(~whichStreamCuedAngle0+ring, data = temp))
      }
      
      temp <- temp[-pracTrials,]
      participant <- temp$subject[1]
      
      temp %<>% mutate(responsePos = cuePos0+responsePosRelative0)
      
      thisEyetrackerFile <- eyetrackerFiles[grepl(paste0('.*',participant,'.*'), eyetrackerFiles)]
      temp %<>% mutate(fixationReject = FALSE)
      
      if(length(thisEyetrackerFile)>0){
        theseFixations <- read.table(thisEyetrackerFile, sep='\t', stringsAsFactors = F, header = T)
        
        theseFixations %<>% mutate(CURRENT_FIX_X_DEG = CURRENT_FIX_X/pixelsPerDegree)
        theseFixations %<>% mutate(CURRENT_FIX_Y_DEG = CURRENT_FIX_Y/pixelsPerDegree)
        
        theseFixations %<>% filter(!TRIAL_INDEX %in% pracTrials)
        
        theseFixations %<>% mutate(fixationDistance = 0)
        
        
        for(index in unique(theseFixations$TRIAL_INDEX)){ #If the trial has multiple fixations, compare each fixation to the initial fixation and reject that trial if it falls outside of a 1º radius circle centered on the initial fix
          
          theseFixationsThisTrial = which(theseFixations$TRIAL_INDEX==index)
          nFixationsThisTrial <- length(theseFixationsThisTrial)
          
          if(nFixationsThisTrial>1){
            
            initialFixationX = theseFixations$CURRENT_FIX_X_DEG[theseFixationsThisTrial[1]]
            initialFixationY = theseFixations$CURRENT_FIX_Y_DEG[theseFixationsThisTrial[1]]
            
            for(thisFixationRow in theseFixationsThisTrial){ #iterate over DF rows rather than fixations
              if(thisFixationRow == theseFixationsThisTrial[1]){
                #skip the first fixation,
              } else {
                
                xVector <- theseFixations$CURRENT_FIX_X_DEG[thisFixationRow] - initialFixationX
                yVector <- theseFixations$CURRENT_FIX_Y_DEG[thisFixationRow] - initialFixationY
                
                fixationDistance <- sqrt(xVector^2 + yVector^2)
                
                theseFixations$fixationDistance[thisFixationRow] <- fixationDistance
                
                if(fixationDistance>=criterion){
                  if(!theseFixations$CURRENT_FIX_BLINK_AROUND[thisFixationRow] %in% c('BEFORE','AFTER') ){
                    temp %<>% mutate(fixationReject = replace(fixationReject, trialnum == index-1, TRUE))
                  }
                }
              }
            }
          }
        }
        
        if(mean(temp$fixationReject)>.4){ #Don't add their data if >2/5ths of the trials were rejected
          next
        }
        
      }
      #Create densities for the SPE with fixation rejections removed
      
      streamColumns <- grep('streamLtrSequence', colnames(temp))
      
      endRow = startRow + nrow(temp) - 1
      
      allErrors[startRow:endRow,] <- temp[,c(1,4,16,8,3,37,7,5)]
      
      startRow <- endRow + 1
      
      if(saveIndividualTSV){
        write.table(twoStreams[!twoStreams$fixationReject,], paste0('wrangledData/',group,'/twoStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
        write.table(eighteenStreams[!eighteenStreams$fixationReject,], paste0('wrangledData/',group,'/eighteenStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
      }
      
      print(mean(temp$fixationReject))      
}


nStreams <- allErrors %>% pull(condition) %>% unique
participants <- allErrors %>% pull(ID) %>% unique

nReps <- 100

bounds <- parameterBounds()
bounds$upper[3] <- 3


runAnyway <- TRUE #If TRUE, fit models regardless of the presence of a parameter file. 
plots <- FALSE

nParamFiles <- length(list.files(path = 'modelOutput',pattern ='parameterEstimates.*\\.csv',full.names = T)) #How many parameter DFs are saved?

if(nParamFiles>0){
  print('we out here')
  paramFiles <- list.files(path = 'modelOutput',pattern ='parameterEstimates.*\\.csv',full.names = T) #What are the saved param files?
  
  splits <- strsplit(paramFiles, 'Estimates|(?<=[0-9][0-9])_|\\.csv',perl = T) #split out the date stamps from the param files names
  
  theseDates <- lapply(splits,FUN =function(x) paste(x[2],x[3])) %>% 
    unlist %>% 
    as.POSIXct(.,format='%d-%m-%Y %H-%M-%S') #Format them as POSIXct (datetime) 
  
  whichMaxDate <- which(theseDates == max(theseDates)) #which is the newest?
  params <- read.csv(paramFiles[whichMaxDate]) #load the newest
  
  #Empty space in the param DF for the un-modelled participants
  notModelled <- allErrors %>% filter(., !ID %in% params$ID) %>% pull(ID) %>% unique
  
  unModelledRows <- expand.grid(
    ID = factor(notModelled),
    condition = factor(unique(allErrors$condition)),
    efficacy = 999,
    latency = 999,
    precision = 999,
    val = 999,
    valGuessing = 999,
    pLRtest = 999,
    stringsAsFactors = F
  )
  
  params <- rbind(params, unModelledRows)
  
} else {
  notModelled <- allErrors %>% pull(ID) %>% unique
  
  params <- expand.grid(
    ID = factor(notModelled),
    condition = factor(unique(allErrors$condition)),
    efficacy = 999,
    latency = 999,
    precision = 999,
    val = 999,
    valGuessing = 999,
    pLRtest = 999,
    stringsAsFactors = F
  )
}  

if(length(notModelled)>0){
  for(thisParticipant in notModelled){
    for(thisCondition in unique(allErrors$condition)){
      print(paste0('Participant: ', thisParticipant, '. Condition: ', thisCondition))
      theseParams <- allErrors %>% filter(., condition == thisCondition, ID == thisParticipant) %>% analyzeOneCondition(., 24, bounds, nReps)
        
      if(theseParams$pLRtest<.05){
        params %<>%
          mutate(efficacy=replace(efficacy, ID == thisParticipant & condition == thisCondition , theseParams$efficacy)) %>%
          as.data.frame()
        
        params %<>%
          mutate(latency=replace(latency, ID == thisParticipant & condition == thisCondition, theseParams$latency)) %>%
          as.data.frame()
        
        params %<>%
          mutate(precision=replace(precision, ID == thisParticipant & condition == thisCondition, theseParams$precision)) %>%
          as.data.frame()
      } else {
        params %<>%
          mutate(efficacy=replace(efficacy, ID == thisParticipant & condition == thisCondition, 0)) %>%
          as.data.frame()
        
        params %<>%
          mutate(latency=replace(latency, ID == thisParticipant & condition == thisCondition, NaN)) %>%
          as.data.frame()
        
        params %<>%
          mutate(precision=replace(precision, ID == thisParticipant & condition == thisCondition, NaN)) %>%
          as.data.frame()
      }
        
      params %<>%
        mutate(val=replace(val, ID == thisParticipant & condition == thisCondition, theseParams$val)) %>%
        as.data.frame()
      
      params %<>%
        mutate(valGuessing=replace(valGuessing, ID == thisParticipant & condition == thisCondition, theseParams$valGuessing)) %>%
        as.data.frame()
      
      params %<>%
        mutate(pLRtest=replace(pLRtest, ID == thisParticipant & condition == thisCondition, theseParams$pLRtest)) %>%
        as.data.frame()
    }
  }
  write.csv(params, paste0('modelOutput/parameterEstimates',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.csv'),row.names = F)
}

nParticipants <- params %>% pull(ID) %>% unique %>% length

params %<>% mutate(stringID = ID)
params %<>% mutate(ID = as.factor(rep(1:nParticipants, times = 3)))
params %<>% mutate(condition = ordered(condition))
paramsForAnalysis <- params %>% filter(efficacy>.1 & efficacy < bounds[1,2] & efficacy > bounds[1,1] & latency < bounds[2,2] & latency > bounds[2,1] & precision < bounds[3,2] & precision > bounds[3,1])
paramsForAnalysis %<>% mutate(latency = latency*rate)
paramsForAnalysis %<>% mutate(precision = precision *rate)


#######################
###Efficacy Analyses###
#######################

efficacyBF <- anovaBF(efficacy ~ condition + ID, 
                      data=paramsForAnalysis,
                      whichRandom = 'ID'
) 


#Only evidence for an effect of ring

ggplot(paramsForAnalysis, aes(x=condition, y = efficacy))+
  #geom_violin(position = position_dodge(.9))+
  geom_jitter(position = position_dodge(.9))+
  geom_line(aes(group = ID, colour = ID))+
  stat_summary(geom = 'point',fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9))+
  lims(y=c(0,1))


latencyBF <- anovaBF(latency ~ condition + ID, 
                     data=paramsForAnalysis,
                     whichRandom = 'ID'
)

ggplot(paramsForAnalysis, aes(x=condition, y = latency))+
  #geom_violin(position = position_dodge(.9))+
  geom_point(position = position_dodge(.9))+
  geom_line(aes(group = ID, colour = ID))+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9))


precisionBF <- anovaBF(precision ~ condition + ring + ID, 
                       data=paramsForAnalysis,
                       whichRandom = 'ID'
)

ggplot(paramsForAnalysis, aes(x=condition, y = precision))+
  #geom_violin(position = position_dodge(.9))+
  geom_jitter(position = position_dodge(.9))+
  geom_line(aes(group = factor(ID), colour = factor(ID)))+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9))+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9))
########################
###Plots with Density###
########################

paramsForAnalysis %<>% mutate(ID=stringID) %>%
  as.data.frame() #Convert the problem name back for plotting

if(participantPlots){
  for(thisParticipant in unique(paramsForAnalysis$ID)){
    for(thisCondition in unique(paramsForAnalysis$condition)){
      thisEfficacy <- paramsForAnalysis %>% filter(ID == thisParticipant & condition == thisCondition) %>% pull(efficacy) 
      thisLatency <- paramsForAnalysis %>% filter(ID == thisParticipant & condition == thisCondition) %>% pull(latency) %>% `/`(1000/12)
      thisPrecision <- paramsForAnalysis %>% filter(ID == thisParticipant & condition == thisCondition) %>% pull(precision) %>% `/`(1000/12)
      
      theseErrors <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
      print(paste0('Participant: ', thisParticipant, '. Condition: ', thisCondition,'. N = ', nrow(theseErrors)))
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
      
      thisFileName <- paste0('modelOutput/Plots/',thisCondition,'/',thisParticipant,'-',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.png')
      ggsave(filename = thisFileName, thisPlot,width = 16, height = 9)
    }
  }
}

