###Parameter Analyses###
rm(list=ls())
library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(BayesFactor)
library(reshape2)

setwd('~/gitCode/nStream/')  #Charlie-specific

PowerPointSize <- c(29.21, 12.09)
PowerPointUnit <- 'cm'

source('ggplotElements.R')
theme_set(theme_apa(base_size = 20))

nIterations = 10000 #for posterior sampling

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

participantPlots <- F

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

BFs <- read.csv('modelOutput/18Streams/BF_ByParticipant.csv')

BFs %<>% mutate(preferredModel = '') %>% 
  mutate(preferredModel = replace(preferredModel, BF>3, 'Buffering')) %>% 
  mutate(preferredModel = replace(preferredModel,BF<(1/3), 'Attention')) %>% 
  mutate(preferredModel = replace(preferredModel, BF<3 & BF>(1/3), 'Neither'))

BFs %>% dcast( Participant ~ Group, value.var = 'BF') %>% write.csv(x = .,
                                                                    file = 'modelOutput/18Streams/BFsForManuscript.csv',
                                                                    row.names = F)

BFCounts <- BFs %>% group_by(Group) %>% summarise(Buffering = length(which(preferredModel=='Buffering')),
                                                  Attention = length(which(preferredModel == 'Attention')),
                                                  Neither = length(which(preferredModel == 'Neither')))



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
  whichStreamCuedAngle = numeric(totalRows),
  stringsAsFactors = F
  )

startRow <- 1

createdMatlab = FALSE

for(dataset in files){
      temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)

      if(testPositions){
        print(xtabs(~ring+nStreams, data = temp))
        print(xtabs(~whichStreamCuedAngle0+ring, data = temp))
      }

      participant <- temp$subject[1]

      if(!grepl('_2', participant)){ #if there's _2 in the participant. It's because the eyetracker crashed, so the first 20 trials are not actually practice.
        temp <- temp[-pracTrials,]
      }

      temp %<>% mutate(responsePos = cuePos0+responsePosRelative0)

      thisEyetrackerFile <- eyetrackerFiles[grepl(paste0('.*',participant,'(?!_2).*'), eyetrackerFiles, perl =T)]
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


      if(!createdMatlab){
        tempNRows <- nrow(temp)
        matlabAllRows <- tempNRows+totalRows
        allErrorsMatlab = temp[(tempNRows+1):matlabAllRows,]
        createdMatlab = TRUE
      }

      allErrors[startRow:endRow,] <- temp[,c(1,4,16,8,3,37,7,5,12)]
      allErrorsMatlab[startRow:endRow,] <- temp

      startRow <- endRow + 1

      print(mean(temp$fixationReject))
}

###18LS4 and 18LS4_2 are the same participant. The tracker crashed###
LS4TRIALS <- which(allErrors$ID == '18LS4'|allErrors$ID=='18LS4_2')
DROPTHESE <- (max(LS4TRIALS)-(length(LS4TRIALS)-249)):max(LS4TRIALS) #The trials to drop from the analysis

allErrors <- allErrors[-DROPTHESE,]

allErrors %<>% mutate(ID = replace(ID, ID == '18LS4_2', '18LS4'))

allErrors %<>% filter(ID!='')
write.csv(allErrors, "Analysis/allErrors18Streams.txt", row.names = F)


LS4TRIALS <- which(allErrorsMatlab$subject == '18LS4'|allErrorsMatlab$subject=='18LS4_2')
DROPTHESE <- (max(LS4TRIALS)-(length(LS4TRIALS)-249)):max(LS4TRIALS) #The trials to drop from the analysis

allErrorsMatlab <- allErrorsMatlab[-DROPTHESE,]

allErrorsMatlab %<>% mutate(subject = replace(subject, subject == '18LS4_2', '18LS4'))

allErrorsMatlab %<>% filter(subject!='')


if(saveIndividualTSV){
  for(thisNStream in c('2','6','18')){
    for(thisParticipant in unique(allErrors$ID)){
      fileName <- paste0('wrangledData/SONA/18Streams/', thisNStream,'/', thisParticipant,'.txt')
      allErrorsMatlab %>%
        filter(subject == thisParticipant & nStreams == thisNStream & !fixationReject) %>%
        write.table(.,
                    file = fileName,
                    sep = '\t',
                    col.names = T,
                    row.names = F)
    }
  }
}


nStreams <- allErrors %>% pull(condition) %>% unique
participants <- allErrors %>% pull(ID) %>% unique

nReps <- 100

bounds <- parameterBounds()
bounds$upper[3] <- 3


runAnyway <- TRUE #If TRUE, fit models regardless of the presence of a parameter file.
plots <- FALSE

nParamFiles <- length(list.files(path = 'modelOutput/18Streams',pattern ='parameterEstimates.*\\.csv',full.names = T)) #How many parameter DFs are saved?

if(nParamFiles>0){
  print('we out here')
  paramFiles <- list.files(path = 'modelOutput/18Streams',pattern ='parameterEstimates.*\\.csv',full.names = T) #What are the saved param files?

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

descriptives <- params %>% group_by(condition) %>% summarise(mean_efficacy = mean(efficacy, na.rm=T),
                                                             sd_efficacy = sd(efficacy, na.rm=T),
                                                             mean_latency = mean(latency, na.rm=T)*rate,
                                                             sd_latency = sd(latency, na.rm=T)*rate,
                                                             mean_precision = mean(precision, na.rm=T)*rate,
                                                             sd_precision = sd(precision, na.rm=T)*rate)

write.csv(x = descriptives,
          file = 'modelOutput/18Streams/descriptives.csv',
          row.names = F)

nParticipants <- params %>% pull(ID) %>% unique %>% length

params %<>% mutate(stringID = ID)
params %<>% mutate(ID = 999)

for(thisID in unique(params$stringID)){
  thisN <- which(unique(params$stringID) == thisID)
  params %<>% mutate(ID = replace(ID, stringID == thisID, thisN))
}

params %<>% mutate(ID = factor(ID))

params %<>% mutate(condition = factor(condition, levels = c(2,6,18), ordered = T))
paramsForAnalysis <- params %>% filter(efficacy>.1 & efficacy < bounds[1,2] & efficacy > bounds[1,1] & latency < bounds[2,2] & latency > bounds[2,1] & precision < bounds[3,2] & precision > bounds[3,1])
paramsForAnalysis %<>% filter(ID != '18TR1') #No eyetracking. Bug in number of trials?
paramsForAnalysis %<>% mutate(latency = latency*rate)
paramsForAnalysis %<>% mutate(precision = precision *rate)

for(thisID in paramsForAnalysis$stringID){
  if(length(which(paramsForAnalysis$stringID==thisID))<3){
    paramsForAnalysis %<>% filter(stringID!=thisID)
  }
}


#######################
###Efficacy Analyses###
#######################

efficacyFullAgainstNull <- anovaBF(efficacy ~ condition + ID,
                      data=paramsForAnalysis,
                      whichRandom = 'ID'
)

BayesFactorLabel <- efficacyFullAgainstNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)

#Only evidence for an effect of ring

efficacyPlot = ggplot(paramsForAnalysis, aes(x=condition, y = efficacy))+
  #geom_violin(position = position_dodge(.9))+
  geom_point(alpha=1, colour = '#dca951', size = 4)+
  geom_line(aes(group = ID),alpha = .3)+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), alpha = .7, size = 5)+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, alpha = .7)+
  annotate(x = 1.25, y = .45, label = BayesFactorLabel, geom = 'text',size = 5,parse = T)+
  labs(x = "Number of Streams", y = "Efficacy [1 - p(guess)]")+
  lims(y = c(0,1))

efficacyPlot


latencyBFFullVSNull <- anovaBF(latency ~ condition + ID,
                     data=paramsForAnalysis,
                     whichRandom = 'ID'
)

samples <- posterior(latencyBFFullVSNull, iterations=nIterations) #http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
consistent <- (samples[,'condition-2']<samples[,'condition-6'] & samples[,'condition-6']<samples[,'condition-18'])
latencyBFOrderedVSFull <- (sum(consistent)/nIterations)/(1/6)

latencyBFOrderedVSNull <- as.vector(latencyBFFullVSNull)*latencyBFOrderedVSFull
latencyBFOrderedVSNull


ttestBF(x = paramsForAnalysis$latency[paramsForAnalysis$condition==2],
        y = paramsForAnalysis$latency[paramsForAnalysis$condition==6],
        paired = T)

ttestBF(x = paramsForAnalysis$latency[paramsForAnalysis$condition==6],
        y = paramsForAnalysis$latency[paramsForAnalysis$condition==18],
        paired = T)

BayesFactorLabelOne <- latencyBFOrderedVSNull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~Null]==', .)
BayesFactorLabelTwo <- latencyBFOrderedVSFull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~No-Order]==', .)



latencyPlot <- ggplot(paramsForAnalysis, aes(x=condition, y = latency))+
  #geom_violin(position = position_dodge(.9))+
  geom_point(alpha=1, colour = '#dca951', size = 4)+
  geom_line(aes(group = ID),alpha = .3)+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), alpha = .7, size = 3)+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, alpha = .7)+
  scale_colour_brewer(palette = 'Spectral')+
  annotate(x = 1.25, y = -50, label = BayesFactorLabelOne, geom = 'text',size = 5,parse = T)+
  annotate(x = 1.25, y = -35, label = BayesFactorLabelTwo, geom = 'text',size = 5,parse = T)+
  lims(y = c(-50,200))+
  labs(x = 'Number of Streams', y = 'Latency (ms)')

latencyPlot


precisionBFFullVSNull <- anovaBF(precision ~ condition + ID,
                               data=paramsForAnalysis,
                               whichRandom = 'ID'
)

samples <- posterior(precisionBFFullVSNull, iterations=nIterations) #http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
consistent <- (samples[,'condition-2']>samples[,'condition-6'] & samples[,'condition-6']>samples[,'condition-18'])
precisionBFOrderedVSFull <- (sum(consistent)/nIterations)/(1/6)

precisionBFOrderedVSNull <- as.vector(precisionBFFullVSNull)*precisionBFOrderedVSFull
precisionBFOrderedVSNull

BayesFactorLabelOne <- precisionBFOrderedVSNull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~Null]==', .)
BayesFactorLabelTwo <- precisionBFOrderedVSFull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~No-Order]==', .)

precisionPlot <- ggplot(paramsForAnalysis, aes(x=condition, y = precision))+
  #geom_violin(position = position_dodge(.9))+
  geom_point(alpha=1, colour = '#dca951', size = 4)+
  geom_line(aes(group = ID),alpha = .3)+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), alpha = .7, size = 5)+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, alpha = .7)+
  scale_colour_brewer(palette = 'Spectral')+
  annotate(x = 1.25, y = 30, label = BayesFactorLabelOne, geom = 'text',size = 5,parse = T)+
  annotate(x = 1.25, y = 40, label = BayesFactorLabelTwo, geom = 'text',size = 5,parse = T)+
  lims(y = c(0,150))+
  labs(x = 'Number of Streams', y = 'Precision (ms)')

precisionPlot

ggsave(filename = 'modelOutput/18Streams/efficacyScatter.png',
       plot = efficacyPlot, 
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')

ggsave(filename = 'modelOutput/18Streams/latencyScatter.png',
       plot = latencyPlot, 
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')

ggsave(filename = 'modelOutput/18Streams/precisionScatter.png',
       plot = precisionPlot, 
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')

########################
###Plots with Density###
########################

paramsForAnalysis %<>% mutate(ID=stringID) %>%
  as.data.frame() #Convert the problem name back for plotting

if(participantPlots){
  for(thisParticipant in unique(paramsForAnalysis$ID)){
    for(thisCondition in unique(paramsForAnalysis$condition[paramsForAnalysis$ID == thisParticipant])){
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

propBeforeCue <- expand.grid(
  Participant = unique(paramsForAnalysis$stringID),
  Group = unique(paramsForAnalysis$condition),
  Proportion = -999,
  nTrialsBeforeCue = -999
)

for(thisParticipant in unique(paramsForAnalysis$stringID)){
  for(thisCondition in unique(paramsForAnalysis$condition)){

    thisNormLatency <- paramsForAnalysis %>% filter(stringID == thisParticipant & condition == thisCondition) %>% pull(latency)
    thisNormPrecision <- paramsForAnalysis %>% filter(stringID == thisParticipant & condition == thisCondition) %>% pull(precision)

    thisNormEfficacy <- paramsForAnalysis %>% filter(stringID == thisParticipant & condition == thisCondition) %>% pull(efficacy)

    thisNTrials <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject) %>% nrow

    thisProportionBeforeCue <- pnorm(0, thisNormLatency, thisNormPrecision)
    thisNEfficaciousBeforeCue <- thisNTrials * thisNormEfficacy * thisProportionBeforeCue

    propBeforeCue %<>% mutate(Proportion = replace(Proportion, Participant==thisParticipant & Group == thisCondition, thisProportionBeforeCue))

    propBeforeCue %<>% mutate(nTrialsBeforeCue = replace(nTrialsBeforeCue, Participant==thisParticipant & Group == thisCondition, thisNEfficaciousBeforeCue))
  }
}

proportionPlot <- ggplot(propBeforeCue, aes(x = factor(Group), y = Proportion))+
  geom_point(alpha = .7)+
  geom_line(aes(group = Participant), alpha = .3)+
  stat_summary(geom = 'point', fun.y = mean, size = 4, alpha = .3)+
  stat_summary(geom = 'errorbar', fun.data = mean_se, width = .1, alpha = .3)+
  labs(x = 'Number of Streams')+
  lims(y = c(0,1))

ggsave(filename = 'modelOutput/18Streams/ProportionPlot.png',
       plot = proportionPlot, 
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')


propBeforeCue %>% group_by(Group) %>% summarise(mean = mean(Proportion), sd = sd(Proportion))

propBeforeCueFullVSNull <- anovaBF(Proportion~Group+Participant,
                           whichRandom = 'Participant',
                           data = propBeforeCue)

samples <- posterior(propBeforeCueFullVSNull, iterations = nIterations)

consistent <- samples[,'Group-2']>samples[,'Group-6'] & samples[,'Group-6']== samples[,'Group-18']

propBeforeCueOrderVSFull <- (sum(consistent)/nIterations)/(1/6)
propBeforeCueOrderVSNull <- propBeforeCueOrderVSFull*as.vector(propBeforeCueFullVSNull)

#Are 6 & 18 streams equal?

#add a new label

propBeforeCue %<>% mutate(equalRestriction = ifelse(Group == '2', '2','equal'))
propBeforeCue %<>% mutate(equalRestriction = factor(equalRestriction))

PropEqualRestrictionBF <- anovaBF(Proportion~equalRestriction+Participant,
                                  whichRandom = 'Participant',
                                  data = propBeforeCue)

propBeforeCueOrderVSNull/as.vector(PropEqualRestrictionBF)

###############################################
###Example Participant Distribution for Plot###
###############################################

examplePlot <- allErrors %>% filter(ID == '18RC3', condition == '2') %>% 
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = 'dashed')


ggsave(filename = 'modelOutput/18Streams/ExampleParticipantPlot18RC32.png',
       plot = examplePlot,
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')
################################
###Aggregated Param Densities###
################################

meanTemporal <- paramsForAnalysis %>% group_by(condition) %>% summarise(precision = mean(precision),
                                                                        latency = mean(latency))


aggregateData <- data.frame(x= seq(-6,6, .05))

aggregateData %<>% mutate(two = dnorm(x*(1000/12), meanTemporal$latency[meanTemporal$condition == '2'], meanTemporal$precision[meanTemporal$condition == '2']),
                          six = dnorm(x*(1000/12), meanTemporal$latency[meanTemporal$condition == '6'], meanTemporal$precision[meanTemporal$condition == '6']),
                          eighteen = dnorm(x*(1000/12), meanTemporal$latency[meanTemporal$condition == '18'], meanTemporal$precision[meanTemporal$condition == '18']))

#aggregateData %<>% melt(id.vars = 'x', measure.vars = c('two', 'six', 'eighteen'))

aggregateDensityPlot <- ggplot(aggregateData, aes(x = x), alpha = .5)+
  geom_area(aes(y = two, fill = 'two'), stat = 'identity', alpha = .8)+
  geom_area(aes(y = six, fill = 'six'), stat = 'identity', alpha = .8)+
  geom_area(aes(y = eighteen, fill = 'eighteen'), stat = 'identity', alpha = .8)+
  scale_fill_manual(values = c('two' = '#23375f', 'six' = '#ffa951', 'eighteen' = '#bc2e48'), labels = c('Two','Six','Eighteen'), breaks  = c('two', 'six', 'eighteen'))+
  labs(x = 'SPE', y = NULL, fill = 'Number of Streams')

ggsave(filename = 'modelOutput/18Streams/densityPlot.png',
       plot = aggregateDensityPlot, 
       height = PowerPointSize[2], 
       width = PowerPointSize[1],
       units='cm')


##################
###Raw Accuracy###
##################

accuracy <- allErrors %>% filter(!fixationReject) %>% group_by(condition, ID) %>% summarise(accuracy = length(which(SPE == 0))/length(SPE))

accuracy %<>% ungroup()

accuracy %<>% mutate(ID = as.factor(ID), condition = factor(condition, levels = c(2,6,18), ordered = T))

anovaBF(accuracy~condition+ID, whichRandom = 'ID', data = accuracy)

accuracy %>% ggplot(., aes(x = condition, y = accuracy))+
  geom_point()+
  stat_summary(fun.y = mean, geom = 'point', shape = 5, size = 5)+
  stat_summary(fun.data = mean_se, geom = 'errorbar')

