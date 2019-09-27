library(ggplot2)
library(data.table)
library(dplyr)
library(magrittr)
devtools::load_all('../mixRSVP/')
rm(list=ls())

setwd('~/gitCode/nStream/Analysis/Model recovery/')


simulatedResponses <- function(nTrials, efficacy = .75, p2 = .8, p3 = .8, modelKind = 'Normal'){
  stimuli <- LETTERS[!LETTERS %in% c('C','W')]
  cueTemporalPos <- sample(6:10, size = nTrials, replace = T)
  
  responses <- expand.grid(trial = 1:nTrials, 
                           SPE = -999, 
                           targetSP = -999,
                           source = character(1),
                           response = character(1),
                           stringsAsFactors = F)
  
  efficaciousTrials <- rbinom(n = nTrials, size = 1, prob = efficacy)
  
  for(thisTrial in 1:nTrials){
    
    efficacious <- efficaciousTrials[thisTrial]
    
    thisCueTemporalPos <- cueTemporalPos[thisTrial]
    
    streamStimuli <- sample(stimuli, 24, replace = F)
    
    if(efficacious){
      #assume that any responses from non-monitored streams are selected using exogenous attention because the transient gets us the location of the cue for free
      if(modelKind == 'Gamma'){
        thisSPE <- rgamma( 
          n = 1,
          shape = p2, 
          scale = p3
        )
        thisSPE <- round(thisSPE)
        
        while((thisSPE + thisCueTemporalPos) > 24){
          thisSPE <- rgamma(
            n = 1,
            shape = p2, 
            scale = p3
          )
          thisSPE <- round(thisSPE)
        }
      } else if(modelKind == 'Normal'){
        thisSPE <- rnorm( 
          n = 1,
          mean = p2, 
          sd = p3
        )
        thisSPE <- round(thisSPE)
        
        while((thisSPE + thisCueTemporalPos) > 24){
          thisSPE <- rnorm(
            n = 1,
            mean = p2, 
            sd = p3
          )
          thisSPE <- round(thisSPE)
        }
      }
      thisResponse <- streamStimuli[thisSPE+thisCueTemporalPos]
      thisSource = "Attention"
      
    } else {
      minSPE = 1-thisCueTemporalPos 
      maxSPE = 24 - thisCueTemporalPos
      thisSPE <- sample(minSPE:maxSPE,1)
      thisResponse <- streamStimuli[thisSPE+thisCueTemporalPos]
      thisSource = 'Guess'
    }
    
    responses %<>% mutate(SPE = replace(SPE, trial == thisTrial, thisSPE),
                          targetSP = replace(targetSP, trial == thisTrial, thisCueTemporalPos),
                          source = replace(source, trial == thisTrial, thisSource),
                          response = replace(response, trial == thisTrial, thisResponse))
  }
  
  return(responses)
}


runAnyway <- FALSE #Run the simulations regardless of whether saved parameters exist

timeStamp <- strftime(Sys.time(),format = '%d-%m-%Y_%H-%M')

paramFiles <- list.files(pattern = 'modelRecoveryMixRSVPParams[^Other]')
simulationFiles <- list.files(pattern = 'modelRecoveryMixRSVPSimulationData')



latencies <- c(1.5,2)

efficacies <- c(.7,.9)

precisions <- c(70,100)/(1000/12)
models <- c('Normal', 'Gamma')

participants = 1:50

nTrials <- 200

nReps <- 20

params <- expand.grid(
  model = models, 
  latency=latencies, 
  efficacy =efficacies, 
  precision =precisions, 
  participant = participants, 
  efficacyEstimate = -999, 
  latencyEstimate = -999, 
  precisionEstimate = -999, 
  val = -999,
  pLRTest = -999
  )

trials <- matrix(nrow = nrow(params), ncol = nTrials,data = -999)

rawData <- cbind(params,trials,trials)
rawData <- as.data.table(rawData)
colnames(rawData)[11:210] <- paste0('SPE', 1:nTrials)
colnames(rawData)[211:410] <- paste0('targetSP', 1:nTrials)


if(length(paramFiles)==0 | runAnyway){ #If we haven't already done the simulations
  for(thisEfficacy in efficacies){
    for(thisLatency in latencies){
      for(thisPrecision in precisions){
        for(thisParticipant in participants){
          for(thisModel in models){
            cat(paste0('Model: ', thisModel, ', Participant: ', thisParticipant,', Latency: ', thisLatency, ', Precision: ', thisPrecision, ', Efficacy: ', thisEfficacy, rep(' ', times = 50), '\r'))
            
            thisRow <- (
              params$model == thisModel &
              params$latency == thisLatency & 
              params$efficacy == thisEfficacy &
              params$precision == thisPrecision &
              params$participant == thisParticipant
              )
            
            if(thisModel == 'Gamma'){
              shape = thisLatency^2/thisPrecision^2
              scale = thisPrecision^2/thisLatency
              
              p2 = shape
              p3 = scale
            } else {
              p2 = thisLatency
              p3 = thisPrecision
            }
            
            
            theseErrors <- simulatedResponses(200, thisEfficacy, p2, p3, thisModel)
            
            rawData[
              model == thisModel &
              latency == thisLatency & 
              efficacy == thisEfficacy &
              precision == thisPrecision &
              participant == thisParticipant, 11:210 := as.list(theseErrors$SPE)
              ] #Put simulated data in the rawData object
            
            rawData[
              model == thisModel &
                latency == thisLatency & 
                efficacy == thisEfficacy &
                precision == thisPrecision &
                participant == thisParticipant, 211:410 := as.list(theseErrors$targetSP)
              ] #Put simulated data in the rawData object
            
            
            invisible( #invisible combined with capture output hides those print 'error' statements hidden deep in mixRSVP::
              capture.output(
                theseParams <- analyzeOneCondition(theseErrors, 24, parameterBounds(thisModel), nReps,modelKind = thisModel) #fit the data
                )
              )
            
            params$latencyEstimate[thisRow] <- theseParams$latency
            params$efficacyEstimate[thisRow] <- theseParams$efficacy
            params$precisionEstimate[thisRow] <- theseParams$precision
            params$val[thisRow] <- theseParams$val
            params$pLRTest[thisRow] <- theseParams$pLRtest
          }  
        }
      }
    }
  }
  paramFileName <- paste0('modelRecoveryMixRSVPParams_', timeStamp,'.csv')
  
  write.csv(x = params, #write the params and raw data
            file = paramFileName)

  simulationFileName <- paste0('modelRecoveryMixRSVPSimulationData_', timeStamp,'.csv')
  write.csv(rawData,
            simulationFileName)
} else{
  
  fileTimes <- gsub(pattern = 'modelRecoveryMixRSVPParams_|.csv', replacement = '',x =paramFiles)
  fileTimes <- as.POSIXct(fileTimes, format= '%d-%m-%Y_%H-%M')
  mostRecentParams <- paramFiles[fileTimes == max(fileTimes)]
  mostRecentSims <- simulationFiles[fileTimes == max(fileTimes)]

  params <- read.csv(mostRecentParams) #if we've already run the simulations, read in the saved data
  rawData <- read.csv(mostRecentSims)
}


params %<>% mutate( #Convert parameter estimates to mean and SD for the gamma distribution
  scale = ifelse(model == 'Gamma', precisionEstimate,NA),
  shape = ifelse(model == 'Gamma', latencyEstimate, NA),
  latencyEstimate = ifelse(model == 'Gamma', shape*scale, latencyEstimate),
  precisionEstimate = ifelse(model == 'Gamma', sqrt(shape)*scale, precisionEstimate),
  generativeModel = model
)

#Get timing for the mismatched model fits
paramsWrongModelFiles <- list.files(pattern='modelRecoveryMixRSVPParamsOtherModel')
paramsWrongModelTimes <- gsub(x = paramsWrongModelFiles, pattern = 'modelRecoveryMixRSVPParamsOtherModel_|\\.csv', replacement = '')
paramsWrongModelTimes <- as.POSIXct(paramsWrongModelTimes, format= '%d-%m-%Y_%H-%M')

mostRecentWrongModel <- paramsWrongModelFiles[paramsWrongModelTimes == max(paramsWrongModelTimes)]

paramsWrongModel <- read.csv(mostRecentWrongModel)#Read in missmatched data generation and model simulation parameter estimates

paramsWrongModel %<>% mutate(
  scale = ifelse(model == 'Gamma', precisionEstimate,NA),
  shape = ifelse(model == 'Gamma', latencyEstimate, NA),
  latencyEstimate = ifelse(model == 'Gamma', shape*scale, latencyEstimate),
  precisionEstimate = ifelse(model == 'Gamma', sqrt(shape)*scale, precisionEstimate)
)

#Bind them all together
allParams <- rbind(paramsWrongModel, params)

#Calculate BFs
allParamsWide <- allParams %>% 
  dcast(participant+latency+precision+efficacy+generativeModel~model, value.var = 'val') %>%
  mutate(BFGenModel = ifelse(generativeModel == 'Gamma', exp(-Gamma+Normal), exp(-Normal+Gamma))) %>%
  mutate(recoveredRightModel = BFGenModel > 3) %>%
  mutate(recoveredWrongModel = BFGenModel < .33) %>%
  mutate(recoveredNeitherModel = BFGenModel >.33 & BFGenModel < 3)

allParamsWide %>% 
  group_by(generativeModel) %>% 
  summarise(
    CorrectModel = sum(recoveredRightModel)/n(), 
    WrongModel = sum(recoveredWrongModel)/n(), 
    NeitherModel = sum(recoveredNeitherModel)/n(),
    n = n()
    ) %>% 


allParams %>% 
  mutate(latencyError = latencyEstimate - latency) %>% 
  group_by(generativeModel, model) %>%
  summarise(mean = mean(latencyError), sd = sd(latencyError), CILow = mean - 1.96*(sd/sqrt(n())),CIHigh = mean + 1.96*(sd/sqrt(n())))

latencyPlot <- ggplot(allParams, aes(x=latency, y=latencyEstimate))+
  geom_jitter(colour = '#ef5e39', width = .005, alpha = .6)+
  stat_summary(fun.y = mean, 
    geom='point', 
    size = 4, 
    colour = '#dca951')+
  stat_summary(fun.data= mean_se,
    geom='errorbar', 
    width = .05, 
    colour = '#dca951')+
  theme_apa(base_size = 15)+
  scale_x_continuous(breaks = unique(params$latency))+
  labs(y = 'Latency Estimate', x = 'True Latency')+
  geom_hline(yintercept = unique(params$latency), linetype = 'dashed')+
  facet_wrap(~model+generativeModel, labeller = 'label_both')+
  facet_grid(cols = vars(generativeModel), rows = vars(model), labeller = 'label_both')+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)


ggsave('Plots/Gamma R/latency.png', latencyPlot, height = 9, width = 9, units = 'in')

allParams %>% 
  mutate(efficacyError = efficacyEstimate - efficacy) %>% 
  group_by(generativeModel, model) %>%
  summarise(mean = mean(efficacyError), sd = sd(efficacyError), se = sd/sqrt(n()), CILow = mean - 1.96*se,CIHigh = mean + 1.96*se)


efficacyPlot <- ggplot(allParams, aes(x=efficacy, y=efficacyEstimate))+
  geom_jitter(colour = '#ef5e39', width = .005, alpha = .6)+
  stat_summary(fun.y = mean, 
               geom='point', 
               size = 4, 
               colour = '#dca951')+
  stat_summary(fun.data= mean_se,
               geom='errorbar', 
               width = .05, 
               colour = '#dca951')+
  theme_apa(base_size = 15)+
  scale_x_continuous(breaks = unique(params$efficacy))+
  geom_hline(yintercept = unique(params$efficacy), linetype = 'dashed')+
  facet_grid(cols = vars(generativeModel), rows = vars(model), labeller = 'label_both')+
  labs(y = 'Efficacy Estimate', x = 'True Efficacy')+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)


ggsave('Plots/Gamma R/efficacy.png', efficacyPlot, height = 9, width = 9, units = 'in')


allParams %>% 
  mutate(precisionError = precisionEstimate - precision) %>% 
  group_by(generativeModel, model) %>%
  summarise(mean = mean(precisionError), sd = sd(precisionError), CILow = mean - 1.96*(sd/sqrt(n())),CIHigh = mean + 1.96*(sd/sqrt(n())))


precisionPlot <- ggplot(allParams, aes(x=precision, y=precisionEstimate))+
  geom_jitter(colour = '#ef5e39', width = .005, alpha = .6)+
  stat_summary(fun.y = mean, 
               geom='point', 
               size = 4, 
               colour = '#dca951')+
  stat_summary(fun.data= mean_se,
               geom='errorbar', 
               width = .05, 
               colour = '#dca951')+
  theme_apa(base_size = 15)+
  scale_x_continuous(breaks = unique(params$precision))+
  scale_y_continuous(breaks = unique(params$precision))+
  geom_hline(yintercept = unique(params$precision), linetype = 'dashed')+
  facet_grid(cols = vars(generativeModel), rows = vars(model), labeller = 'label_both')+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)

ggsave('Plots/Gamma R/precision.png', precisionPlot, height = 9, width = 9, units = 'in')
