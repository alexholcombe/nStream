###########################################################################################
##What is the probability of pre-cue responses if you're only monitoring K of N streams?###
###########################################################################################
rm(list=ls())
library(truncnorm)
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(BayesFactor)

setwd('~/gitCode/nStream/')

nTrials <- 300

nStreams <- c(2,6,18)
nSimulations <- 500

efficacy <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_EfficacyNorm.csv')
allErrors <- read.csv('Analysis/allErrors18Streams.txt', stringsAsFactors = F)

simulatedParticipants <- efficacy %>% pull(Participant) %>% unique() %>% as.character()

simulatedResponses <- function(nStreams, nTrials, nMonitoredStreams = 2, efficacy = .75){
  stimuli <- LETTERS[!LETTERS %in% c('C','W')]
  cueTemporalPos <- 6:10
 
  responses <- expand.grid(trial = 1:nTrials, 
                           nStreams = nStreams, 
                           SPE = -999, 
                           source = character(1),
                           response = character(1),
                           stringsAsFactors = F)
  
  for(thisNStream in nStreams){
    for(thisTrial in 1:nTrials){
      if(nMonitoredStreams > thisNStream){
        nMonitoredStreams = thisNStream
      }
      
      nonGuess <- rbinom(n = 1, size = 1, prob = efficacy)
      
      thisCuedStream <- sample(1:thisNStream, size = 1)
      
      thisCueTemporalPos <- sample(cueTemporalPos, 1)
      
      
      theseMonitoredStreams <- sample(1:thisNStream, size = nMonitoredStreams, replace = F)
      streamStimuli <- matrix(nrow = 24, ncol = thisNStream)
      for(stream in 1:thisNStream){
        streamStimuli[,stream] <- sample(stimuli, 24, replace = F)
      }
      thisBufferedTemporalPos <- sample(c(thisCueTemporalPos, thisCueTemporalPos-1), size = 1)
      
      if(nonGuess){
        if(thisCuedStream %in% theseMonitoredStreams & rbinom(n = 1, size = 1, prob = .8)){
          thisResponse <- streamStimuli[thisBufferedTemporalPos, thisCuedStream]
          thisSPE <- thisBufferedTemporalPos - thisCueTemporalPos
          thisSource = "Monitoring"
        } else { #assume that any responses from non-monitored streams are selected using exogenous attention because the transient gets us the location of the cue for free
          thisSPE <- rtruncnorm(n = 1,
                                a = 0, 
                                b = 24 - thisCueTemporalPos,
                                mean = 1,
                                sd = 1)
          thisSPE <- round(thisSPE)
          thisResponse <- streamStimuli[thisSPE+thisCueTemporalPos, thisCuedStream]
          thisSource = "Attention"
        } 
      } else {
        thisResponse <- sample(stimuli, size = 1)
        thisSPE <- which(streamStimuli[, thisCuedStream] == thisResponse) - thisCueTemporalPos
        thisSource = 'Guess'
      }
      
      responses %<>% mutate(SPE = replace(SPE, trial == thisTrial & nStreams == thisNStream, thisSPE),
                            source = replace(source, trial == thisTrial & nStreams == thisNStream, thisSource),
                            response = replace(response, trial == thisTrial & nStreams == thisNStream, thisResponse))
    }
  }

  return(responses)
}

allResponses <- expand.grid(
  participant = character(1),
  monitoredStreams = numeric(1),
  simulation = numeric(1),
  trial = numeric(1), 
  nStreams = numeric(1), 
  SPE = -999, 
  source = character(1),
  response = character(2e4),
  stringsAsFactors = F
  )

startRow <- 1
for(thisMonitoredStreams in c(1,2)){
  for(thisSimulation in 1:nSimulations){
    for(thisParticipant in simulatedParticipants){
      for(thisNStream in nStreams){
        paste0('Participant = ', thisParticipant, ' nStream = ', thisNStream, ' Simulation = ', thisSimulation, ' nMonitored = ', thisMonitoredStreams,'                               \r') %>% cat 
        thisEfficacy <- efficacy %>% filter(Participant == thisParticipant & Group == thisNStream) %>% pull(Estimate)
        thisNTrial <- allErrors %>% filter(condition == thisNStream, ID == thisParticipant, !fixationReject) %>% nrow()
        
        thisDistribution <- simulatedResponses(thisNStream,thisNTrial, thisMonitoredStreams ,efficacy = thisEfficacy)
        
        thisDistribution %<>% mutate(participant = thisParticipant,
                                     monitoredStreams = thisMonitoredStreams,
                                     simulation = thisSimulation)
        
        thisDistribution %<>% select(participant,
                                     monitoredStreams,
                                     simulation,
                                     trial,
                                     nStreams,
                                     SPE,
                                     source,
                                     response)
        
        endRow <- startRow + nrow(thisDistribution) -1
        allResponses[startRow:endRow,] <- thisDistribution
        startRow <- endRow+1
      }
    }
  }
}

allResponses %<>% filter(SPE > -999) 

proportionNegModel <- allResponses %>% group_by(nStreams, participant, simulation, monitoredStreams) %>% summarise(pNeg = sum(SPE<0)/n())
proportionNegEmpirical <- allErrors %>% filter(!fixationReject) %>% group_by(condition, ID) %>% summarise(pNeg = sum(SPE<0)/n())

proportionModelDifference <- proportionNegModel %>% dcast(participant+simulation+monitoredStreams~nStreams, value.var = 'pNeg')
colnames(proportionModelDifference) <- c('participant', 'simulation', 'monitoredStreams', 'two','six','eighteen')

proportionModelDifference %<>% mutate('twoToSix' = two - six, 'sixToEighteen' = six - eighteen)

proportionEmpiricalDifference <- proportionNegEmpirical %>% dcast(ID~condition, value.var = 'pNeg')
colnames(proportionEmpiricalDifference) <- c('ID', 'two','six','eighteen')

proportionEmpiricalDifference %<>% mutate('twoToSix' = two - six, 'sixToEighteen' = six - eighteen)

ggplot()+
  geom_point(data = proportionNegModel, aes(x = factor(nStreams), y = pNeg, colour = 'Model', shape = factor(monitoredStreams)),alpha = .5)+
  geom_point(data = proportionNegEmpirical, aes(x = factor(condition), y = pNeg, colour = 'Empirical'),alpha = .5)+
  stat_summary(fun.y = mean, geom = 'point', shape = 18, size = 5)+
  scale_colour_manual(values = c("Model" = 'red', "Empirical" = 'green'))


proportionModelDifference %<>% melt(id.vars = c('participant', 'monitoredStreams'), measure.vars = c('twoToSix', 'sixToEighteen'))
proportionEmpiricalDifference %<>% melt(id.vars = 'ID', measure.vars = c('twoToSix', 'sixToEighteen'))

ggplot()+
  geom_point(data = proportionModelDifference, aes(x = variable, y = value, colour = 'Model', shape = factor(monitoredStreams)),alpha = .5)+
  geom_point(data = proportionEmpiricalDifference, aes(x = variable, y = value, colour = 'Empirical'),alpha = .5)+
  stat_summary(fun.y = mean, geom = 'point', shape = 18, size = 5)+
  scale_colour_manual(values = c("Model" = 'red', "Empirical" = 'green'))

##############################
###2-6 One Stream monitored###
##############################
x = proportionModelDifference %>% filter(variable == 'twoToSix', monitoredStreams == 1, !is.na(value)) %>% pull(value)
y = proportionEmpiricalDifference %>% filter(variable == 'twoToSix') %>% pull(value)

ttestBF(x = x,
        y = y)

###############################
###2-6 Two Streams monitored###
###############################
x = proportionModelDifference %>% filter(variable == 'twoToSix', monitoredStreams == 2) %>% pull(value)
y = proportionEmpiricalDifference %>% filter(variable == 'twoToSix') %>% pull(value)

ttestBF(x = x,
        y = y)

###############################
###6-18 One Stream monitored###
###############################
x = proportionModelDifference %>% filter(variable == 'sixToEighteen', monitoredStreams == 1) %>% pull(value)
y = proportionEmpiricalDifference %>% filter(variable == 'sixToEighteen') %>% pull(value)

ttestBF(x = x,
        y = y)

################################
###6-18 Two Streams monitored###
################################
x = proportionModelDifference %>% filter(variable == 'sixToEighteen', monitoredStreams == 2) %>% pull(value)
y = proportionEmpiricalDifference %>% filter(variable == 'sixToEighteen') %>% pull(value)

ttestBF(x = x,
        y = y)
