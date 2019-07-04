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
library(mixRSVP)
library(data.table)

setwd('~/gitCode/nStream/')

runAnyway <- FALSE

nTrials <- 300

nStreams <- c(2,6,18)
nSimulations <- 1000

efficacy <- .75
allErrors <- read.csv('Analysis/allErrors18Streams.txt', stringsAsFactors = F)

simulatedParticipants <- allErrors %>% pull(ID) %>% unique() %>% as.character()

simulatedResponses <- function(nStreams, nTrials, nMonitoredStreams = 2, efficacy = .75){
  stimuli <- LETTERS[!LETTERS %in% c('C','W')]
  cueTemporalPos <- sample(6:10, size = nTrials, replace = T)
 
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
      
      thisCueTemporalPos <- cueTemporalPos[thisTrial]

      theseMonitoredStreams <- sample(1:thisNStream, size = nMonitoredStreams, replace = F)
      streamStimuli <- matrix(nrow = 24, ncol = thisNStream)
      
      for(stream in 1:thisNStream){
        streamStimuli[,stream] <- sample(stimuli, 24, replace = F)
      }
      
      thisBufferedTemporalPos <- sample(c(thisCueTemporalPos, thisCueTemporalPos-1), size = 1)
      
      if(nonGuess){
        if(thisCuedStream %in% theseMonitoredStreams){
          thisResponse <- streamStimuli[thisBufferedTemporalPos, thisCuedStream]
          thisSPE <- thisBufferedTemporalPos - thisCueTemporalPos
          thisSource = "Monitoring"
        } else { #assume that any responses from non-monitored streams are selected using exogenous attention because the transient gets us the location of the cue for free
          thisSPE <- rgamma( #The parameters here don't matter because what we're interested in is the pre-cue responses, which the gamma can never generate
            n = 1,
            shape = 4, 
            scale = .3
          )
          thisSPE <- round(thisSPE)
          
          while((thisSPE + thisCueTemporalPos) > 24){
            thisSPE <- rgamma(
              n = 1,
              shape = 4, 
              scale = .3
            )
            thisSPE <- round(thisSPE)
          }
          thisResponse <- streamStimuli[thisSPE+thisCueTemporalPos, thisCuedStream]
          thisSource = "Attention"
        } 
      } else {
        minSPE = 1-thisCueTemporalPos 
        maxSPE = 24 - thisCueTemporalPos
        thisSPE <- sample(minSPE:maxSPE,1)
        thisResponse <- streamStimuli[thisSPE+thisCueTemporalPos, thisCuedStream]
        thisSource = 'Guess'
      }
      
      responses %<>% mutate(SPE = replace(SPE, trial == thisTrial & nStreams == thisNStream, thisSPE),
                            source = replace(source, trial == thisTrial & nStreams == thisNStream, thisSource),
                            response = replace(response, trial == thisTrial & nStreams == thisNStream, thisResponse))
    }
  }

  return(responses)
}


if(!file.exists('Analysis/MonitoringModelling.csv') | runAnyway){
  allResponsesHeader <- data.frame( #Dummy for writing header to CSV, we'll write the csv every simulation loop because the very large DF slows the loop down as it iterates (not sure why)
    monitoredStreams = double(0),
    simulation = integer(0),
    trial = integer(0), 
    nStreams = double(0), 
    SPE = integer(0), 
    source = character(0),
    response = character(0),
    stringsAsFactors = F
  )
  
  write.table(x = allResponsesHeader, file = 'Analysis/MonitoringModelling.csv', col.names = TRUE, sep = ',') #write header to CSV
  
  
  
  for(thisNStream in nStreams){
    for(thisMonitoredStreams in 1:4){
      if(thisMonitoredStreams>thisNStream){
        paste0('passing ', rep(' ', times = 60), '\r') %>% cat()
        next
      }
      for(thisSimulation in 1:nSimulations){
        paste0('nStream = ', thisNStream, ' Simulation = ', thisSimulation, ' nMonitored = ', thisMonitoredStreams,'                               \r') %>% cat 
        thisEfficacy <- efficacy 
        thisNTrial <- 200
        
        thisDistribution <- simulatedResponses(thisNStream,thisNTrial, thisMonitoredStreams, efficacy = thisEfficacy)
        
        thisDistribution %<>% mutate(
          monitoredStreams = thisMonitoredStreams,
          simulation = thisSimulation
        )
        
        thisDistribution %<>% select(monitoredStreams,
                                     simulation,
                                     trial,
                                     nStreams,
                                     SPE,
                                     source,
                                     response)
        
        write.table(x = thisDistribution, file = 'Analysis/MonitoringModelling.csv', append = T, sep = ',', col.names = F, row.names = F)
      }
    }
  }
  
}

allResponses <- fread('Analysis/MonitoringModelling.csv', sep = ',', header = T, stringsAsFactors = F)

proportionNegModel <- allResponses %>% group_by(nStreams, simulation, monitoredStreams) %>% summarise(pNeg = sum(SPE<0)/n())
proportionNegEmpirical <- allErrors %>% filter(!fixationReject) %>% group_by(condition, ID) %>% summarise(pNeg = sum(SPE<0)/n())

proportionModelDifference <- proportionNegModel %>% dcast(simulation+monitoredStreams~nStreams, value.var = 'pNeg')
colnames(proportionModelDifference) <- c('simulation', 'monitoredStreams', 'two','six','eighteen')

proportionModelDifference %<>% mutate('twoToSix' = two - six, 'sixToEighteen' = six - eighteen)

proportionEmpiricalDifference <- proportionNegEmpirical %>% dcast(ID~condition, value.var = 'pNeg')
colnames(proportionEmpiricalDifference) <- c('ID', 'two','six','eighteen')

proportionEmpiricalDifference %<>% mutate('twoToSix' = two - six, 'sixToEighteen' = six - eighteen)

ggplot()+
  geom_point(data = proportionNegModel, aes(x = factor(nStreams), y = pNeg, shape = 'Model', colour = factor(monitoredStreams)),alpha = .5)+
  geom_point(data = proportionNegEmpirical, aes(x = factor(condition), y = pNeg, shape = 'Empirical'),alpha = .5, size = 4)+
  #stat_summary(fun.y = mean, geom = 'point', shape = 18, size = 5)+
  scale_shape_manual(values = c("Model" = 17, "Empirical" = 5))



proportionModelDifference %<>% melt(id.vars = c('participant', 'monitoredStreams'), measure.vars = c('twoToSix', 'sixToEighteen'))
proportionEmpiricalDifference %<>% melt(id.vars = 'ID', measure.vars = c('twoToSix', 'sixToEighteen'))

ggplot()+
  geom_point(data = proportionModelDifference, aes(x = variable, y = value, shape = 'Model', colour = factor(monitoredStreams)),alpha = .5)+
  geom_point(data = proportionEmpiricalDifference, aes(x = variable, y = value, shape = 'Empirical'),alpha = .5, size = 4)+
  #stat_summary(fun.y = mean, geom = 'point', shape = 18, size = 5)+
  scale_shape_manual(values = c("Model" = 17, "Empirical" = 5))

################################
###Two streams, One monitored###
################################
x = proportionNegModel %>% filter(nStreams == 2, monitoredStreams == 1) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 2) %>% pull(pNeg)

TwoOne <- ttestBF(x = x,
        y = y)


################################
###Two streams, Two monitored###
################################
x = proportionNegModel %>% filter(nStreams == 2, monitoredStreams == 2) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 2) %>% pull(pNeg)

TwoTwo <- ttestBF(x = x,
        y = y)

################################
###Six streams, One monitored###
################################
x = proportionNegModel %>% filter(nStreams == 6, monitoredStreams == 1) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 6) %>% pull(pNeg)

SixOne <- ttestBF(x = x,
        y = y)

################################
###Six streams, Two monitored###
################################
x = proportionNegModel %>% filter(nStreams == 6, monitoredStreams == 2) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 6) %>% pull(pNeg)

SixTwo <- ttestBF(x = x,
        y = y)

##################################
###Six streams, Three monitored###
##################################
x = proportionNegModel %>% filter(nStreams == 6, monitoredStreams == 3) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 6) %>% pull(pNeg)

SixThree <- ttestBF(x = x,
                  y = y)

##################################
###Six streams, Four monitored###
##################################
x = proportionNegModel %>% filter(nStreams == 6, monitoredStreams == 4) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 6) %>% pull(pNeg)

SixFour <- ttestBF(x = x,
                    y = y)

################################
###18 streams, One monitored####
################################
x = proportionNegModel %>% filter(nStreams == 18, monitoredStreams == 1) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 18) %>% pull(pNeg)

EighteenOne <- ttestBF(x = x,
        y = y)


################################
###18 streams, Two monitored####
################################
x = proportionNegModel %>% filter(nStreams == 18, monitoredStreams == 2) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 18) %>% pull(pNeg)

EighteenTwo <- ttestBF(x = x,
        y = y)

##################################
###18 streams, Three monitored####
##################################
x = proportionNegModel %>% filter(nStreams == 18, monitoredStreams == 3) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 18) %>% pull(pNeg)

EighteenThree <- ttestBF(x = x,
                       y = y)

#################################
###18 streams, Four monitored####
#################################
x = proportionNegModel %>% filter(nStreams == 18, monitoredStreams == 4) %>% pull(pNeg)
y = proportionNegEmpirical %>% filter(condition == 18) %>% pull(pNeg)

EighteenFour <- ttestBF(x = x,
                         y = y)


BFs <- expand.grid(
  nStreams = c(2,6,18),
  monitoredStreams = 1:4,
  BF = -999
)

BFs %<>% 
  mutate(BF = replace(BF, nStreams == 2 & monitoredStreams == 1, TwoOne)) %>%
  mutate(BF = replace(BF, nStreams == 2 & monitoredStreams == 2, TwoTwo)) %>%
  mutate(BF = replace(BF, nStreams == 6 & monitoredStreams == 1, SixOne)) %>%
  mutate(BF = replace(BF, nStreams == 6 & monitoredStreams == 2, SixTwo)) %>%
  mutate(BF = replace(BF, nStreams == 6 & monitoredStreams == 3, SixThree)) %>%
  mutate(BF = replace(BF, nStreams == 6 & monitoredStreams == 4, SixFour)) %>%
  mutate(BF = replace(BF, nStreams == 18 & monitoredStreams == 1, EighteenOne)) %>%
  mutate(BF = replace(BF, nStreams == 18 & monitoredStreams == 2, EighteenTwo)) %>%
  mutate(BF = replace(BF, nStreams == 18 & monitoredStreams == 3, EighteenThree)) %>%
  mutate(BF = replace(BF, nStreams == 18 & monitoredStreams == 4, EighteenFour)) %>%
  mutate(BFLabel = ifelse(BF != -999, paste0('BF10 =', format(BF, scientific = T, digits = 2)),'' ))

proportionNegEmpirical %<>% mutate(nStreams = condition)

proportionNegEmpiricalMeans <- proportionNegEmpirical %>% group_by(nStreams) %>% summarise(mean = mean(pNeg))
proportionNegModelMeans <- proportionNegModel %>% group_by(nStreams, monitoredStreams) %>% summarise(mean = mean(pNeg))

BFPlot <- ggplot()+
  geom_histogram(data = proportionNegModel, aes(x = pNeg), alpha = .5, fill = 'blue')+
  geom_point(data = proportionNegEmpirical, aes(x = pNeg, y = 350),alpha = .5, size = 4, colour = 'red')+
  geom_text(data = BFs, aes(label = BFLabel), x = .4, y = 300)+
  geom_vline(data = proportionNegEmpiricalMeans, aes(xintercept = mean), linetype = 'dashed', colour = 'red')+
  geom_vline(data = proportionNegModelMeans, aes(xintercept = mean), linetype = 'dashed', colour = 'blue')+
  facet_grid(cols = vars(monitoredStreams), rows = vars(nStreams),labeller = 'label_both')

ggsave(filename = 'Analysis/MonitoringPlot.png', plot = BFPlot, width = 16, height = 9, units = 'in') 
  
  
  