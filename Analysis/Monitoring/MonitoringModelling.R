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

nTrials <- 200

nStreams <- c(2,6,18)
nSimulations <- 10000

efficacy <- .75
allErrors <- fread('Analysis/allErrors18Streams.txt', stringsAsFactors = F)

simulatedParticipants <- allErrors[,ID]

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

monitoringFiles <- list.files(path = 'Analysis/Monitoring/',full.names = T, pattern = 'csv')


if(length(monitoringFiles) == 0 | runAnyway){
  allSimulationsHeader <- data.frame( #Dummy for writing header to CSV, we'll write to the csv every simulation loop and clear the trials from memory because the very large DF slows the loop down as it iterates (not sure why)
    monitoredStreams = double(0),
    simulation = integer(0),
    trial = integer(0), 
    nStreams = double(0), 
    SPE = integer(0), 
    source = character(0),
    response = character(0),
    stringsAsFactors = F
  )
  
  write.table(x = allSimulationsHeader, file = 'Analysis/Monitoring/MonitoringModelling.csv', col.names = TRUE, sep = ',') #write header to CSV
  
  
  
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
        
        write.table(x = thisDistribution, file = 'Analysis/Monitoring/MonitoringModelling.csv', append = T, sep = ',', col.names = F, row.names = F)
      }
    }
  }
  allSimulations <- fread('Analysis/Monitoring/MonitoringModelling.csv', sep = ',', header = T, stringsAsFactors = F)
  saveRows <- seq(1, nrow(allSimulations), 2500000)
  for(i in saveRows){ #github has a 100mb limit so we need to save the csvs in parts. 
    index <- which(saveRows==i)
    theseRows <- allSimulations[i:(i+2499999),]
    write.csv(x = theseRows, file = paste0('Analysis/Monitoring/MonitoringModelling', index, '.csv'),row.names = F)
  }
} else { #Loads  csvs and bind them
  allSimulations <- rbindlist(lapply(monitoringFiles, fread))
}



proportionNegModel <- allSimulations[,.(pNeg = sum(SPE<0)/.N), by = .(nStreams, monitoredStreams, simulation)]

proportionNegEmpirical <- allErrors[fixationReject == FALSE,.(pNeg = sum(SPE<0)/.N), by = .(condition, ID)]


################################
###Two streams, One monitored###
################################
x = proportionNegModel[nStreams == 2 & monitoredStreams == 1, pNeg]
y = proportionNegEmpirical[condition == 2, pNeg]

TwoOne <- ttestBF(x = x,
        y = y)


################################
###Two streams, Two monitored###
################################
x = proportionNegModel[nStreams == 2 & monitoredStreams == 2, pNeg]
y = proportionNegEmpirical[condition == 2, pNeg]

TwoTwo <- ttestBF(x = x,
        y = y)

################################
###Six streams, One monitored###
################################
x = proportionNegModel[nStreams == 6 &monitoredStreams == 1, pNeg]
y = proportionNegEmpirical[condition == 6, pNeg]

SixOne <- ttestBF(x = x,
        y = y)

################################
###Six streams, Two monitored###
################################
x = proportionNegModel[nStreams == 6 & monitoredStreams == 2, pNeg]
y = proportionNegEmpirical[condition == 6, pNeg]

SixTwo <- ttestBF(x = x,
        y = y)

##################################
###Six streams, Three monitored###
##################################
x = proportionNegModel[nStreams == 6 & monitoredStreams == 3, pNeg]
y = proportionNegEmpirical[condition == 6, pNeg]

SixThree <- ttestBF(x = x,
                  y = y)

##################################
###Six streams, Four monitored###
##################################
x = proportionNegModel[nStreams == 6 & monitoredStreams == 4, pNeg]
y = proportionNegEmpirical[condition == 6, pNeg]

SixFour <- ttestBF(x = x,
                    y = y)

################################
###18 streams, One monitored####
################################
x = proportionNegModel[nStreams == 18 & monitoredStreams == 1, pNeg]
y = proportionNegEmpirical[condition == 18, pNeg]

EighteenOne <- ttestBF(x = x,
        y = y)


################################
###18 streams, Two monitored####
################################
x = proportionNegModel[nStreams == 18 & monitoredStreams == 2, pNeg]
y = proportionNegEmpirical[condition == 18, pNeg]

EighteenTwo <- ttestBF(x = x,
        y = y)

##################################
###18 streams, Three monitored####
##################################
x = proportionNegModel[nStreams == 18 & monitoredStreams == 3, pNeg]
y = proportionNegEmpirical[condition == 18, pNeg]

EighteenThree <- ttestBF(x = x,
                       y = y)

#################################
###18 streams, Four monitored####
#################################
x = proportionNegModel[nStreams == 18 & monitoredStreams == 4, pNeg]
y = proportionNegEmpirical[condition == 18, pNeg]

EighteenFour <- ttestBF(x = x,
                         y = y)


BFs <- CJ(
  nStreams = c(2,6,18),
  monitoredStreams = 1:4,
  BF = -999
)


BFs[nStreams == 2 & monitoredStreams == 1, BF:=as.vector(TwoOne)]
BFs[nStreams == 2 & monitoredStreams == 2, BF:= as.vector(TwoTwo)] 
BFs[nStreams == 6 & monitoredStreams == 1, BF:= as.vector(SixOne)]
BFs[nStreams == 6 & monitoredStreams == 2, BF:= as.vector(SixTwo)]
BFs[nStreams == 6 & monitoredStreams == 3, BF:= as.vector(SixThree)]
BFs[nStreams == 6 & monitoredStreams == 4, BF:= as.vector(SixFour)]
BFs[nStreams == 18 & monitoredStreams == 1, BF:= as.vector(EighteenOne)]
BFs[nStreams == 18 & monitoredStreams == 2, BF:= as.vector(EighteenTwo)]
BFs[nStreams == 18 & monitoredStreams == 3, BF:= as.vector(EighteenThree)]
BFs[nStreams == 18 & monitoredStreams == 4, BF:= as.vector(EighteenFour)]
BFs[BF!=-999, BFLabel := paste0('BF[10] ==', format(BF, scientific = T, digits = 2))]
BFs[BF==-999, BFLabel := NA]

proportionNegEmpirical[,nStreams := condition]

proportionNegEmpiricalMeans <- proportionNegEmpirical[,.(mean = mean(pNeg)), .(nStreams)]
proportionNegModelMeans <- proportionNegModel[,.(mean = mean(pNeg)),.(nStreams, monitoredStreams)]

BFPlot <- ggplot()+
  geom_histogram(data = proportionNegModel, aes(x = pNeg), alpha = .5, fill = 'blue')+
  geom_point(data = proportionNegEmpirical, aes(x = pNeg, y = 3500),alpha = .5, size = 4, colour = 'red')+
  geom_text(data = BFs, aes(label = BFLabel), x = .4, y = 3000,parse = T)+
  geom_vline(data = proportionNegEmpiricalMeans, aes(xintercept = mean), linetype = 'dashed', colour = 'red')+
  geom_vline(data = proportionNegModelMeans, aes(xintercept = mean), linetype = 'dashed', colour = 'blue')+
  facet_grid(cols = vars(monitoredStreams), rows = vars(nStreams),labeller = 'label_both')

BFPlot

ggsave(filename = 'Analysis/Monitoring/MonitoringPlot.png', plot = BFPlot, width = 16, height = 9, units = 'in') 
  

randSims <- sample(1:nSimulations,size = 10)

params <- CJ(
  sim = randSims, 
  monitoredStreams = 1:4,
  nStreams = c(2,6,18),
  efficacy = -999,
  latency = -999,
  precision = -999,
  val = -999,
  valGuessing = -999,
  
)  

for(sim in randSims){
  for(thisNStream in c(2,6,18)){
    for(thisMonitoredStreams in 1:4){
      theseData <- allSimulations[nStream == nStream & monitoredStreams == thisMonitoredStreams & simulation == sim,]
    }
  }
}