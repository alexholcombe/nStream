library(ggplot2)
library(dplyr)
library(magrittr)
devtools::load_all('../mixRSVP/')
library(papaja)


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


test <- simulatedResponses(1000)

minSPE <- 1- max(test$targetSP)
maxSPE <- 24 - min(test$targetSP)


guessingDist <- createGuessingDistribution(minSPE = minSPE, maxSPE = maxSPE,targetSP = test$targetSP, numItemsInStream = 24)
guessingDist <- guessingDist/sum(guessingDist)

lineData <- data.frame(x = seq(-9,18),y = dnorm(seq(-9,18), .8,.8))
lineData$y <- lineData$y + guessingDist

params = data.frame(efficacy = .75, latency = .8, precision = .8, model = 'Normal')

calc_curves_dataframe(test,minSPE,maxSPE,24,params,'Normal')


simulatedPlot <- ggplot(test, aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  theme_apa()

ggsave('~/gitCode/nStream/manuscripts_etc/Manuscript Figures/ExamplePlot.png', plot = simulatedPlot, width = 16, height = 9, units = 'in')
