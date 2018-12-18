library(ggplot2)
library(mixRSVP)
rm(list=ls())

setwd('~/gitCode/nStream/modellingScripts/Model recovery/')

latencies <- c(1,1.5)

efficacies <- c(0, .5, 1)

precisions <- c(50,70,100)/(1000/12)

participants = 1:10

nTrials <- 200

nReps <- 100

cuePos = 10


minSPE <- -cuePos + 1
maxSPE <- 24 - cuePos

expandedGrid <- expand.grid(latency=latencies, efficacy =efficacies, precision =precisions, participant = participants)

trials <- matrix(nrow = nrow(expandedGrid), ncol = nTrials)

rawData <- cbind(expandedGrid,trials)

params <- cbind(expandedGrid, 
                data.frame(
                  efficacyEstimate = rep(NA, times = nrow(expandedGrid)),
                  latencyEstimate = rep(NA, times = nrow(expandedGrid)),
                  precisionEstimate = rep(NA, times = nrow(expandedGrid))))

if(!file.exists('modelRecoveryMixRSVP.csv')){ #If we haven't already done the simulations
  for(latency in latencies){
    for(precision in precisions){
      for(efficacy in efficacies){
        for(participant in participants){
          print(paste0('Participant: ', participant,', Latency: ', latency, ', Precision: ', precision, ', Efficacy: ', efficacy))
          
          thisRow <- (expandedGrid$latency == latency & 
            expandedGrid$efficacy == efficacy &
            expandedGrid$precision == precision &
            expandedGrid$participant == participant)
          print(which(thisRow))
          
          theseErrors <- rep(999, times = nTrials) #Simulated data for this "participant"
          
          nonGuesses <- rbinom(nTrials, 1,efficacy) #Which trials are non-guesses? 1 == Non-guess, 0 == Guess
          nNonGuesses <- length(which(nonGuesses==1)) #Number of non-guesses
          
          theseErrors[which(nonGuesses==1)] <- round(rnorm(nNonGuesses, latency,precision)) #Replace the non-guess trials with data from a Gaussian
          
          while(any(theseErrors[which(nonGuesses==1)]>maxSPE | theseErrors[which(nonGuesses==1)]<minSPE)){ #If any of the Gaussian trials are outside the range (minSPE,maxSPE), replace them
            toReplace <- theseErrors[which(nonGuesses==1)]>maxSPE | theseErrors[which(nonGuesses==1)]
            print(theseErrors[toReplace])
            theseErrors[toReplace] <- round(rnorm(length(toReplace), latency,precision))
          }
          
          theseErrors[which(nonGuesses==0)] <- round(runif(nTrials-nNonGuesses,minSPE,maxSPE)) #Guesses come from a uniform distribution
          
          rawData[thisRow,5:204] <- theseErrors #Put simulated data in the rawData object
          
          thisDF <- data.frame(SPE = theseErrors, targetSP = cuePos) #A df with the right column titles for the mixRSVP:: functions
          
          invisible( #invisible combined with capture output hides those print 'error' statements hidden deep in mixRSVP::
            capture.output(
              theseParams <- analyzeOneCondition(thisDF, 24, parameterBounds(), nReps) #fit the data
              )
            )
          
          if(theseParams$pLRtest<.05){ #Check the output of the likelihood ratio test
            params$latencyEstimate[thisRow] <- theseParams$latency
            params$efficacyEstimate[thisRow] <- theseParams$efficacy
            params$precisionEstimate[thisRow] <- theseParams$precision
          } else { #If no different from a uniform, efficacy 0 and no Guassian 
            params$latencyEstimate[thisRow] <- NaN
            params$efficacyEstimate[thisRow] <- 0
            params$precisionEstimate[thisRow] <- NaN
          }
          print(theseParams$pLRtest) #Just checking. Make sure P-value and params make sense. p>.05 means efficacy = 0
          print(params[thisRow, 5:7])
          
        }
      }
    }
  }
  write.csv(x = params, #write the params and raw data
            'modelRecoveryMixRSVP.csv')
  
  write.csv(rawData,
            'modelRecoveryMixRSVPData.csv')
} else{
  params <- read.csv('modelRecoveryMixRSVP.csv') #if we've already run the simulations, read in the saved data
  rawData <- read.csv('modelRecoveryMixRSVPData.csv')
}

cor.test(params$latency, params$latencyEstimate)

params$falsePositive <- params$efficacy==0 & params$efficacyEstimate!=0

ggplot(params, aes(x=latency, y=latencyEstimate))+
  geom_point(aes(colour = falsePositive))+
  stat_summary(fun.y = mean, 
    geom='point', 
    size = 4, 
    colour = '#dca951')+
  stat_summary(fun.data= mean_se,
    geom='errorbar', 
    width = .05, 
    colour = '#dca951')+
  scale_colour_manual(values=c('TRUE' = '#628093' ,'FALSE' = '#ef5e39'), 
    labels=c('TRUE'='False Positive', 'FALSE' = 'True Positive'), 
    name = 'Likelihood Result')+
  theme(panel.background = element_blank(),
    axis.line = element_line(size=.2))+
  scale_x_continuous(breaks = unique(params$latency))+
  geom_hline(yintercept = unique(params$latency), linetype = 'dashed')


cor.test(params$efficacy, params$efficacyEstimate)

ggplot(params, aes(x=efficacy, y=efficacyEstimate))+
  geom_point(aes(colour = falsePositive))+
  stat_summary(fun.y = mean, 
    geom='point', 
    size = 4, 
    colour = '#dca951')+
  stat_summary(fun.data= mean_se,
    geom='errorbar', 
    width = .05, 
    colour = '#dca951')+
  scale_colour_manual(values=c('TRUE' = '#628093' ,'FALSE' = '#ef5e39'), 
    labels=c('TRUE'='False Positive', 'FALSE' = 'True Positive'), 
    name = 'Likelihood Result')+
  theme(panel.background = element_blank(),
    axis.line = element_line(size=.2))+
  geom_hline(yintercept = unique(params$efficacy), linetype = 'dashed')

cor.test(params$precision, params$precisionEstimate)

ggplot(params, aes(x=precision, y=precisionEstimate))+
  geom_point(aes(colour = falsePositive))+
  stat_summary(fun.y = mean, 
               geom='point', 
               size = 4, 
               colour = '#dca951')+
  stat_summary(fun.data= mean_se,
               geom='errorbar', 
               width = .05, 
               colour = '#dca951')+
  scale_colour_manual(values=c('TRUE' = '#628093' ,'FALSE' = '#ef5e39'), 
                      labels=c('TRUE'='False Positive', 'FALSE' = 'True Positive'), 
                      name = 'Likelihood Result')+
  theme(panel.background = element_blank(),
        axis.line = element_line(size=.2))+
  geom_hline(yintercept = unique(params$precision), linetype = 'dashed')
 