library(ggplot2)
library(dplyr)
library(magrittr)
library(BayesFactor)
rm(list=ls())
setwd('~/gitCode/nStream/')

inclusionBF <- function(model, variable){
  
  ###https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp###

  
  priorProbs <- model %>% newPriorOdds() %>% `*`(model) %>% as.BFprobability() %>% as.vector() 
  
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


allErrors <- read.table('Analysis/allErrors.txt', sep='\t', header = T)

buttonCounts <- expand.grid(participant = unique(allErrors$ID),
                            nStream = c('twoStreams','eightStreams'),
                            zero = numeric(1),
                            two = numeric(2))

efficacy <- read.csv('modelOutput/CSV/Confidence/TGRSVP_Exp2_EfficacyNormConfidence.csv')
latency <- read.csv('modelOutput/CSV/Confidence/TGRSVP_Exp2_LatencyNormConfidence.csv')
precision <- read.csv('modelOutput/CSV/Confidence/TGRSVP_Exp2_PrecisionNormConfidence.csv')

efficacy$buttonPresses <- numeric(nrow(efficacy))
latency$buttonPresses <- numeric(nrow(efficacy))
precision$buttonPresses <- numeric(nrow(efficacy))

for(thisParticipant in unique(efficacy$Participant)){
    theseData <- allErrors %>% filter(ID == thisParticipant)
    
    # thisPlot <- ggplot(theseData, aes(x = error))+
    #   geom_histogram(binwidth = 1)+
    #   facet_wrap(~condition+factor(button))
    # 
    # show(thisPlot)
    for(thisNStream in c('twoStreams','eightStreams')){
      twoCount <- theseData %>% filter(condition == thisNStream) %>% pull(button) %>% '=='(2) %>% which() %>% length
      zeroCount <- theseData %>% filter(condition == thisNStream) %>% pull(button) %>% '=='(0) %>% which() %>% length
      
      efficacy %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'Low', zeroCount))
      efficacy %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'High', twoCount))
      
      latency %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'Low', zeroCount))
      latency %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'High', twoCount))
      
      precision %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'Low', zeroCount))
      precision %<>% mutate(buttonPresses = replace(buttonPresses, Participant == thisParticipant & nStream == thisNStream & Confidence == 'High', twoCount))
    }
}

precision %<>% mutate(ci_width = Upper - Lower)
latency %<>% mutate(ci_width = Upper - Lower)
efficacy %<>% mutate(ci_width = Upper - Lower)

efficacy %<>% mutate(participantN = rep(1:10, times = 4) %>% factor()) #because our participant codes trigger some base64 weirdness in anovaBF
latency %<>% mutate(participantN = rep(1:10, times = 4) %>% factor()) #because our participant codes trigger some base64 weirdness in anovaBF
precision %<>% mutate(participantN = rep(1:10, times = 4) %>% factor()) #because our participant codes trigger some base64 weirdness in anovaBF

efficacyBF <- anovaBF(Estimate~Confidence*nStream+participantN,
                          data = efficacy, 
                          whichRandom = 'participantN')

efficacyInclusionBFs <- expand.grid(factor = c('Confidence','nStream','nStream:Confidence'),
                                    BF = numeric(1))

for(thisFactor in efficacyInclusionBFs$factor){
  efficacyInclusionBFs %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(efficacyBF, thisFactor)))
}

ggplot(efficacy, aes(x = buttonPresses, y = ci_width))+
  geom_point()+
  facet_wrap(~nStream)

latencyBF <- anovaBF(Estimate~Confidence*nStream+participantN,
                      data = latency[complete.cases(latency),], 
                      whichRandom = 'participantN')

latencyInclusionBFs <- expand.grid(factor = c('Confidence','nStream','nStream:Confidence'),
                                    BF = numeric(1))

for(thisFactor in latencyInclusionBFs$factor){
  latencyInclusionBFs %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(latencyBF, thisFactor)))
}

ggplot(latency, aes(x = buttonPresses, y = ci_width))+
  geom_point()+
  facet_wrap(~nStream)

ggplot(precision, aes(x = buttonPresses, y = ci_width))+
  geom_point()+
  facet_wrap(~nStream)

ggplot(efficacy, aes(x=Confidence, y = buttonPresses))+
  geom_point(alpha = .4)+
  facet_wrap(~nStream)+
  stat_summary(fun.y = mean, geom='point', size = 3, alpha = .4)+
  stat_summary(fun.data = mean_se, geom='errorbar', width =.2)
 
ggplot(efficacy, aes(x=nStream, y = Estimate))+
  geom_point(aes(colour = Participant, group = Confidence), position = position_dodge(1), alpha = .6)+
  geom_errorbar(aes(ymin=Lower,ymax = Upper, colour = Participant, group=Confidence ),position = position_dodge(1), width = .2, alpha = .4)


ggplot(latency, aes(x=nStream, y = Estimate))+
  geom_point(aes(colour = Participant, group = Confidence), position = position_dodge(1), alpha = .6)+
  geom_errorbar(aes(ymin=Lower,ymax = Upper, colour = Participant, group=Confidence ),position = position_dodge(1), width = .2, alpha = .4)


precisionBF <- anovaBF(Estimate~Confidence*nStream+participantN,
                     data = precision[complete.cases(precision),], 
                     whichRandom = 'participantN')

precisionInclusionBFs <- expand.grid(factor = c('Confidence','nStream','nStream:Confidence'),
                                   BF = numeric(1))

for(thisFactor in precisionInclusionBFs$factor){
  precisionInclusionBFs %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(precisionBF, thisFactor)))
}

ggplot(precision, aes(x=nStream, y = Estimate))+
  geom_point(aes(colour = Confidence), position = position_dodge(1), alpha = .6)+
  #geom_errorbar(aes(ymin=Lower,ymax = Upper, group=Confidence ),position = position_dodge(1), width = .2, alpha = .4)+
  lims(y=c(-200,200))
