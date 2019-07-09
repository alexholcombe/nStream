library(BayesFactor)
library(effsize)
library(lme4)
rm(list=ls())


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

eightStreams <- read.csv('Analysis/Gamma Fits/paramsDF8_30-04-2019_12-14.csv', stringsAsFactors = T)
precue <- read.csv('Analysis/Gamma Fits/paramsPrecue.csv', stringsAsFactors = T)

eightStreams %<>% filter(model== 'Normal') %>% mutate(experiment = '2v8 Streams')
precue %<>% filter(model == 'Normal') %>% mutate(experiment = 'precue')

params <- rbind(eightStreams, precue) %>% 
  mutate(experiment = factor(experiment),
         condition = factor(condition, levels = c('twoStreams', 'eightStreams')),
         latency = latency*(1000/12),
         precision = precision*(1000/12) )

interceptModelLatency <-  lmer(latency~(1|participant), data = params)
conditionModelLatency <-  lmer(latency~condition+(1|participant), data = params)
experimentModelLatency <-  lmer(latency~experiment+(1|participant), data = params)
conditionAndExperimentModelLatency <- lmer(latency~condition+experiment+(1|participant), data = params)
interactiveModelLatency <- lmer(latency~condition*experiment+(1|participant), data = params)

Anova <- anova(interceptModelLatency,conditionModelLatency, experimentModelLatency, conditionAndExperimentModelLatency, interactiveModelLatency)
interactiveBICLatency <- Anova['interactiveModelLatency','BIC']
additiveBICLatency <- Anova['conditionAndExperimentModelLatency','BIC']

deltaBICLatency <- additiveBICLatency - interactiveBICLatency

interactionBFLatency <- exp(deltaBICLatency/2)

bayesModelLatency <- generalTestBF(latency~condition*experiment+participant, whichRandom = 'participant', data = params)
inclusionBF(model = bayesModelLatency, variable = 'condition:experiment')
inclusionBF(model = bayesModelLatency, variable = 'condition')
inclusionBF(model = bayesModelLatency, variable = 'experiment')


interceptModelPrecision <-  lmer(precision~(1|participant), data = params)
conditionModelPrecision <-  lmer(precision~condition+(1|participant), data = params)
experimentModelPrecision <-  lmer(precision~experiment+(1|participant), data = params)
conditionAndExperimentModelPrecision <- lmer(precision~condition+experiment+(1|participant), data = params)
interactiveModelPrecision <- lmer(precision~condition*experiment+(1|participant), data = params)

Anova <- anova(interceptModelPrecision,conditionModelPrecision, experimentModelPrecision, conditionAndExperimentModelPrecision, interactiveModelPrecision)

interactiveBICPrecision <- Anova['interactiveModelPrecision','BIC']
additiveBICPrecision <- Anova['conditionAndExperimentModelPrecision','BIC']

deltaBICPrecision <- additiveBICPrecision - interactiveBICPrecision

interactionBFPrecision <- exp(deltaBICPrecision/2)

bayesModelPrecision <- anovaBF(precision~condition*experiment+participant, whichRandom = 'participant', data = params)
inclusionBF(model = bayesModelPrecision, variable = 'condition:experiment')
inclusionBF(model = bayesModelPrecision, variable = 'condition')
inclusionBF(model = bayesModelPrecision, variable = 'experiment')


interceptModelEfficacy <-  lmer(efficacy~(1|participant), data = params)
conditionModelEfficacy <-  lmer(efficacy~condition+(1|participant), data = params)
experimentModelEfficacy <-  lmer(efficacy~experiment+(1|participant), data = params)
conditionAndExperimentModelEfficacy <- lmer(efficacy~condition+experiment+(1|participant), data = params)
interactiveModelEfficacy <- lmer(efficacy~condition*experiment+(1|participant), data = params)

Anova <- anova(interceptModelEfficacy,conditionModelEfficacy, experimentModelEfficacy, conditionAndExperimentModelEfficacy, interactiveModelEfficacy)

interactiveBICEfficacy <- Anova['interactiveModelEfficacy','BIC']
additiveBICEfficacy <- Anova['conditionAndExperimentModelEfficacy','BIC']

deltaBICEfficacy <- additiveBICEfficacy - interactiveBICEfficacy

interactionBFEfficacy <- exp(deltaBICEfficacy/2)

bayesModelEfficacy <- anovaBF(efficacy~condition*experiment+participant, whichRandom = 'participant', data = params)
inclusionBF(model = bayesModelEfficacy, variable = 'condition:experiment')
inclusionBF(model = bayesModelEfficacy, variable = 'condition')
inclusionBF(model = bayesModelEfficacy, variable = 'experiment')
