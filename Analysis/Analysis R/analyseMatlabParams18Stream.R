rm(list=ls())
setwd('~/gitCode/nStream/')


library(ggplot2)
library(magrittr)
library(BayesFactor)
library(dplyr)
library(truncnorm)

PowerPointSize <- c(29.21, 12.09)
PowerPointUnit <- 'cm'

analyseParams <- function(params, thisModel){
  nIterations <- 10000
  thisModelParams <- params %>% filter(Model == thisModel)

  resultsList <- list(Efficacy = list(),
                      Latency = list(),
                      Precision = list()) #nest analysis results by param[1] -> Anova/Order-restriction models
                                          #                              [2] -> Descriptives
                                          #                              [3] -> Plot

  ##############
  ###EFFICACY###
  ##############
  theseEfficacies <- thisModelParams %>% filter(Parameter == 'Efficacy')

  descriptives <- theseEfficacies %>% group_by(Group) %>% summarise(mean = mean(Estimate), sd = sd(Estimate), n=length(Estimate))

  efficacyFullVsNull <- anovaBF(Estimate~Group + Participant,
                                whichRandom = 'Participant',
                                data=theseEfficacies)

  efficacyPlot <- ggplot(theseEfficacies, aes(x=Group, y =Estimate))+
    geom_point(alpha=1, colour = '#dca951', size = 4)+
    geom_line(aes(group = Participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), alpha = .7, size = 5)+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, alpha = .7)+
    labs(x = "Number of Streams", y = "Efficacy [1 - p(guess)]")+
    lims(y = c(0,1))


  resultsList$Efficacy %<>% c(.,
                              Bayes = efficacyFullVsNull,
                              Descriptives = list(descriptives),
                              scatterPlot = list(efficacyPlot))

  ##############
  ###LATENCY####
  ##############
  theseLatencies <- thisModelParams %>% filter(Parameter == 'Latency')

  descriptives <- theseLatencies %>% group_by(Group) %>% summarise(mean = mean(Estimate), sd = sd(Estimate), n=length(Estimate))

  latencyFullVsNull <- anovaBF(Estimate~Group + Participant,
                                whichRandom = 'Participant',
                                data=theseLatencies)

  samples <- posterior(latencyFullVsNull, iterations = nIterations)

  consistent <- samples[,'Group-2']<samples[,'Group-6'] & samples[,'Group-6']<samples[,'Group-18']

  latencyOrderVsFull <- (sum(consistent)/nIterations)/(1/6)
  latencyOrderVsNull <- latencyOrderVsFull*as.vector(latencyFullVsNull)

  latencyPlot <- ggplot(theseLatencies, aes(x=Group, y =Estimate))+
    geom_point(alpha=.9, colour = '#628093', size = 5)+
    geom_line(aes(group = Participant),alpha = .3)+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, size = .5)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), size = 6, shape = 23,fill = '#ffa951')+
    scale_colour_brewer(palette = 'Spectral')+
    lims(y = c(-50,200))+
    labs(x = 'Number of Streams', y = 'Latency (ms)')


  resultsList$Latency %<>% c(.,
                             Bayes = list(FullVsNull = latencyFullVsNull, OrderedVsNull = latencyOrderVsNull, OrderedVsFull = latencyOrderVsFull),
                             descriptives =list(descriptives),
                             scatterPlot = list(latencyPlot))

  ###############
  ###PRECISION###
  ###############
  thesePrecisions <- thisModelParams %>% filter(Parameter == 'Precision')

  descriptives <- thesePrecisions %>% group_by(Group) %>% summarise(mean = mean(Estimate), sd = sd(Estimate), n=length(Estimate))

  precisionFullVsNull <- anovaBF(Estimate~Group + Participant,
                               whichRandom = 'Participant',
                               data=thesePrecisions)

  samples <- posterior(precisionFullVsNull, iterations = nIterations)

  consistent <- samples[,'Group-2']>samples[,'Group-6'] & samples[,'Group-6']>samples[,'Group-18']

  precisionOrderVsFull <- (sum(consistent)/nIterations)/(1/6)
  precisionOrderVsNull <- precisionOrderVsFull*as.vector(precisionFullVsNull)

  thesePrecisions %<>% mutate(equalityLabel = ifelse(Group != 2, 'Equal', '2'))
  thesePrecisions %<>% mutate(equalityLabel = factor(equalityLabel))

  EqualVsNull <- anovaBF(Estimate~equalityLabel+Participant,
                         whichRandom = 'Participant',
                         data = thesePrecisions)

  OrderVsEqual = precisionOrderVsNull/as.vector(EqualVsNull)

  precisionPlot <- ggplot(thesePrecisions, aes(x=Group, y =Estimate))+
    geom_point(alpha=1, colour = '#dca951', size = 4)+
    geom_line(aes(group = Participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), alpha = .7, size = 5)+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, alpha = .7)+
    scale_colour_brewer(palette = 'Spectral')+
    lims(y = c(0,150))+
    labs(x = 'Number of Streams', y = 'Precision (ms)')

  resultsList$Precision %<>% c(.,
                             Bayes = list(FullVsNull = precisionFullVsNull, OrderedVsNull = precisionOrderVsNull, OrderedVsFull = precisionOrderVsFull, EqualVsNull = EqualVsNull, OrderVsEqual = OrderVsEqual),
                             Descriptives =list(descriptives),
                             scatterPlot = list(precisionPlot))


  return(resultsList)

}

allErrors <- read.csv('Analysis/allErrors18Streams.txt')

modelBayesFactors <- read.csv('modelOutput/18Streams/BF_ByParticipant.csv')

latencyNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_LatencyNorm.csv')
precisionNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_PrecisionNorm.csv')
efficacyNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_EfficacyNorm.csv')

latencyTruncNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_LatencyTruncNorm.csv')
precisionTruncNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_PrecisionTruncNorm.csv')
efficacyTruncNorm <- read.csv('modelOutput/18Streams/CSV/TGRSVP_Exp2_EfficacyTruncNorm.csv')

params <- rbind(latencyNorm,
                precisionNorm,
                efficacyNorm,
                latencyTruncNorm,
                precisionTruncNorm,
                efficacyTruncNorm)

params %<>% mutate(estimateAsSPE = ifelse(Parameter!='Efficacy', Estimate/(1000/12), Estimate))

params$Group %<>% as.factor() #Need IV as factor for BayesFactor functions

params %<>% filter(Participant != '18TR1') #No eyetracking for this participant

modelBayesFactors$Preferred = '999'

for(thisParticipant in unique(modelBayesFactors$Participant)){
  for(thisGroup in unique(modelBayesFactors$Group)){
    thisBF <- modelBayesFactors %>% filter(Participant == thisParticipant & Group == thisGroup) %>% pull(BF)
    if(thisBF > 3){
      modelBayesFactors %<>% mutate(Preferred = replace(Preferred,
                                                        Participant == thisParticipant & Group == thisGroup,
                                                        'Buffering'))
    } else if(thisBF < 1/3){
      modelBayesFactors %<>% mutate(Preferred = replace(Preferred,
                                                        Participant == thisParticipant & Group == thisGroup,
                                                        'attention'))
    } else if(thisBF> 1/3 | thisBF<3){
      modelBayesFactors %<>% mutate(Preferred = replace(Preferred,
                                                        Participant == thisParticipant & Group == thisGroup,
                                                        'Neither'))
    }
  }
}

################################
###Counts of model selections###
################################
modelBayesFactors %>% group_by(Group) %>% summarise(buffer = length(which(Preferred=='Buffering')),
                                                    attention = length(which(Preferred == 'attention')),
                                                    Neither = length(which(Preferred == 'Neither')))

#Norm Results
NormResults <- analyseParams(params,'Normal')

#Truncated Norm Results
TNormResults <- analyseParams(params,'Normal')


ggsave(
  x = NormResults$Latency$scatterPlot,
  filename = 'modelOutput/18Streams/LatencyScatter18Streams.png',
  height = PowerPointSize[2],
  width = PowerPointSize[1],
  units = PowerPointUnit
)

 ##################
###DOESNT WORK####
##################
#
# for(thisParticipant in unique(params$Participant)){
#   densities <- data.frame(SPE = numeric(0),
#                           condition = character(0),
#                           model = character(0),
#                           density = numeric(0))
#   for(thisGroup in unique(params$Group)){
#     theseErrors <- allErrors %>% filter(ID == thisParticipant & condition == thisGroup & !fixationReject)
#     theseNormParams <- params %>% filter(Participant == thisParticipant & Group == thisGroup & Model == 'Normal')
#     normLatency <- theseNormParams %>% filter(Parameter == 'Latency') %>% pull(Estimate)
#     normLatency <- normLatency/(1000/12)
#     normPrecision <- theseNormParams %>% filter(Parameter == 'Precision') %>% pull(Estimate)
#     normPrecision <- normPrecision/(1000/12)
#
#     theseTNormParams <- params %>% filter(Participant == thisParticipant & Group == thisGroup & Model == 'Truncated Normal')
#     tNormLatency <- theseTNormParams %>% filter(Parameter == 'Latency') %>% pull(Estimate)
#     tNormLatency <- tNormLatency/(1000/12)
#     tNormPrecision <- theseTNormParams %>% filter(Parameter == 'Precision') %>% pull(Estimate)
#     tNormPrecision <- tNormPrecision/(1000/12)
#     tNormA <- ifelse((tNormLatency-tNormPrecision)<0, 0, tNormLatency-tNormPrecision)
#
#     minSPE <- theseErrors %>% pull(SPE) %>% min
#     maxSPE <- theseErrors %>% pull(SPE) %>% max
#
#     thisRange <- seq(minSPE, maxSPE, .1)
#
#     normDensities <- data.frame(SPE = thisRange,
#                                 condition = thisGroup,
#                                 model = 'Attention',
#                                 density =  dnorm(thisRange,
#                                                  mean = normLatency,
#                                                  sd = normPrecision))
#
#     tNormDensities <- data.frame(SPE = thisRange,
#                                  condition = thisGroup,
#                                  model = 'Attention',
#                                  density = dtruncnorm(thisRange,
#                                                       mean = tNormLatency,
#                                                       sd = tNormPrecision,
#                                                       a = tNormA,
#                                                       b = Inf))
#
#
#     densities <- rbind(densities, normDensities, tNormDensities)
#
#   }
#   errors <- allErrors %>% filter(ID == thisParticipant & !fixationReject)
#   thisNRow <- errors %>% nrow
#
#   preferredModel <- modelBayesFactors %>% filter(Participant == thisParticipant)
#
#   densities$preferredModel <- '999'
#
#   for(thisDensityCondition in c(2,6,18)){
#     thisPreferredModel <- preferredModel %>% filter(Group == thisDensityCondition) %>% pull(Preferred)
#     densities %<>% mutate(preferredModel = replace(preferredModel,
#                                                    condition == thisDensityCondition,
#                                                    thisPreferredModel))
#   }
#
#   thisPlot <- ggplot(errors, aes(x=SPE))+
#     #geom_histogram(binwidth = 1)+
#     geom_line(data = densities, aes(x = SPE, y=density*thisNRow/3, group = interaction(model, preferredModel), colour = preferredModel))+ #scale density to histogram with density * N * binwidth
#     scale_y_continuous(sec.axis = sec_axis(~./nrow(theseErrors), name = 'Density'))+
#     labs(y = 'Frequency', title = thisParticipant)+
#     facet_wrap(~factor(condition))
#
#   show(thisPlot)
# }

