#######################################
####Confidence Mixed Model Analysis####
#######################################
library(lme4)
library(magrittr)
library(dplyr)

rm(list=ls())

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.table('Analysis/allErrors.txt', sep = '\t', stringsAsFactors = F, header = T)

allData %<>% rename(SPE = error, targetSP=cuePos0)

allData %<>% filter(button %in% c(0,2))

allData %<>% 
  mutate(conf = ifelse(button == 0, 'unsure', 'sure')) %>% 
  mutate(conf = factor(conf, levels = c('sure', 'unsure'))) %>%
  mutate(condition = factor(condition, levels = c('twoStreams', 'eightStreams')))


interceptModel <- glmer(conf~1+(1|ID), data = allData, family = binomial(link = 'logit'))
SPEModel <- glmer(conf~abs(SPE)+(1|ID), data = allData, family = binomial(link='logit'))
conditionModel <- glmer(conf~condition+(1|ID), data = allData, family = binomial(link='logit'))
mainEffectsModel <- glmer(conf~condition+abs(SPE)+(1|ID), data = allData, family = binomial(link='logit'))
interactionModel <- glmer(conf~abs(SPE)*condition+(1|ID), data = allData, family = binomial(link='logit'))


x <- anova(interceptModel, SPEModel,conditionModel, mainEffectsModel, interactionModel)

x

summary(mainEffectsModel)
