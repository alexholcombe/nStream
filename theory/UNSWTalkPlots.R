rm(list=ls())
library(ggplot2)
source('../ggplotElements.R')

p <- .75
nTrials <- 5000
latency <- 1
precision <- 1.5

minError <- -17
maxError <- 17

guessing <- runif((1-p)*nTrials, min = minError, max = maxError)
normSelected <- rnorm(p*nTrials, mean = latency, sd = precision)

logNormSelected <- rlnorm(p*nTrials, mean = 1.2, sd=log(1.5))


normObs <- data.frame(trial = c(rep('guessing', times = (1-p)*nTrials), rep('non-guessing', times = p*nTrials)), error = c(guessing, normSelected))
logNormObs <- data.frame(trial = c(rep('guessing', times = (1-p)*nTrials), rep('non-guessing', times = p*nTrials)), error = c(guessing, logNormSelected))

normErrorHist <- ggplot(normObs, aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(x='Serial Position Error', title = 'Errors')+
  apatheme+
  theme(axis.text.x = element_text(family='Arial', size=20))+
  lims(x=c(-17,17))

normErrorHist


normErrorDensity <- ggplot(normObs, aes(x=error))+
  geom_density(adjust = 5,aes(fill = trial, colour = trial, alpha = trial))+
  scale_fill_manual(values = c('#feb24c', '#31a354'))+
  scale_colour_manual(values = c('#feb24c', '#31a354'))+
  scale_alpha_manual(values = c(1,.8))+
  labs(x = 'Serial Position Error', fill = 'Trial', colour ='Trial', alpha = 'Trial', title = 'Underlying Model')+
  apatheme+
  theme(axis.text.x = element_text(family='Arial', size=20))+
  lims(x=c(-17,17))

normErrorDensity

logNormErrorHist <- ggplot(logNormObs, aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(x='Serial Position Error', title = 'Empirical Errors')+
  apatheme


logNormErrorDensity <- ggplot(logNormObs, aes(x=error))+
  geom_density(adjust = 5,aes(fill = trial, colour = trial, alpha = trial))+
  scale_fill_manual(values = c('#feb24c', '#31a354'))+
  scale_colour_manual(values = c('#feb24c', '#31a354'))+
  scale_alpha_manual(values = c(1,.8))+
  labs(x = 'Serial Position Error', fill = 'Trial', colour ='Trial', alpha = 'Trial', title = 'Underlying Model')+
  apatheme+
  theme(axis.text.x = element_text(family='Arial', size=20))

logNormErrorDensity
