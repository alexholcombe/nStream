rm(list=ls())
library(ggplot2)
source('../ggplotElements.R')
theme_set(theme_apa(base_size = 20) ) 

p <- .99 #efficacy
nTrials <- 10000
latency <- 0.6
precision <- 1.5

minError <- -17
maxError <- 17

guessing <- runif((1-p)*nTrials, min = minError, max = maxError)
normSelected <- rnorm(p*nTrials, mean = latency, sd = precision)

logNorm_sd = 3 #7
logNorm_latency = 1
logNormSelected <- rlnorm(p*nTrials, mean = logNorm_latency, sd=log(logNorm_sd))


normObs <- data.frame(trial = c(rep('guessing', times = (1-p)*nTrials), 
                      rep('non-guessing', times = p*nTrials)), error = c(guessing, normSelected))
logNormObs <- data.frame(trial = c(rep('guessing', times = (1-p)*nTrials), 
                         rep('non-guessing', times = p*nTrials)), error = c(guessing, logNormSelected))

normErrorHist <- ggplot(normObs, aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(x='Serial Position Error', title = 'Errors')+
  theme(axis.text.x = element_text(family='Arial', size=20))+
  xlim(-17,17) + geom_vline(xintercept=0)

normErrorHist

ylims<-c(0,0.21)

normErrorDensity <- ggplot(normObs, aes(x=error))+
  geom_density(adjust = 5,aes(fill = trial, colour = trial, alpha = trial))+
  scale_fill_manual(values = c('#feb24c', '#31a354'))+
  scale_colour_manual(values = c('#feb24c', '#31a354'))+
  scale_alpha_manual(values = c(1,.8))+
  labs(x = 'Serial Position Error', fill = 'Trial', colour ='Trial', alpha = 'Trial', title = 'Model')+
  theme(axis.text.x = element_text(family='Arial', size=20))+
  xlim(-17,17) +  ylim(ylims)+ #+ geom_vline(xintercept=0) 
  theme(plot.background = element_rect(fill = "transparent",colour = NA),  panel.background = element_blank()) #so can save transparent image, hopefully

normErrorDensity
ggsave("normErrorDensity.png", bg = "transparent")

logNormErrorHist <- ggplot(logNormObs, aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(x='Serial Position Error', title = 'Empirical Errors')

logNormErrorDensity <- ggplot(logNormObs, aes(x=error))+
  geom_density(adjust = 5,aes(fill = trial, colour = trial, alpha = trial))+
  scale_fill_manual(values = c('#feb24c', '#31a354'))+
  scale_colour_manual(values = c('#feb24c', '#31a354'))+
  scale_alpha_manual(values = c(1,.8))+
  xlim(-17,17) + # geom_vline(xintercept=0) 
  labs(x = 'Serial Position Error', fill = 'Trial', colour ='Trial', alpha = 'Trial', title = 'Model') +
  theme (plot.background = element_rect(fill = "transparent",colour = NA),  panel.background = element_blank()) #so can save transparent image, hopefully
#+  theme(axis.text.x = element_text(family='Arial', size=20)) 

logNormErrorDensity 
ggsave("logNormErrorDensity.png", bg = "transparent")

logNormErrorDensity + ylim(ylims) 