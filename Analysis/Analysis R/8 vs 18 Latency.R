########################################
###Compare eight and eighteen streams###
########################################
library(magrittr)
library(BayesFactor)
library(dplyr)

rm(list=ls())

eightParams <- read.csv('Analysis/Gamma Fits/paramsDF8_30-04-2019_12-14.csv')
eighteenParams <- read.csv('Analysis/Gamma Fits/paramsDF18_25-04-2019_16-04.csv')

#####################
###Compare latency###
#####################

x = eighteenParams %>% filter(condition == 18, model == 'Normal') %>% pull(latency)
y = eightParams %>% filter(condition == 'eightStreams', model == 'Normal') %>% pull(latency)

ttestBF(x = x, y = y,rscale = 1/sqrt(2))

#######################
###Compare precision###
#######################

x = eighteenParams %>% filter(condition == 18, model == 'Normal') %>% pull(precision)
y = eightParams %>% filter(condition == 'eightStreams', model == 'Normal') %>% pull(precision)

ttestBF(x = x, y = y,rscale = sqrt(2))
