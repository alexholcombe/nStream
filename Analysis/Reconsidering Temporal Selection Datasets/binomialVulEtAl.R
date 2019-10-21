##Bootstrap example on published AB data

library(ggplot2)
library(magrittr)
library(dplyr)
library(papaja)

setwd('~/gitCode/nStream/')

devtools::load_all('../mixRSVP/')



rm(list=ls())

ABData <- read.csv('Analysis/Reconsidering Temporal Selection Datasets/reconsideringTheABData.csv', stringsAsFactors = F)
ABData %<>% filter(dataSet == 'MIT') #Select Vul et al

ABData %<>% rename(SPE = target1Error, targetSP = target1Pos)

dataSets <- ABData %>% pull(dataSet) %>% unique()

ABData %>% filter(lag<=6) %>% 
  rename(ID = participant) %>%  #Histograms, drop lags >= 7 because the blink is over at this point
  ggplot(aes(x = SPE))+
    geom_histogram(binwidth = 1) +
    facet_grid(cols = vars(lag), rows = vars(ID), labeller = 'label_both')+
    theme_apa()


combinedSPEPlot <- ABData %>% filter(lag<=6) %>% 
  rename(Lag = lag) %>%
  rename(ID = participant) %>%  #Histograms, drop lags >= 7 because the blink is over at this point
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1) +
  facet_grid(cols = vars(Lag), labeller = 'label_both')+
  scale_x_continuous(breaks = seq(-9,9,3))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  theme_apa(base_size = 20)

ggsave(filename = 'Analysis/Reconsidering Temporal Selection Datasets/VulEtAlCombinedSPE.png', plot = combinedSPEPlot, width = 16, height = 9, units = 'in')

ABData %>% #Ye olde conditional accuracy plot
  mutate(conditionalAccuracy = SPE == 0 & target2Error == 0)%>%
  group_by(participant, lag) %>%
  summarise(T2T1 = mean(conditionalAccuracy)) %>%
  ggplot(aes(x = lag*83, y = T2T1))+
    geom_point(aes(group = participant),alpha = .2)+
    geom_line(aes(group = participant),alpha = .2)+
    stat_summary(fun.y = mean, geom = 'point')+
    stat_summary(fun.y = mean, geom = 'line')+
    stat_summary(fun.data = mean_se, geom = 'errorbar')+
    labs(x = 'lag (ms)', y = 'p(T2|T1)')+
    theme_apa()

pvalues <- data.frame( #empty dataframe of pvalues, there are deliberately too many rows to ensure memory is appropriate. I trim them below
  dataset = character(1e4),
  participant = numeric(1e4),
  SPE = numeric(1e4),
  lag = numeric(1e4),
  count = numeric(1e4),
  pvalue = numeric(1e4),
  stringsAsFactors = F
)

SPEToTest <- -1 #Which SPE are we testing?

thisRow <- 1
for(thisDataSet in dataSets){
  theseParticipants <- ABData %>% filter(dataSet == thisDataSet) %>% pull(participant) %>% unique %>% sort
  for(thisParticipant in theseParticipants){
    theseLags <- ABData %>% filter(dataSet == thisDataSet, participant == thisParticipant) %>% pull(lag) %>% unique %>% sort
    for(thisLag in theseLags){
      cat('dataSet:', thisDataSet, ', participant:', thisParticipant, 'lag:', thisLag,'                                        \r')
      theseData <- ABData %>% filter(dataSet == thisDataSet, participant == thisParticipant, lag == thisLag)
      
      thisEfficacy <- sum(theseData$SPE == 0)/nrow(theseData) #Efficacy from SPE = 0
      
      minSPE <- 1 - max(theseData$targetSP)#guessing distribution arguments
      maxSPE <- 24 - min(theseData$targetSP)
      
      thisXDomain <- minSPE:maxSPE
      
      guessingDist <- createGuessingDistribution(minSPE, maxSPE, theseData$targetSP, 24)
      guessingDist <- guessingDist/sum(guessingDist) #Guessing distribution as probabilities
      
      thisProb <- guessingDist[thisXDomain == SPEToTest]*(1-thisEfficacy) #Scale the guessing distribution by 1-efficacy
      
      thisPvalue <- pbinom(sum(theseData$SPE == SPEToTest), nrow(theseData), thisProb, lower.tail = F) #Calculate a p-value, For this many trials, given the probabilty of -1 in the guessing distribution scaled by efficacy, what is the probability of a count at least as extreme as this count
      
      thisCount <- length(which(theseData$SPE == 0))
      pvalues[thisRow,] <- data.frame(thisDataSet, thisParticipant, -1, thisLag, thisCount, thisPvalue)
      thisRow <- thisRow+1
    }
  }
}

pvalues <- pvalues[1:(thisRow-1),]

write.csv(x = pvalues, file = 'Analysis/Reconsidering Temporal Selection Datasets/pvalues.csv', row.names = F)
