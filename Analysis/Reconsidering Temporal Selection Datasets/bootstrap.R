##Bootstrap example on published AB data

library(ggplot2)
library(magrittr)
library(dplyr)
devtools::load_all('../mixRSVP/')

rm(list=ls())

ABData <- read.csv('Analysis/Reconsidering Temporal Selection Datasets/reconsideringTheABData.csv', stringsAsFactors = F)
ABData %<>% filter(dataSet == 'MIT')

ABData %<>% rename(SPE = target1Error, targetSP = target1Pos)

dataSets <- ABData %>% pull(dataSet) %>% unique()


pvalues <- data.frame(
  dataset = character(1e4),
  participant = numeric(1e4),
  SPE = numeric(1e4),
  lag = numeric(1e4),
  count = numeric(1e4),
  pvalue = numeric(1e4),
  stringsAsFactors = F
)

thisRow <- 1
for(thisDataSet in dataSets){
  theseParticipants <- ABData %>% filter(dataSet == thisDataSet) %>% pull(participant) %>% unique %>% sort
  for(thisParticipant in theseParticipants){
    theseLags <- ABData %>% filter(dataSet == thisDataSet, participant == thisParticipant) %>% pull(lag) %>% unique %>% sort
    for(thisLag in theseLags){
      cat('dataSet:', thisDataSet, ', participant:', thisParticipant, 'lag:', thisLag,'                                        \r')
      theseData <- ABData %>% filter(dataSet == thisDataSet, participant == thisParticipant, lag == thisLag)
      thisPvalue <- bootstrapPValue(theseData, 26, -1)
      thisCount <- length(which(theseData$SPE == 0))
      pvalues[thisRow,] <- data.frame(thisDataSet, thisParticipant, -1, thisLag, thisCount, thisPvalue)
      thisRow <- thisRow+1
    }
  }
}

pvalues <- pvalues[1:(thisRow-1),]

write.csv(x = pvalues, file = 'Analysis/Reconsidering Temporal Selection Datasets/pvalues.csv', row.names = F)
