library(ggplot2)
library(dplyr)
library(magrittr)
library(R.matlab)
rm(list=ls())


apa = papaja::theme_apa()

setwd('~/gitCode/nStream/Analysis/Reconsidering Temporal Selection Datasets/')


matFiles <- list.files(pattern = '\\.mat')

allData <- data.frame( #Assigning memory to save time. Cut unused rows later
  dataSet = character(1e5),
  participant = numeric(1e5),
  trial = numeric(1e5),
  block = numeric(1e5),
  target1Pos = numeric(1e5),
  target2Pos = numeric(1e5),
  lag = numeric(1e5),
  target1Error = numeric(1e5),
  target2Error = numeric(1e5),
  target1Response = numeric(1e5),
  target2Response = numeric(1e5),
  stringsAsFactors = F
)

startRow <- 1

for(thisFile in matFiles){
  thisMat <- readMat(thisFile)
  
  thisDatasetName <- thisFile %>% gsub(pattern = '\\.mat|CompiledData_', replacement = '')
  nParticipants <- dim(thisMat$allT1Error)[1]
  nBlocks <- dim(thisMat$allT1Error)[2]
  nTrialsEachBlock <- dim(thisMat$allT1Error)[3]
  
  for(thisParticipant in 1:nParticipants){
    for(thisBlock in 1:nBlocks){
      cat('Dataset: ', thisDatasetName, 'Participant: ', thisParticipant, '. Block: ', thisBlock, '                                                                 \r')
      endRow <- startRow + nTrialsEachBlock - 1
      theseLags <- thisMat$allLags[thisParticipant, thisBlock,]
      theseT1Error <- thisMat$allT1Error[thisParticipant, thisBlock,]
      theseT2Error <- thisMat$allT2Error[thisParticipant, thisBlock,]
      theseT1Pos <- thisMat$allT1Pos[thisParticipant, thisBlock,]
      theseT2Pos <- thisMat$allT2Pos[thisParticipant, thisBlock,]
      theseT1Resp <- thisMat$allT1Resp[thisParticipant, thisBlock,]
      theseT2Resp <- thisMat$allT2Resp[thisParticipant, thisBlock,]
      
      
      theseData <- data.frame(
        dataSet = thisDatasetName,
        participant = thisParticipant,
        trial = 1:nTrialsEachBlock,
        block = thisBlock,
        target1Pos = theseT1Pos,
        target2Pos = theseT2Pos,
        lag = theseLags,
        target1Error = theseT1Error, 
        target2Error = theseT2Error, 
        target1Response = theseT1Resp, 
        target2Response = theseT2Resp,
        stringsAsFactors = F
      )
      
      allData[startRow:endRow,] <- theseData
      startRow <- endRow + 1
    }
  }
}

allData <- allData[1:endRow,]


allData %>% #A sanity check. Need to convert lags to MS for this to be properly interpretable
  filter(target1Error == 0, !is.na(target2Error)) %>%
  group_by(dataSet, participant, lag) %>%
  summarise(conditionalAccuracy = sum(target2Error == 0)/n()) %>%
  ggplot(aes(x = lag, y = conditionalAccuracy))+
  geom_point(aes(group = dataSet), alpha = .3)+
  stat_summary(geom = 'line', fun.y = mean, aes(group = dataSet))+
  scale_x_continuous(breaks = 1:10)+
  apa

write.csv(x = allData, 'reconsideringTheABData.csv', row.names = F)




  

