####################################################
###Collate all Goodbourn and Holcombe (2015) data###
####################################################


library(ggplot2)
library(magrittr)
library(dplyr)
library(R.matlab)

rm(list=ls())

setwd('~/gitCode/nStream/Analysis/Goodbourn and Holcombe/')

observers <- c('Experienced', 'SONA')

Stimuli <- LETTERS[!LETTERS %in% c('C','V')]

possibleFiles <- list.files(pattern = '\\.mat',recursive = T)

dataFiles <- possibleFiles[grepl(pattern = '(?<!User)Data/.*\\.mat', x = possibleFiles, perl = T)]

##allLetterOrder is identity of the letters
##allResponses is the identity of the responses letter
##allTargets is the temporal position of the cue
##paste0('####', 1:24,' = ', Stimuli, sep = '\n\r') %>% cat()
####1 = A
####2 = B
####3 = D
####4 = E
####5 = F
####6 = G
####7 = H
####8 = I
####9 = J
####10 = K
####11 = L
####12 = M
####13 = N
####14 = O
####15 = P
####16 = Q
####17 = R
####18 = S
####19 = T
####20 = U
####21 = W
####22 = X
####23 = Y
####24 = Z

allData <- data.frame(
  ID = character(1.5e6),
  date = character(1.5e6),
  exp = character(1.5e6),
  pool = character(1.5e6),
  block = numeric(1.5e6),
  trial = numeric(1.5e6),
  stream = numeric(1.5e6),
  condition = numeric(1.5e6),
  targetSP = numeric(1.5e6),
  SPE = numeric(1.5e6),
  rate = numeric(1.5e6),
  eccentricity = numeric(1.5e6),
  stimuli = character(1.5e6),
  stringsAsFactors=F
)

startRow <- 1

lastParticipant <- 'None'
for(file in dataFiles){
  temp <- readMat(file)
  thisID <- gsub(pattern = '.*Data/(?=[A-Z][A-Z])|(?<=[A-Z][A-Z])_.*', x = file, replacement = '', perl = T)
  if(thisID == lastParticipant){
    thisBlock <- thisBlock + 1
  } else {
    thisBlock <- 1
  }
  lastParticipant = thisID
  thisDate <- gsub('.*_(?=[0-9][0-9])|_([0-9]\\.).*', file, replacement ='', perl = T) %>% as.POSIXct (format = '%y-%m-%d') %>% as.character()
  
  thisExp <- gsub('.*SONA/|.*Observers/|/Data/.*', file, replacement = '', perl = T)
  
  thisPool <- gsub('.*Materials/|/Exp(?=[0-9]).*', file, replacement = '', perl = T)
  
  if('allPositions' %in% names(temp)){
    thisCondition <- temp$allPositions
  } else if('conditionNum' %in% names(temp)){
    thisCondition <- temp$conditionNum
  }
  
  tempDFRows <- dim(temp$allResponses)[1]
  
  tempDF <- expand.grid(
    ID = character(1),
    date = character(1),
    exp = character(1),
    pool = character(1),
    block = numeric(1),
    trial = 1:tempDFRows,
    stream = 1:2,
    condition = numeric(1),
    targetSP = numeric(1),
    SPE = numeric(1),
    rate = numeric(1),
    eccentricity = numeric(1),
    stimuli = character(1),
    stringsAsFactors = F
  )
  
  # if(length(thisCondition) > 1){
  #   thisCondition <- rep(thisCondition,times = 2) #One for each stream
  # }
  
  tempDF %<>% mutate(ID = thisID,
                      date = thisDate,
                      exp = thisExp,
                      pool = thisPool,
                      block = thisBlock
                     )
  
  for(thisStream in 1:2){
    stop = F
    
    allStimuli <- c()
    allSPEs <- c()
    streamConditions <- c()
    nTrialsTemp <- dim(temp$allResponses)[1]
    for(trial in 1:nTrialsTemp){
      
      if(length(thisCondition)==1){
        thisTrialCondition = thisCondition
      } else {
        thisTrialCondition = thisCondition[trial]
      }
      
      cat(' Participant = ', thisID, '. Condition = ', thisTrialCondition, '. Stream = ', thisStream, '. Exp = ', thisExp, '. Block = ', thisBlock, '                     \r', collapse = '')
      
      streamConditions <- c(streamConditions, thisTrialCondition)
      
      if(all(temp$allLetterOrder[trial, thisStream, ]==25)){ #If there were no stimuli in this stream
        theseStimuli = NA
      } else {
        theseStimuli <- Stimuli[temp$allLetterOrder[trial, thisStream, ]] %>% paste(collapse = '')
      }
      
      
      allStimuli <- c(allStimuli, theseStimuli)
      thisResponseSP <- which(temp$allLetterOrder[trial, thisStream, ] == temp$allResponses[trial,thisStream])
      
      if(length(thisResponseSP>0)){
        thisSPE <- thisResponseSP - temp$allTargets[trial, thisStream] 
      } else {
        thisSPE <- NA
      }
      
      
      allSPEs <- c(allSPEs, thisSPE)
    }
    
    if(stop){
      break
    }
    tempDF %<>% mutate(
      stimuli = replace(stimuli, stream == thisStream, allStimuli),
      SPE = replace(SPE, stream == thisStream, allSPEs),
      targetSP = replace(targetSP, stream == thisStream, as.vector(temp$allTargets[,thisStream]) ),
      condition = replace(condition, stream == thisStream, streamConditions)
    )
    
    }
    endRow <- startRow + nrow(tempDF) -1
    allData[startRow:endRow,] <- tempDF
    startRow = endRow + 1
  
  if(stop){
    break
  }
}

allData <- allData[-(startRow:nrow(allData)),]

write.csv(file = 'Data and Materials/allData.csv',
          x = allData)
