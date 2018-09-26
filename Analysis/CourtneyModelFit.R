library(ggplot2)
library(magrittr)
library(mixRSVP)
library(dplyr)

rm(list=ls()) #remove all variables in workspace?

setwd('~/gitCode/nStream/')  #Charlie-specific
#source('../ggplotElements.R') #charlie-specific

source('ggplotElements.R')
theme_set(theme_apa(base_size = 20)) 

dataPath <- 'rawData'

pracTrials <- 1:20

nReps = 100
bounds <- parameterBounds()

bounds['precision','upper'] <- 3


ringNames <- c('one','two','three')

files <- list.files(pattern = '^CH.*\\.txt$', path = dataPath, full.names = T)
print(files)
#removed split files
#splitFiles <- grep('rawData/IK4_04May2017_14-35.txt|rawData/IK42_04May2017_15-06.txt|rawData/LT5_04May2017_15-52.txt|rawData/LT52_04May2017_16-21.txt', files)
#files <- files[-splitFiles]

# for(group in groups){
#   groupFiles <- list.files(path=paste0(dataPath,group), pattern='.txt',full.names=T)
#   groupFiles <- groupFiles[grep('Hubert',groupFiles, invert=T)]
#   files <- c(files, groupFiles)
# }


widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 56.5 #cm
pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180)

eyetrackerFiles <- list.files(path = 'rawData/Eyetracker',full.names = T)

criterion = 1 #fixation reject criterion

skew <- function(x){
  denom <- (sd(x,na.rm=T)^3)*(length(which(!is.na(x)))-1)*(length(which(!is.na(x)))-2)
  deviation <- x[!is.na(x)] - mean(x,na.rm=T)
  numer <- sum(deviation^3)*length(which(!is.na(x)))
  numer/denom
}


dataSets <- list()



totalRows <- 0


IDs <- character()



for(dataset in files){
  ID <- strsplit(dataset, '(?<=a)/|_(?=[0-9])', perl=T)[[1]][2]
  ID <- paste0(strsplit(ID,'')[[1]][1:2],collapse = '')
  print(ID)
  if(!ID %in% IDs){
    IDs <- c(IDs, ID)
  }
  #Some date formatting
  dateString <- paste(strsplit(dataset, '_(?=[0-9])|\\.txt', perl=T)[[1]][2:3], collapse = '_')
  dateString <- as.POSIXct(dateString,format='%d%b%Y_%H-%M')
  
  temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)
  
  print(nrow(temp))
  
  totalRows <- totalRows + nrow(temp)
  
  #nStreams <- length(grep('streamLtrSequence', colnames(temp))) 
  
  
  dataSets[[ID]][[as.character(dateString)]][['data']] <- temp
}

totalRows <- totalRows - (length(IDs)*20) #because we'll be dropping the first 20 trials

allErrors <- data.frame(
  exp = character(totalRows), 
  crowded = character(totalRows), 
  SPE = numeric(totalRows), 
  targetSP = numeric(totalRows),
  ID = character(totalRows), 
  ring = numeric(totalRows),
  stringsAsFactors = F)

startRow <- 1

theseTempCols <- c('responsePosRelative0', 'cuePos0', 'crowded', 'ring')

for(participant in names(dataSets)){
  for(dateString in names(dataSets[[participant]])){
    print(participant)
    
    nInAlphabeticalOrder <- which(IDs==participant) #parameter estimates from MM are in alphabetical order, so this lets me select the appropriate params
          
    temp <- dataSets[[participant]][[dateString]][['data']]
    
    timesForThisParticipant <- dataSets[[participant]] %>% names %>% as.POSIXct()

    firstBlock <- as.POSIXct(dateString) == min(timesForThisParticipant) #if true, drop the first 20 trials as training
    if(firstBlock){
      print('firstBlock')
      temp <- temp[-pracTrials,]
    }
      
    temp$responsePos <- temp$cuePos0+temp$responsePosRelative0
    
    tempSkewNStreams <- aggregate(responsePosRelative0~crowded, temp, skew)
    
    tempSkewTotal <- skew(temp$responsePosRelative0)
    
    #Create densities for the SPE with fixation rejections removed
    
    streamColumns <- grep('streamLtrSequence', colnames(temp))
    
    crowded <- temp[temp$crowded=='Yes',]
    
    for(ring in 0:2){
      thisRingName <- ringNames[ring+1]
      thisRingTrials <- crowded[crowded$ring==ring,]
      #print(nrow(thisRingTrials))
      assign(paste0('crowded',thisRingName), thisRingTrials)
    }
    
    bouma <- temp[temp$crowded=='Bouma',]
    for(ring in 0:2){
      thisRingName <- ringNames[ring+1]
      thisRingTrials <- bouma[bouma$ring==ring,]
      #print(nrow(thisRingTrials))
      assign(paste0('bouma',thisRingName), thisRingTrials)
    }    
    
    endRow <- startRow + nrow(temp) -1
    
  

    allErrors$SPE[startRow:endRow] <- temp$responsePosRelative0
    
    allErrors$targetSP[startRow:endRow] <- temp$cuePos0
    
    allErrors$ID[startRow:endRow] <- participant
    
    
    allErrors$crowded[startRow:endRow] <- temp$crowded
    
    allErrors$ring[startRow:endRow] <- temp$ring
      
    startRow <- endRow + 1
  }
}

params <- expand.grid(
  ID = 'CH',
  ring = 0:2,
  crowded = c('Yes','Bouma'),
  efficacy = -999,
  latency = -999,
  precision = -999,
  val = -999,
  valGuessing = -999,
  pLRtest = -999
)

for(thisParticipant in unique(allErrors$ID)){
  for(thisCondition in unique(allErrors$crowded)){
    for(thisRing in unique(allErrors$ring)){
      print(paste0('Participant: ', thisParticipant, '. Ring: ', thisRing, '. Condition: ', thisCondition))
      

      theseParams <- allErrors %>% filter(., crowded == thisCondition, ring == thisRing, ID == thisParticipant) %>% analyzeOneCondition(., 24, bounds, nReps)
      #Doing everything I can to silence those messages
      
      if(theseParams$pLRtest<.05){
        params %<>%
          mutate(efficacy=replace(efficacy, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$efficacy)) %>%
          as.data.frame()
        
        params %<>%
          mutate(latency=replace(latency, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$latency)) %>%
          as.data.frame()
        
        params %<>%
          mutate(precision=replace(precision, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$precision)) %>%
          as.data.frame()
      } else {
        params %<>%
          mutate(efficacy=replace(efficacy, ID == thisParticipant & crowded == thisCondition & ring == thisRing, 0)) %>%
          as.data.frame()
        
        params %<>%
          mutate(latency=replace(latency, ID == thisParticipant & crowded == thisCondition & ring == thisRing, NaN)) %>%
          as.data.frame()
        
        params %<>%
          mutate(precision=replace(precision, ID == thisParticipant & crowded == thisCondition & ring == thisRing, NaN)) %>%
          as.data.frame()
      }
      
      params %<>%
        mutate(val=replace(val, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$val)) %>%
        as.data.frame()
      
      params %<>%
        mutate(valGuessing=replace(valGuessing, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$valGuessing)) %>%
        as.data.frame()
      
      params %<>%
        mutate(pLRtest=replace(pLRtest, ID == thisParticipant & crowded == thisCondition & ring == thisRing, theseParams$pLRtest)) %>%
        as.data.frame()
    }
  }
}
write.csv(params, paste0('modelOutput/CourtneyParams',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.csv'),row.names = F)


params$ring %<>% as.factor

########################
###Plots with Density###
########################


for(thisParticipant in unique(params$ID)){
  for(thisCondition in unique(params$crowded)){
    for(thisRing in unique(params$ring)){
      thisEfficacy <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(efficacy)
      thisLatency <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(latency)
      thisPrecision <- thisEfficacy <- params %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing) %>% pull(precision)
      
      theseErrors <- allErrors %>% filter(ID == thisParticipant & crowded == thisCondition & ring == thisRing)
      print(paste0('Participant: ', thisParticipant, '. Ring: ', thisRing, '. Condition: ', thisCondition,'. N = ', nrow(theseErrors)))
      minError <- theseErrors %>% pull(SPE) %>% min
      maxError <- theseErrors %>% pull(SPE) %>% max
      thisRange <- seq(minError,maxError,.1)
      
      theseDensities <- data.frame(SPE = thisRange, density = dnorm(thisRange, thisLatency, thisPrecision))
      if(any(is.nan(theseDensities$density))){
        print(theseDensities)
      }
      
      thisPlot <- ggplot(theseErrors, aes(x=SPE))+
        geom_histogram(binwidth = 1)+
        geom_line(data = theseDensities, aes(x = SPE, y=density*nrow(theseErrors)))+ #scale density to histogram with density * N * binwidth
        scale_y_continuous(sec.axis = sec_axis(~./nrow(theseErrors), name = 'Density'))+
        labs(y = 'Frequency')
      
      thisFileName <- paste0('modelOutput/Plots/Courtney/',thisCondition,'/',thisRing,'/',thisParticipant,'-',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.png')
      ggsave(filename = thisFileName, thisPlot,width = 16, height = 9)
    }
  }
}

