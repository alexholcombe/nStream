library(ggplot2)

rm(list=ls()) #remove all variables in workspace?

setwd('~/gitCode/nStream/')  #Charlie-specific
#source('../ggplotElements.R') #charlie-specific

source('ggplotElements.R')
theme_set(theme_apa(base_size = 20)) 

plots <- T #if true, create plots
if(plots){
  savePlots <- F #save plots?
}

saveIndividualTSV <- T #Save data files?
saveAllErrorsTSV <- T

bootstrapPredictions <- F #Should we bootstrap means and CIs for model predictions?
nRepetitions <- 1000 #number of bootstrap repetitions

dataPath <- 'rawData'


if(plots){
  if(savePlots){
    if(!'plots' %in% list.dirs(full.names=F)){
      dir.create('plots')
    }
    if(!group %in% list.dirs(path = 'plots', full.names = F)){
      dir.create(paste0('plots/',group))
    }
  }
}


ringNames <- c('one','two','three')

files <- list.files(pattern = '^[A-Z][A-Z]_.*\\.txt$', path = dataPath, full.names = T)
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
  print(ID)
  IDs <- c(IDs, ID)
  #Some date formatting
  dateString <- strsplit(strsplit(dataset, '_(?=[0-9])', perl=T)[[1]][2],'')[[1]]
  print(dateString)
  day <- paste0(dateString[1:2],collapse='')
  month <- paste0(dateString[3:5],collapse='')
  year <- paste0(dateString[6:9],collapse='')
  dateString <- as.Date(paste(day,month,year, sep=':'),format='%d:%b:%Y') 
  
  temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)
  print(nrow(temp))
  if(nrow(temp)==320){
    temp <- temp[-pracTrials,]
  } else if(nrow(temp)>320) {
    temp <- temp[-c(pracTrials,maxTrials:nrow(temp)),]
  }
  
  
  totalRows <- totalRows + nrow(temp)
  
  #nStreams <- length(grep('streamLtrSequence', colnames(temp))) 
  
  
  dataSets[[ID]][[as.character(dateString)]][['data']] <- temp
}

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
    
    temp$responsePos <- temp$cuePos0+temp$responsePosRelative0
    
    tempSkewNStreams <- aggregate(responsePosRelative0~crowded, temp, skew)
    
    tempSkewTotal <- skew(temp$responsePosRelative0)
    
    #Create densities for the SPE with fixation rejections removed
    
    if(plots){

        
      tempPlot <- ggplot(temp[], aes(x=responsePosRelative0))+
        geom_histogram(binwidth = 1)+
        scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),4))+
        geom_vline(xintercept = 0, linetype = 'dashed')+
        facet_wrap(~crowded)+
        labs(x = 'Serial Position Error',
             y='Count',
             title = paste0('participant: ', participant))
      
      show(tempPlot)
      
      if(savePlots){
        ggsave(paste0('plots/',participant,'.png'),tempPlot, width = 30, height = 30, units = 'cm')
        #ggsave(paste0('plots/',group,'/',participant,'ByStream.png'),plotByStream, width = 20, height = 20, units = 'cm')
      }
      
      dataSets[[participant]][[dateString]][['plotAll']] <- tempPlot
      #dataSets[[group]][[participant]][[dateString]][['plotByStreams']] <- plotByStream
    }
    
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
    print(startRow)
    print(endRow)
  

    allErrors$SPE[startRow:endRow] <- temp$responsePosRelative0
    
    allErrors$targetSP[startRow:endRow] <- temp$cuePos0
    
    allErrors$ID[startRow:endRow] <- participant
    print(unique(allErrors$ID))
    
    allErrors$crowded[startRow:endRow] <- temp$crowded
    
    allErrors$ring[startRow:endRow] <- temp$ring
      
    startRow <- endRow + 1
    
    if(saveIndividualTSV){
      for(condition in c('crowded','bouma')){
        for(ring in ringNames){
          thisData <- get(paste0(condition, ring))
          write.table(thisData, paste0('wrangledData/',condition,'/',ring,'/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
        }
      }
    }
    dataSets[[participant]][[dateString]][['skewStreams']] <- tempSkewNStreams
    dataSets[[participant]][[dateString]][['skew']] <- tempSkewTotal 
  }
}


#allErrors$exp <- factor(allErrors$exp, levels = c('crowded','bouma','End6Strm82msSOA','Ex6Strm82msSOA'))

if(saveAllErrorsTSV){
  write.table(allErrors, file = 'Analysis/allErrors.txt', sep='\t', row.names = F, col.names = T)
}

#plus or minus one SPE for kim
#kim <- aggregate(error~ID, data = allErrors[allErrors$condition=='crowded',], FUN = function(x) length(which(x>=-1 & x<=1))/length(x))

totalPlot <- ggplot(allErrors, aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(y = 'Count', x = 'Serial Position Error')+
  facet_wrap(~ID+condition+ring, nrow = 3)+
  geom_vline(xintercept = 0, linetype = 'dashed')

show(totalPlot)

if(plots){
  if(savePlots){
    ggsave(paste0('plots/',group,'/Combined.png'),totalPlot, width = 20, height = 20, units = 'cm')
  }
}