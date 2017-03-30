library(ggplot2)

rm(list=ls())

setwd('~/gitcode/nStream/')
source('~/R_utils/ggplotElements.R')

plots <- T #if true, create plots
savePlots <- T #save plots?


dataPath <- 'rawData/'

groups <- c('2vs8') #c('End6Strm82msSOA','Ex6Strm115msSOA')

if(!'plots' %in% list.dirs(full.names=F)) dir.create('plots')

files <- c()

for(group in groups){
  groupFiles <- list.files(path=paste0(dataPath,group), pattern='.txt',full.names=T)
  groupFiles <- groupFiles[grep('Hubert',groupFiles, invert=T)]
  files <- c(files, groupFiles)
}



skew <- function(x){
  denom <- (sd(x,na.rm=T)^3)*(length(which(!is.na(x)))-1)*(length(which(!is.na(x)))-2)
  deviation <- x[!is.na(x)] - mean(x,na.rm=T)
  numer <- sum(deviation^3)*length(which(!is.na(x)))
  numer/denom
}

dataSets <- list()

#The first two datasets (dates and times in datafile's file names) from 7/2 are 6 streams at 115ms per item. The distributions look symmetric
#The next two are 8 streams at 82.35ms per item. This was intended to induce skew. The distributions are still symmetric.
#The next two (8/2) are endogenously cued by red pixel in fixation stimulus. 6 streams at 82.35ms. Changed to 6 streams because endogenous cue was too ambiguous with 8 streams
#North-East cue is a little off in the endogenous cuing expt as of 8/2/2017


for(dataset in files){
  ID <- strsplit(dataset, '(?<=A|s)/|_(?=[0-9])', perl=T)[[1]][2] #split the string at a forward slash preceded by the letter A or an underscore followed by any digit
  cue <- strsplit(dataset, '(?<=A)/|_(?=[0-9])', perl=T)[[1]][1]
  cue <- strsplit(cue, '(?<=rawData)/|[0-9]', perl=T)[[1]][2]
  
  if(cue == 'End'){
    cue <- 'Endogenous'
  } else {
      cue <- 'Exogenous'
    }
  
  #Some date formatting
  dateString <- strsplit(strsplit(dataset, '_(?=[0-9])', perl=T)[[1]][2],'')[[1]]
  day <- paste0(dateString[1:2],collapse='')
  month <- paste0(dateString[3:5],collapse='')
  year <- paste0(dateString[6:9],collapse='')
  dateString <- as.Date(paste(day,month,year, sep=':'),format='%d:%b:%Y') 
  
  temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)
  
  #nStreams <- length(grep('streamLtrSequence', colnames(temp))) 
  
  
  dataSets[[ID]][[dataString]][['data']] <- temp
}

allExogenousErrors <- c()
allEndogenousErrors <- c()

for(participant in names(dataSets)){
    for(dateString in names(dataSets[[participant]])){
      temp <- dataSets[[participant]][[dateString]][['data']]
      
      temp$correctPos <- NA
      for(rowN in 1:nrow(temp)){
        temp$correctPos[rowN] <- which(strsplit(temp[rowN, paste0('streamLtrSequence', temp$whichStream0[rowN])],'')[[1]]==gsub(' ','',temp$answer0[rowN]))-1
        temp$responsePos[rowN] <- which(strsplit(temp[rowN, paste0('streamLtrSequence', temp$whichStream0[rowN])],'')[[1]]==gsub(' ','',temp$resp0[rowN]))-1
      }
      tempSkewStreams <- aggregate(responsePosRelative0~whichStream0, temp, skew)
      tempSkewTotal <- skew(temp$responsePosRelative0)
      if(plots){
          tempPlot <- ggplot(temp, aes(x=responsePosRelative0))+
            geom_histogram(binwidth = 1)+
            scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
            #geom_text(x = 4, y=30, label=paste0('skew =', round(tempSkewTotal,2)))+
            geom_vline(xintercept = 0, linetype = 'dashed')+
            labs(x = 'Serial Position Error',
                 y='Count',
                 title = paste0('participant: ', participant, ' nStreams: ', nStreams))+
            apatheme
          
          show(tempPlot)
          
          plotByStream <- ggplot(temp, aes(x=responsePosRelative0))+
            geom_histogram(binwidth = 1)+
            scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
            geom_text(data=tempSkewStreams, x = 10, y=10, aes(label=paste0('skew =', round(responsePosRelative0,2))))+
            facet_wrap(~whichStream0)
          if(savePlots){
            ggsave(paste0('plots/',participant,nStreams,cue,'.png'),tempPlot, width = 20, height = 20, units = 'cm')
            ggsave(paste0('plots/',participant,nStreams,cue,'ByStream.png'),plotByStream, width = 20, height = 20, units = 'cm')
        }
       
        dataSets[[participant]][[nStreams]][[cue]][['plotAll']] <- tempPlot
        dataSets[[participant]][[nStreams]][[cue]][['plotByStreams']] <- plotByStream
      }
      
      streamColumns <- grep('streamLtrSequence', colnames(temp))
      
      #A three dimensional array. The dimensions represent trial, stream, and type of error (temporal or spatial)
      #Maybe extend this to an array that represents all possibe combinations of temporal and spatial error for a given trial
  
      responsePos <- array(NA, dim = c(nrow(temp), length(streamColumns), 2))
      responsePos[,,2] <- matrix(1:length(streamColumns), nrow = nrow(temp), ncol=length(streamColumns), byrow=T)
      
      #For each trial, where did the response fall in each stream?
      for(column in 1:length(streamColumns)){
        x <- t(sapply(strsplit(temp[,streamColumns[column]],''), function(x) x))
        for(rowN in 1:nrow(temp)){
          response <- gsub(' ' ,'',temp$resp0[rowN])
          stream <- x[rowN,]
          temporalPos <- which(stream==response) - temp$correctPos[rowN]
          responsePos[rowN,column,1] <- temporalPos
          responsePos[rowN,column,2] <- responsePos[rowN,column,2] - temp$whichStream0[rowN]
        }
        
      }
      
      cueForFolder <- c('Ex','End')[(cue=='Endogenous')+1]
      
      if(cue == 'Exogenous' & nStreams=='6Streams'){
        SOAString <- '115msSOA'
      } else { 
        SOAString <- '82msSOA'  
      }

      twoStreams <- temp[temp$streamsPerRing==2,]
      eightStreams <- temp[temp$streamsPerRing==8,]
      
      write.table(twoStreams, paste0('wrangledData/twoStreams/',cueForFolder, nStreams, SOAString,'/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
      write.table(eightStreams, paste0('wrangledData/eightStreams/',cueForFolder, nStreams, SOAString,'/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
      
      dataSets[[participant]][[nStreams]][[cue]][['skewStreams']] <- tempSkewStreams
      dataSets[[participant]][[nStreams]][[cue]][['skew']] <- tempSkewTotal 
  }
}
