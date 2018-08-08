library(ggplot2)

rm(list=ls())

setwd('~/gitCode/nStream/')
source('../ggplotElements.R')

plots <- T #if true, create plots
if(plots){
  savePlots <- T #save plots?
}

saveCSV = T


dataPath <- 'rawData/'

groups <- c('End6Strm82msSOA','Ex6Strm82msSOA')

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
  ID <- strsplit(dataset, '(?<=8|A)/|_(?=[0-9])', perl=T)[[1]][2] #split the string at a forward slash preceded by the numeral 8 or an underscore followed by any digit
  group <- strsplit(dataset,'/')[[1]][2]
  print(group)
  print(ID)
  #Some date formatting
  dateString <- strsplit(strsplit(dataset, '_(?=[0-9])', perl=T)[[1]][2],'')[[1]]
  day <- paste0(dateString[1:2],collapse='')
  month <- paste0(dateString[3:5],collapse='')
  year <- paste0(dateString[6:9],collapse='')
  dateString <- as.Date(paste(day,month,year, sep=':'),format='%d:%b:%Y') 
  
  temp <- read.table(dataset,sep='\t',header=T, stringsAsFactors = F)
  
  #nStreams <- length(grep('streamLtrSequence', colnames(temp))) 
  
  
  dataSets[[group]][[ID]][[as.character(dateString)]][['data']] <- temp
}

allExogenousErrors <- c()
allEndogenousErrors <- c()

for(group in names(dataSets)){
	for(participant in names(dataSets[[group]])){
	    for(dateString in names(dataSets[[group]][[participant]])){
	      temp <- dataSets[[group]][[participant]][[dateString]][['data']]
	      
	      temp$correctPos <- NA
	      for(rowN in 1:nrow(temp)){
	        temp$correctPos[rowN] <- which(strsplit(temp[rowN, paste0('streamLtrSequence', temp$whichStream0[rowN])],'')[[1]]==gsub(' ','',temp$answer0[rowN]))-1
	        temp$responsePos[rowN] <- which(strsplit(temp[rowN, paste0('streamLtrSequence', temp$whichStream0[rowN])],'')[[1]]==gsub(' ','',temp$resp0[rowN]))-1
	      }
        
	      if(group == '2vs8'){
	        tempSkewNStreams <- aggregate(responsePosRelative0~streamsPerRing, temp, skew)
	        tempSkewTotal <- skew(temp$responsePosRelative0)
	      }
	      
	      if(plots){
            tempPlot <- ggplot(temp, aes(x=responsePosRelative0))+
              geom_histogram(binwidth = 1)+
              scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
              #geom_text(x = 4, y=30, label=paste0('skew =', round(tempSkewTotal,2)))+
              geom_vline(xintercept = 0, linetype = 'dashed')+
              labs(x = 'Serial Position Error',
                   y='Count',
                   title = paste0('participant: ', participant, ' Exp: ', group))+
              apatheme
            
            show(tempPlot)
            
	          if(group != '2vs8'){
	            plotByStream <- ggplot(temp, aes(x=responsePosRelative0))+
	              geom_histogram(binwidth = 1)+
	              scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
	              #geom_text(data=tempSkewStreams, x = 10, y=10, aes(label=paste0('skew =', round(responsePosRelative0,2))))+
	              facet_wrap(~whichStream0)
	          } else {
	            plotByStream <- ggplot(temp, aes(x=responsePosRelative0))+
	              geom_histogram(binwidth = 1)+
	              scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
	              #geom_text(data=tempSkewStreams, x = 10, y=10, aes(label=paste0('skew =', round(responsePosRelative0,2))))+
	              facet_wrap(~streamsPerRing)
	          }
            if(savePlots){
              ggsave(paste0('plots/',group,'/',participant,'.png'),tempPlot, width = 20, height = 20, units = 'cm')
              ggsave(paste0('plots/',group,'/',participant,'ByStream.png'),plotByStream, width = 20, height = 20, units = 'cm')
            }
            
            dataSets[[group]][[participant]][[dateString]][['plotAll']] <- tempPlot
            dataSets[[group]][[participant]][[dateString]][['plotByStreams']] <- plotByStream
	      }
	      
	      streamColumns <- grep('streamLtrSequence', colnames(temp))
	      
	      #A three dimensional array. The dimensions represent trial, stream, and type of error (temporal or spatial)
	      #Maybe extend this to an array that represents all possibe combinations of temporal and spatial error for a given trial
	      
	# 
	#       responsePos8Streams <- array(NA, dim = c(length(which(temp$streamsPerRing==8)), 8, 2))
	#       responsePos8Streams[,,2] <- matrix(1:length(streamColumns), nrow = length(which(temp$streamsPerRing==8)), ncol=8, byrow=T)
	# 
	#       responsePos2Streams <- array(NA, dim = c(length(which(temp$streamsPerRing==2)), 2, 2))
	#       responsePos2Streams[,,2] <- matrix(1:length(streamColumns), nrow = length(which(temp$streamsPerRing==2)), ncol=2, byrow=T)      
	#       
	#       
	#       thisTwoStreamRow <- 0 #Iterates through the rows of the 2 stream array
	#       thisEightStreamRow <- 0
	#       
	#       #For each trial, where did the response fall in each stream?
	#       for(column in 1:length(streamColumns)){
	#         x <- t(sapply(strsplit(temp[,streamColumns[column]],''), function(x) x))
	#         for(rowN in 1:nrow(temp)){
	#           
	#           nStreamThisRow <- temp$streamsPerRing[rowN] #How many streams on this row?
	#           
	#           if(column == 1){ #Only increase the counters if its the first column, there's 8 iterations per row
	#             if(nStreamThisRow == 2){
	#               thisTwoStreamRow = thisTwoStreamRow+1
	#             } else if(nStreamThisRow == 8){
	#               thisEightStreamRow = thisEightStreamRow+1
	#             }
	#           }
	#           
	#           response <- gsub(' ' ,'',temp$resp0[rowN])
	#           
	#           stream <- x[rowN,]
	#           temporalPos <- which(stream==response) - temp$correctPos[rowN]
	#           if(nStreamThisRow==2 & column<3){
	#             
	#             responsePos2Streams[thisTwoStreamRow,column,1] <- temporalPos
	#             responsePos2Streams[thisTwoStreamRow,column,2] <- responsePos2Streams[thisTwoStreamRow,column,2] - temp$whichStream0[rowN]
	#           } else if(nStreamThisRow==8) {
	#           
	#             responsePos8Streams[thisEightStreamRow,column,1] <- temporalPos
	#             responsePos8Streams[thisEightStreamRow,column,2] <- responsePos8Streams[thisEightStreamRow,column,2] - temp$whichStream0[rowN]
	#           }
	#         }
	#         
	#       }
	      ###########################################################################################################
	      #### nTrials kludge. Dropping the first 20 ### 
	      ###########################################################################################################
	      
	      tempKludged <- temp[21:nrow(temp),]
	      print(paste0('wrangledData/',group,'/',participant,'.txt'))
	      if(group=='2vs8'){
	      	twoStreams <- tempKludged[tempKludged$streamsPerRing==2,]
	      	eightStreams <- tempKludged[tempKludged$streamsPerRing==8,]
	      	if(saveCSV){
	        	write.table(twoStreams, paste0('wrangledData/twoStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
	        	write.table(eightStreams, paste0('wrangledData/eightStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
	     	 }
	      } else {
	      	if(saveCSV){
	      		write.table(tempKludged, paste0('wrangledData/',group,'/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
	      	}
	      }
	      
	      if(group == '2vs8'){
	        dataSets[[group]][[participant]][[dateString]][['skewStreams']] <- tempSkewNStreams
	        dataSets[[group]][[participant]][[dateString]][['skew']] <- tempSkewTotal   
	      }
	      
	  	}
    }
}
