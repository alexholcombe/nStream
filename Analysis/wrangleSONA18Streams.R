library(ggplot2)

rm(list=ls()) #remove all variables in workspace?

setwd('~/gitCode/nStream/')  #Charlie-specific
#source('../ggplotElements.R') #charlie-specific

source('ggplotElements.R')
theme_set(theme_apa(base_size = 20)) 

checkPositions = F
eyetracking = F

plots <- T #if true, create plots
if(plots){
  savePlots <- F #save plots?
}

saveIndividualTSV <- T #Save data files?
saveAllErrorsTSV <- F

bootstrapPredictions <- F #Should we bootstrap means and CIs for model predictions?
nRepetitions <- 1000 #number of bootstrap repetitions

dataPath <- 'rawData/' #'rawData/' for 2 vs 8


pracTrials <- 0
maxTrials <- 180 #one participant had the eyetracker crash and ended up doing ~325 trials

#incomplete and bugged attempts, drop these
#JH2 couldn't be eyetracked, blinked 3-4 times per trial
#OS3, EG11 and PC13 didn't complete the experiment because the eyetracker wasn't detecting their eyes
dropThese <- c('JH2')

#groups <- c('2vs8','End6Strm82msSOA','Ex6Strm82msSOA')

group = 'crowdingTest'

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

if(saveIndividualTSV){
  if(!group %in% list.dirs('wrangledData/', full.names=F)){
    dir.create(paste0('wrangledData/',group))
  }
}


files <- list.files(pattern = '.txt$', path = dataPath, full.names = T)
print(files)


widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 59 #cm
pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180)

if(eyetracking){
  eyetrackerFiles <- list.files(path = 'rawData/18Streams/Eyetracker',full.names = T)  
}

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
  ID <- strsplit(dataset, '(?<=ata)//|_(?=[0-9])', perl=T)[[1]][2] #split the string at a forward slash preceded by the numeral 8 or an underscore followed by any digit
  if(!ID %in% dropThese){
    #group <- strsplit(dataset,'/')[[1]][2]
    print(group)
    print(ID)
    IDs <- c(IDs, ID)
    #Some date formatting
    dateString <- strsplit(strsplit(dataset, '_(?=[0-9])', perl=T)[[1]][2],'')[[1]]
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
    
    
    dataSets[[group]][[ID]][[as.character(dateString)]][['data']] <- temp
  }
}

allErrors <- data.frame(exp = character(totalRows), 
                        crowded = character(totalRows), 
                        error = numeric(totalRows), 
                        ID = character(totalRows),
                        ring = numeric(totalRows), 
                        stringsAsFactors = F)

startRow <- 1

for(group in names(dataSets)){
  for(participant in names(dataSets[[group]])){
    for(dateString in names(dataSets[[group]][[participant]])){
      print(participant)
      
      nInAlphabeticalOrder <- which(IDs==participant) #parameter estimates from MM are in alphabetical order, so this lets me select the appropriate params
      
            
      temp <- dataSets[[group]][[participant]][[dateString]][['data']]
      
      temp$responsePos <- temp$cuePos0+temp$responsePosRelative0
      
      
      if(checkPositions){
        matchCue <- c()
        matchResponsePosRelative <- c()
        for(row in 1:nrow(temp)){
          streamCol <- paste0('streamLtrSequence', temp$cuedStream0[row])
          splitStream <- strsplit(temp[,streamCol], '')
          thisCueMatch <- temp$cuePos0 == which(splitStream == temp$answer0[row])
          thisSPE <- which(splitStream == temp$resp0) - which(splitStream == temp$answer0[row])
          thisSPEMatch <- temp$responsePosRelative0 == thisSPE
          matchCue <- c(matchCue, thisCueMatch)
          matchResponsePosRelative <- c(matchResponsePosRelative, thisSPEMatch)
        }
        print(all(matchResponsePosRelative))
        print(all(matchCue))
      }
      
      if(eyetracking){
        thisEyetrackerFile <- eyetrackerFiles[grep(paste0('.*',participant,'.*'), eyetrackerFiles)]
        theseFixations <- read.table(thisEyetrackerFile, sep='\t', stringsAsFactors = F, header = T)
      
        theseFixations$CURRENT_FIX_X_DEG <- theseFixations$CURRENT_FIX_X/pixelsPerDegree
        theseFixations$CURRENT_FIX_Y_DEG <- theseFixations$CURRENT_FIX_Y/pixelsPerDegree
        
        temp$fixationReject <- FALSE
        theseFixations$fixationDistance <- numeric(nrow(theseFixations))
        
        for(index in unique(theseFixations$TRIAL_INDEX)){ #If the trial has multiple fixations, compare each fixation to the initial fixation and reject that trial if it falls outside of a 1º radius circle centered on the initial fix
          
          theseFixationsThisTrial = which(theseFixations$TRIAL_INDEX==index)
          nFixationsThisTrial <- length(theseFixationsThisTrial)
          
          if(nFixationsThisTrial>1){
            
            initialFixationX = theseFixations$CURRENT_FIX_X_DEG[theseFixationsThisTrial[1]]
            initialFixationY = theseFixations$CURRENT_FIX_Y_DEG[theseFixationsThisTrial[1]]
            
            for(thisFixationRow in theseFixationsThisTrial){ #iterate over DF rows rather than fixations
              if(thisFixationRow == theseFixationsThisTrial[1]){
                #skip the first fixation,
              } else {
                
                xVector <- theseFixations$CURRENT_FIX_X_DEG[thisFixationRow] - initialFixationX
                yVector <- theseFixations$CURRENT_FIX_Y_DEG[thisFixationRow] - initialFixationY
                
                fixationDistance <- sqrt(xVector^2 + yVector^2)
                
                theseFixations$fixationDistance[thisFixationRow] <- fixationDistance
                
                if(fixationDistance>=criterion){
                  if(!theseFixations$CURRENT_FIX_BLINK_AROUND[thisFixationRow] %in% c('BEFORE','AFTER') ){
                    if(index<=220){
                      temp$fixationReject[index] <- TRUE
                      #print(fixationDistance)
                      #print(index)
                    }
                  }
                }
              }
            }
          }
        }
        
        if(mean(temp$fixationReject)>.4){ #Don't add their data if >2/5ths of the trials were rejected
          next
        }
      }
      
      
      tempSkewNStreams <- aggregate(responsePosRelative0~cuedStream0, temp, skew)
      
      tempSkewTotal <- skew(temp$responsePosRelative0)
      
      #Create densities for the SPE with fixation rejections removed
      
      if(plots){
        
        
        if(bootstrapPredictions){
          nEight <- length(which(temp$nStreams==8))
          nTwo <- length(which(temp$nStreams==2))
          
          eightB <- data.frame(bootStrappedPredictions = numeric(nEight*nRepetitions))
          twoB <- data.frame(bootStrappedPredictions = numeric(nTwo*nRepetitions))
          
          for(repetition in 1:nRepetitions){
            eight <- round(rnorm(nEight, eighteenStreamsLatency,eighteenStreamsPrecision)/83.25)
            eightRows <- (repetition*nEight-nEight+1):(repetition*nEight)
            
            two <- round(rnorm(nTwo, twoStreamsLatency,twoStreamsPrecision)/83.25)
            twoRows <- (repetition*nTwo-nTwo+1):(repetition*nTwo)
            
            eightB$bootStrappedPredictions[eightRows] <- eight
            twoB$bootStrappedPredictions[twoRows] <- two
          }
          bootstrapResults <- rbind(eightB, twoB)
          bootstrapResults$nStreams <- 0
          bootstrapResults$nStreams[1:nrow(twoB)] <- 2
          bootstrapResults$nStreams[nrow(twoB)+1:nrow(eightB)] <- 8
        }
        
        cueOffsets = c(3,7,11.5)
        
        temp$eccentricity <- cueOffsets[temp$ring+1]  
        
        tempPlot <- ggplot(temp, aes(x=responsePosRelative0))+
          geom_histogram(binwidth = 1)+
          scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),4))+
          #geom_text(x = 4, y=30, label=paste0('skew =', round(tempSkewTotal,2)))+
          geom_vline(xintercept = 0, linetype = 'dashed')+
          #stat_summary(data=bootstrapResults, aes(x=bootstrapPredictions), fun.y=mean, geom='line')+
          facet_wrap(~eccentricity)+
          labs(x = 'Serial Position Error',
               y='Count',
               title = paste0('participant: ', participant, ' Exp: ', group))
        
        show(tempPlot)
        
        three <- temp[temp$ring==0,]
        seven <- temp[temp$ring==1,]
        eleven <- temp[temp$ring==2,]
        
        endRow <- startRow + nrow(temp) -1
        
        print(startRow)
        print(endRow)
        
        allErrors$exp[startRow:endRow] <- temp$experimentPhase
        allErrors$ring[startRow:endRow] <- temp$ring
        allErrors$ID[startRow:endRow] <- participant
        allErrors$error[startRow:endRow] <- temp$responsePosRelative0
        allErrors$crowded[startRow:endRow] <- temp$crowded
        
        if(eyetracking){
          allErrors$fixationReject[startRow:endRow] <- twoStreams$fixationReject
        }
        
        # plotByStream <- ggplot(temp[!temp$fixationReject,], aes(x=responsePosRelative0))+
        #   geom_histogram(binwidth = 1)+
        #   scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),1))+
        #   geom_vline(xintercept = 0, linetype = 'dashed')+
        #   labs(x = 'Serial Position Error',
        #        y='Count',
        #        title = paste0('participant: ', participant, ' Exp: ', group))+
        #   facet_wrap(~whichStream0)
        
        if(savePlots){
          ggsave(paste0('plots/',group,'/',participant,'.png'),tempPlot, width = 30, height = 30, units = 'cm')
          #ggsave(paste0('plots/',group,'/',participant,'ByStream.png'),plotByStream, width = 20, height = 20, units = 'cm')
        }
        
        dataSets[[group]][[participant]][[dateString]][['plotAll']] <- tempPlot
        #dataSets[[group]][[participant]][[dateString]][['plotByStreams']] <- plotByStream
      }
      
      
      if(saveIndividualTSV){
        if(eyetracking){
          write.table(twoStreams[!twoStreams$fixationReject,], paste0('wrangledData/',group,'/twoStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
          write.table(eighteenStreams[!eighteenStreams$fixationReject,], paste0('wrangledData/',group,'/eighteenStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
        } else {
          write.table(three, paste0('wrangledData/', group,'/Three/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
          write.table(seven, paste0('wrangledData/', group,'/Seven/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
          write.table(eleven, paste0('wrangledData/', group,'/Eleven/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
        }  
      }
      dataSets[[group]][[participant]][[dateString]][['skewStreams']] <- tempSkewNStreams
      dataSets[[group]][[participant]][[dateString]][['skew']] <- tempSkewTotal 
    }
  }
}

#allErrors$exp <- factor(allErrors$exp, levels = c('twoStreams','eighteenStreams','End6Strm82msSOA','Ex6Strm82msSOA'))

#Drop any unused rows. These were allocated for participant(s) who had too many fixation rejections
allErrors <- allErrors[!allErrors$ID=='',]
allErrors <- allErrors[allErrors$button %in% c(0,2),]

if(saveAllErrorsTSV){
  write.table(allErrors, file = 'Analysis/allErrors.txt', sep='\t', row.names = F, col.names = T)
}

#plus or minus one SPE for kim
kim <- aggregate(error~ID, data = allErrors[allErrors$condition=='twoStreams',], FUN = function(x) length(which(x>=-1 & x<=1))/length(x))

totalPlot <- ggplot(allErrors[!allErrors$fixationReject,], aes(x=error))+
  geom_histogram(binwidth = 1)+
  labs(y = 'Count', x = 'Serial Position Error')+
  facet_wrap(~ID+condition, ncol = 2)+
  geom_vline(xintercept = 0, linetype = 'dashed')

#show(totalPlot) 

if(plots){
  if(savePlots){
    ggsave(paste0('plots/',group,'/Combined.png'),totalPlot, width = 20, height = 20, units = 'cm')
  }
}
