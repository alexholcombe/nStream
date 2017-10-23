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


# #Manually concatenate the split data files. The unsplit files are now stored in rawData/split
# IK4 <- read.table('rawData/IK4_04May2017_14-35.txt', sep = '\t', header = T, stringsAsFactors = F)
# IK42 <- read.table('rawData/IK42_04May2017_15-06.txt', sep = '\t', header = T, stringsAsFactors = F)
# 
# IK4 <- rbind(IK4,IK42)
# 
# LT5 <- read.table('rawData/LT5_04May2017_15-52.txt', sep='\t', header = T, stringsAsFactors = F)
# LT52 <- read.table('rawData/LT52_04May2017_16-21.txt', sep='\t', header =T, stringsAsFactors = F)
# 
# LT5 <- rbind(LT5,LT52)
# 
# JA8 <- read.table('rawData/JA8_08May2017_11-33.txt',sep='\t', header =T, stringsAsFactors = F)
# JA82 <- read.table('rawData/JA8_08May2017_11-40.txt',sep='\t', header =T, stringsAsFactors = F)
# 
# JA8 <- rbind(JA8,JA82)
# 
# write.table(LT5,file = 'rawData/LT5_04May2017_15-52CONCATENATED.txt', col.names = T, sep='\t', row.names = F)
# write.table(IK4,file = 'rawData/IK4_04May2017_14-35CONCATENATED.txt', col.names = T, sep='\t', row.names = F)
# write.table(JA8,file = 'rawData/JA8_08May2017_11-33CONCATENATED.txt',col.names = T,sep='\t', row.names = F)
# 
# 
# eyeIK4 <- read.table('rawData/Eyetracker/IK4.txt', sep='\t', header = T, stringsAsFactors = F)
# eyeIK42 <- read.table('rawData/Eyetracker/IK42.txt', sep='\t', header = T, stringsAsFactors = F)
# 
# eyeIK4 <- rbind(eyeIK4,eyeIK42)
# 
# write.table(eyeIK4, file = 'rawData/Eyetracker/IK4.txt', col.names = T, sep='\t', row.names = F)
# 
# eyeLT5 <- read.table('rawData/Eyetracker/LT5.txt', sep = '\t', header = T, stringsAsFactors  = F)
# eyeLT52 <- read.table('rawData/Eyetracker/LT52.txt', sep = '\t', header = T,stringsAsFactors =  F)
# 
# eyeLT5 <- rbind(eyeLT5,eyeLT52)
# 
# write.table(eyeLT5, file = 'rawData/Eyetracker/LT5.txt', col.names = T, sep='\t', row.names = F)
#practice trials
pracTrials <- 1:20
maxTrials <- 321 #one participant had the eyetracker crash and ended up doing ~325 trials

#incomplete and bugged attempts, drop these
#MH1 ran the experiment on a bugged version of the code
#OS3, EG11 and PC13 didn't complete the experiment because the eyetracker wasn't detecting their eyes
dropThese <- c('AW1', 'DY1', 'AM3', 'YW2', 'FY4')

#groups <- c('2vs8','End6Strm82msSOA','Ex6Strm82msSOA')

group = 'SONA'

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

efficacy <- read.csv('modelOutput/CSV/Efficacy.csv')
latency <- read.csv('modelOutput/CSV/Latency.csv')
precision <- read.csv('modelOutput/CSV/Precision.csv')


files <- list.files(pattern = '^[A-Z][A-Z][0-9].*\\.txt$', path = dataPath, full.names = T)
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

eyetrackerFiles <- list.files(path = 'rawData/Eyetracking',full.names = T)

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
  ID <- strsplit(dataset, '(?<=a)/|_(?=[0-9])', perl=T)[[1]][2] #split the string at a forward slash preceded by the numeral 8 or an underscore followed by any digit
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

allErrors <- data.frame(exp = character(totalRows), condition = character(totalRows), error = numeric(totalRows), ID = character(totalRows), fixationReject = logical(totalRows), button = numeric(totalRows), stringsAsFactors = F)

startRow <- 1

for(group in names(dataSets)){
  for(participant in names(dataSets[[group]])){
    for(dateString in names(dataSets[[group]][[participant]])){
      print(participant)
      
      nInAlphabeticalOrder <- which(IDs==participant) #parameter estimates from MM are in alphabetical order, so this lets me select the appropriate params
      
      
      #Mixture Modelled parameter estimates
      twoStreamsLatency <- latency$twoStreams[nInAlphabeticalOrder]
      twoStreamsPrecision <- precision$eightStreams[nInAlphabeticalOrder]
      
      eightStreamsLatency <- latency$eightStreams[nInAlphabeticalOrder]
      eightStreamsPrecision <- precision$eightStreams[nInAlphabeticalOrder]
      
            
      temp <- dataSets[[group]][[participant]][[dateString]][['data']]
      
      temp$responsePos <- temp$cuePos0+temp$responsePosRelative0
      
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
      
      tempSkewNStreams <- aggregate(responsePosRelative0~whichStream0, temp, skew)
      
      tempSkewTotal <- skew(temp$responsePosRelative0)
      
      #Create densities for the SPE with fixation rejections removed
      
      if(plots){
        
        
        if(bootstrapPredictions){
          nEight <- length(which(temp$nPreCueStreams==8))
          nTwo <- length(which(temp$nPreCueStreams==2))
          
          eightB <- data.frame(bootStrappedPredictions = numeric(nEight*nRepetitions))
          twoB <- data.frame(bootStrappedPredictions = numeric(nTwo*nRepetitions))
          
          for(repetition in 1:nRepetitions){
            eight <- round(rnorm(nEight, eightStreamsLatency,eightStreamsPrecision)/83.25)
            eightRows <- (repetition*nEight-nEight+1):(repetition*nEight)
            
            two <- round(rnorm(nTwo, twoStreamsLatency,twoStreamsPrecision)/83.25)
            twoRows <- (repetition*nTwo-nTwo+1):(repetition*nTwo)
            
            eightB$bootStrappedPredictions[eightRows] <- eight
            twoB$bootStrappedPredictions[twoRows] <- two
          }
          bootstrapResults <- rbind(eightB, twoB)
          bootstrapResults$nPreCueStreams <- 0
          bootstrapResults$nPreCueStreams[1:nrow(twoB)] <- 2
          bootstrapResults$nPreCueStreams[nrow(twoB)+1:nrow(eightB)] <- 8
        }

          
        tempPlot <- ggplot(temp[!temp$fixationReject,], aes(x=responsePosRelative0))+
          geom_histogram(binwidth = 1)+
          scale_x_continuous(breaks=seq(min(temp$responsePosRelative0), max(temp$responsePosRelative0),4))+
          #geom_text(x = 4, y=30, label=paste0('skew =', round(tempSkewTotal,2)))+
          geom_vline(xintercept = 0, linetype = 'dashed')+
          #stat_summary(data=bootstrapResults, aes(x=bootstrapPredictions), fun.y=mean, geom='line')+
          facet_wrap(~nPreCueStreams)+
          labs(x = 'Serial Position Error',
               y='Count',
               title = paste0('participant: ', participant, ' Exp: ', group))
        
        show(tempPlot)
        
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
      
      streamColumns <- grep('streamLtrSequence', colnames(temp))
      
      twoStreams <- temp[temp$nPreCueStreams==2,]
      eightStreams <- temp[temp$nPreCueStreams==8,]
      
      
      endRow <- startRow + nrow(twoStreams) -1
      
      print(startRow)
      print(endRow)
      
      allErrors$condition[startRow:endRow] <- 'twoStreams'
      allErrors$ID[startRow:endRow] <- participant
      allErrors$error[startRow:endRow] <- twoStreams$responsePosRelative0
      allErrors$fixationReject[startRow:endRow] <- twoStreams$fixationReject
      allErrors$button[startRow:endRow] <- twoStreams$button0
      
      startRow <- endRow+1
      
      endRow <- startRow + nrow(eightStreams)-1
      
      allErrors$condition[startRow:endRow] <- 'eightStreams'
      allErrors$ID[startRow:endRow] <- participant
      allErrors$error[startRow:endRow] <- eightStreams$responsePosRelative0
      allErrors$fixationReject[startRow:endRow] <- eightStreams$fixationReject
      allErrors$button[startRow:endRow] <- eightStreams$button0
      
      startRow <- endRow + 1
      
      if(saveIndividualTSV){
        write.table(twoStreams[!twoStreams$fixationReject,], paste0('wrangledData/',group,'/twoStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
        write.table(eightStreams[!eightStreams$fixationReject,], paste0('wrangledData/',group,'/eightStreams/',participant,'.txt'), sep='\t', col.names = T, row.names = F)
      }
      dataSets[[group]][[participant]][[dateString]][['skewStreams']] <- tempSkewNStreams
      dataSets[[group]][[participant]][[dateString]][['skew']] <- tempSkewTotal 
    }
    print(mean(temp$fixationReject))
  }
}

#allErrors$exp <- factor(allErrors$exp, levels = c('twoStreams','eightStreams','End6Strm82msSOA','Ex6Strm82msSOA'))

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
  facet_wrap(~condition+button)+
  geom_vline(xintercept = 0, linetype = 'dashed')

show(totalPlot)

if(plots){
  if(savePlots){
    ggsave(paste0('plots/',group,'/Combined.png'),totalPlot, width = 20, height = 20, units = 'cm')
  }
}
