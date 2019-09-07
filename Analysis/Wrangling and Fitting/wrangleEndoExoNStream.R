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

saveIndividualTSV <- F #Save data files?
saveAllErrorsTSV <- T

dataPath <- 'rawData/Endo-Exo nStreams'


#practice trials
pracTrials <- 1:20
maxTrials <- 240 #one participant had the eyetracker crash and ended up doing ~325 trials



#groups <- c('2vs8','End6Strm82msSOA','Ex6Strm82msSOA')

group = 'SONA'

files <- list.files(pattern = '^[A-Z][A-Z].*\\.txt$', path = dataPath, full.names = T)
print(files)


widthPix= 1280 #monitor width in pixels of Agosta
heightPix= 1024 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 53.0 #cm
pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180)

eyetrackerFiles <- list.files(path = 'rawData/Endo-Exo nStreams/Eyetracking',full.names = T)

criterion = 1 #Radius of a circle around the central point. Fixations outside of this circle result in rejected trials

skew <- function(x){
  denom <- (sd(x,na.rm=T)^3)*(length(which(!is.na(x)))-1)*(length(which(!is.na(x)))-2)
  deviation <- x[!is.na(x)] - mean(x,na.rm=T)
  numer <- sum(deviation^3)*length(which(!is.na(x)))
  numer/denom
}


dataSets <- list()



totalRows <- 0


IDs <- character()

dropThese = c()

for(dataset in files){
  ID <- strsplit(dataset, '(?<=s)/|(?<=[A-Z][A-Z])End|(?<=[A-Z][A-Z])Ex', perl=T)[[1]][2] #split the string at a forward slash preceded by the numeral 8 or an underscore followed by any digit
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
    
    
    if(grepl('CLEx', dataset)){
      cueType = 'exogenousRing'
      temp$cueType = cueType
    } else if(grepl('CLEnd', dataset)){
      cueType = 'endogenous'
      temp$cueType = cueType
    } else {
      cueType = temp$cueType[1]
    }
    
    
    if(nrow(temp)==240){
      temp <- temp[-pracTrials,]
    } else if(nrow(temp)>320) {
      temp <- temp[-c(pracTrials,maxTrials:nrow(temp)),]
    }
    
    
    totalRows <- totalRows + nrow(temp)
    
    #nStreams <- length(grep('streamLtrSequence', colnames(temp))) 
    
    
    dataSets[[ID]][[cueType]][[as.character(dateString)]][['data']] <- temp
  }
}

allErrors <- data.frame(
  experimentPhase = character(totalRows),
  trialnum = numeric(totalRows),
  subject = character(totalRows),
  task = character(totalRows),
  noisePercent = numeric(totalRows),
  targetLeftRightIfOne = character(totalRows),
  nStreams = numeric(totalRows),
  eccentricity = numeric(totalRows),
  polarAngle = numeric(totalRows),
  resp0 = character(totalRows),
  answer0 = character(totalRows),
  correct0 = numeric(totalRows),
  whichStream0 = numeric(totalRows),
  whichRespCue0 = numeric(totalRows),
  responsePosRelative0 = numeric(totalRows),
  streamLtrSequence0 = character(totalRows),
  streamLtrSequence1 = character(totalRows),
  streamLtrSequence2 = character(totalRows),
  streamLtrSequence3 = character(totalRows),
  streamLtrSequence4 = character(totalRows),
  streamLtrSequence5 = character(totalRows),
  timingBlips = numeric(totalRows),
  cueType = character(totalRows),
  cuePos0 = numeric(totalRows),
  responsePos = numeric(totalRows),
  fixationReject = logical(totalRows), 
  ID = character(totalRows),
  stringsAsFactors = F)

startRow <- 1

for(participant in names(dataSets)){
  for(cue in names(dataSets[[participant]])){
    for(dateString in names(dataSets[[participant]][[cue]])){
      print(participant)
      
      
      temp <- dataSets[[participant]][[cue]][[dateString]][['data']]
      
      temp$cuePos0 <- -999
      
      for(thisTrial in 1:nrow(temp)){
        cuedStream <- unlist(strsplit(temp[thisTrial,paste0('streamLtrSequence', temp$whichStream0[thisTrial])],''))
        cuePos <- which(cuedStream == gsub(' ','',temp$answer0[thisTrial]))-1
        temp$cuePos0[thisTrial] <- cuePos
      }
      
      temp$responsePos <- temp$cuePos0+temp$responsePosRelative0
      
      cueTypeForTrackerName <- ifelse(temp$cueType[1]=='endogenous', 'End', 'Ex')
      
      thisEyetrackerFile <- eyetrackerFiles[grep(paste0('.*',participant,cueTypeForTrackerName,'.*'), eyetrackerFiles)]
      theseFixations <- read.table(thisEyetrackerFile, sep='\t', stringsAsFactors = F, header = T)
      
      theseFixations$CURRENT_FIX_X_DEG <- theseFixations$CURRENT_FIX_X/pixelsPerDegree
      theseFixations$CURRENT_FIX_Y_DEG <- theseFixations$CURRENT_FIX_Y/pixelsPerDegree
      
      temp$fixationReject <- FALSE
      theseFixations$fixationDistance <- -99
      
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
      
      #Create densities for the SPE with fixation rejections removed
      
      streamColumns <- grep('streamLtrSequence', colnames(temp))
      
      ID <- strsplit(dataset, '(?<=s)/|(?<=[A-Z][A-Z])End|(?<=[A-Z][A-Z])Ex', perl=T)[[1]][2]
      temp$ID <- ID
      
      endRow <- startRow + nrow(temp)-1
      
      allErrors[startRow:endRow,] <- temp
      
      startRow <- endRow + 1

    }
    print(mean(temp$fixationReject))
  }
}


#allErrors$exp <- factor(allErrors$exp, levels = c('twoStreams','eightStreams','End6Strm82msSOA','Ex6Strm82msSOA'))

#Drop any unused rows. These were allocated for participant(s) who had too many fixation rejections
allErrors <- allErrors[!allErrors$ID=='',]

if(saveAllErrorsTSV){
  write.table(allErrors, file = 'Analysis/allErrorsEndoExoNstream.txt', sep='\t', row.names = F, col.names = T)
}


totalPlot <- ggplot(allErrors[!allErrors$fixationReject,], aes(x=responsePosRelative0))+
  geom_histogram(binwidth = 1)+
  labs(y = 'Count', x = 'Serial Position Error')+
  facet_wrap(~cueType+nStreams+ID)+
  geom_vline(xintercept = 0, linetype = 'dashed')

show(totalPlot)

if(plots){
  if(savePlots){
    ggsave(paste0('plots/',group,'/Combined.png'),totalPlot, width = 20, height = 20, units = 'cm')
  }
}
