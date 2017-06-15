setwd('~/gitCode/nStream/rawData/')
rm(list=ls())

RSVPFiles <- list.files(pattern = '\\.txt', recursive = F)
IDs <- unlist(strsplit(RSVPFiles, split = '(?<=[A-Z]|[a-z])_(?=[0-9])', perl = T)) #Split the file names
IDs <- IDs[seq(1,length(IDs),2)]#Select the IDs and discard the timestamps

eyetrackerFiles <- list.files(path = 'Eyetracker/')

RSVPWithEyeTracking <- RSVPFiles[paste0(IDs,'.txt') %in% eyetrackerFiles]
IDsWithEyeTracking <- IDs[paste0(IDs,'.txt') %in% eyetrackerFiles]

widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 56.5 #cm

pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180)


for(eyeTrackedDataFile in RSVPWithEyeTracking){
  thisFile <- which(RSVPWithEyeTracking == eyeTrackedDataFile)
  ID <- IDsWithEyeTracking[thisFile]
  
  eyeTrackedData <- read.table(paste0('Eyetracker/',ID,'.txt'), sep='\t', header =T)
  RSVPData <- read.table(eyeTrackedDataFile, sep='\t',header=T)
  
  eyeTrackedData$CURRENT_FIX_X_DEG <- eyeTrackedData$CURRENT_FIX_X/pixelsPerDegree #The origin must be the bottom left of the screen
  eyeTrackedData$CURRENT_FIX_Y_DEG <- eyeTrackedData$CURRENT_FIX_Y/pixelsPerDegree
  
  fixPosPix <- c(widthPix/2, heightPix/2)
  fixPosDeg <- fixPosPix/pixelsPerDegree
  
  RSVPData$fixationReject <- FALSE
  
  eyeTrackedData$fixationDistance <- numeric(nrow(eyeTrackedData))
  
  criterion = 1 #if any fixation falls more than one deg from the initial fixation, reject this trial
  
  for(index in unique(eyeTrackedData$TRIAL_INDEX)){ #If the trial has multiple fixations, compare each fixation to the initial fixation and reject that trial if it falls outside of a 1º radius circle centered on the initial fix
    
    theseFixations = which(eyeTrackedData$TRIAL_INDEX==index)
    nFixationsThisTrial <- length(theseFixations)
    
    if(nFixationsThisTrial>1){
      
      initialFixationX = eyeTrackedData$CURRENT_FIX_X_DEG[theseFixations[1]]
      initialFixationY = eyeTrackedData$CURRENT_FIX_Y_DEG[theseFixations[1]]
      
      for(thisFixationRow in theseFixations){ #iterate over DF rows rather than fixations
        if(thisFixationRow == theseFixations[1]){
          #skip the first fixation,
        } else {
          
          xVector <- eyeTrackedData$CURRENT_FIX_X_DEG[thisFixationRow] - fixPosDeg[1]
          yVector <- eyeTrackedData$CURRENT_FIX_Y_DEG[thisFixationRow] - fixPosDeg[2]
          
          fixationDistance <- sqrt(xVector^2 + yVector^2)
          
          eyeTrackedData$fixationDistance[thisFixationRow] <- fixationDistance

          if(fixationDistance>=criterion){
            if(!eyeTrackedData$CURRENT_FIX_BLINK_AROUND[thisFixationRow] %in% c('BEFORE','AFTER') ){
              if(index<=220){
                #RSVPData$fixationReject[index] <- TRUE
                print(fixationDistance)
                print(index)
              }
            }
          }
        }
      }
    }
  }
  #assign(paste0(ID,'Eye'), eyeTrackedData)
  #assign(paste0(ID,'RSVP'), RSVPData)
}
