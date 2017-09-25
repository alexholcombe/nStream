setwd('~/gitCode/nStream/rawData/') #set the working directory
rm(list=ls()) #clear the workspace

################################################################################################
####Some of these files were accidentally split during data collection. ########################
####This code is just here to show my labmates how to analyse the eyetracking data##############
####It isn't the final eyetracking code for the experiment. That's in Analysis/wrangleSONA.R####
################################################################################################

computeStreamPos <- function(streami, baseAngleCWFromEast){
  nStreams <- 8
  thisAngle <- baseAngleCWFromEast+(streami/nStreams * 360)
  thisAngle%%360
}


SPECheck = TRUE

RSVPFiles <- list.files(pattern = '^[A-Z][A-Z][0-9].*\\.txt$', recursive= F) #List the data files that correspond to the IDs used for the sona participants. These had the form [first initial][second initial][number]
IDs <- unlist(strsplit(RSVPFiles, split = '(?<=[A-Z][0-9]|[0-9])_(?=[0-9][0-9][A-Z])', perl = T)) #Split the file names
IDs <- IDs[seq(1,length(IDs),2)]#Select the IDs and discard the timestamps.

eyetrackerFiles <- list.files(path = 'Eyetracking/') #eyetracking files are in rawData/Eyetracker

RSVPWithEyeTracking <- RSVPFiles[paste0(IDs,'.txt') %in% eyetrackerFiles] #Select RSVP files associated with eyetracking files. This works because of the ordering of the file names 
IDsWithEyeTracking <- IDs[paste0(IDs,'.txt') %in% eyetrackerFiles] #Select Eyetracking files associated with RSVP files. 

widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #cm
viewingDist = 59.0 #cm

pixelsPerDegree = widthPix / (atan(monitorwidth/viewingDist)/pi*180) #pixels per degree of visual angle

start <- 1

for(eyeTrackedDataFile in RSVPWithEyeTracking){ #iterate over the RSVP files with eyetracking
  thisFile <- which(RSVPWithEyeTracking == eyeTrackedDataFile) #Where in the alphabetically ranked RSVP files with eyetracking does this file fall?
  ID <- IDsWithEyeTracking[thisFile] #get its ID (the ID vector has the same order as the RSVP files)
  
  eyeTrackedData <- read.table(paste0('Eyetracking/',ID,'.txt'), sep='\t', header =T) #read in the eyetracking data
  
  if(ID %in% c('AM3')){
    RSVPData <- read.table(eyeTrackedDataFile, sep='\t',header=F, skip = 1, stringsAsFactors = F) #read in the RSVP data
    colnames(RSVPData) <- c("experimentPhase", "trialnum", "subject", "task", "noisePercent", "targetLeftRightIfOne", "nPreCueStreams", "baseAngleCWfromEast", "resp0", "button0", "cuePos0", "answer0", "correct0", "whichStream0", "angleOfWhichStream0", "whichRespCue0", "responsePosRelative0", "streamLtrSequence0", "streamLtrSequence1", "streamLtrSequence2", "streamLtrSequence3", "streamLtrSequence4", "streamLtrSequence5", "streamLtrSequence6", "streamLtrSequence7", "timingBlips")
  } else {
    RSVPData <- read.table(eyeTrackedDataFile, sep='\t',header=T, stringsAsFactors = F) #read in the RSVP data
  }
  
  if(SPECheck){
    for(row in 1:nrow(RSVPData)){
      thisStream <- RSVPData$whichStream0[row]
      theseLetters <- unlist(strsplit(RSVPData[row,paste0("streamLtrSequence", thisStream)],''))
      thisSP <- which(theseLetters==RSVPData$resp0[row])
      thisSPE <- thisSP - 1 - RSVPData$cuePos0[row] 
      if(thisSPE!=RSVPData$responsePosRelative0[row]){
        print(row)
        break
      }
    }
  }
  
  eyeTrackedData$CURRENT_FIX_X_DEG <- eyeTrackedData$CURRENT_FIX_X/pixelsPerDegree #X coordinate of the current fixation in degrees of visual angle. The origin is the screen's bottom left corner
  eyeTrackedData$CURRENT_FIX_Y_DEG <- eyeTrackedData$CURRENT_FIX_Y/pixelsPerDegree #Y coord in degrees
  
  fixPosPix <- c(widthPix/2, heightPix/2) #Fixation point should be at the center of the screen, but I don't use this for my analysis.
                                          #Instead, I use the initial fixation, which can vary from the fixation point because of noise in the eyetracking
  fixPosDeg <- fixPosPix/pixelsPerDegree  #So you can ignore these lines (or change the code to use them)
  
  RSVPData$fixationReject <- FALSE #Create a column that represents fixation rejections in the RSVP data. Set all rows to FALSE
  
  eyeTrackedData$fixationDistance <- numeric(nrow(eyeTrackedData)) #Create an empty numeric column in the eyetracking data. This will represent the distance between a fixation and the initial fixation
  
  criterion = 2 #if any fixation falls more than one deg from the initial fixation, reject this trial
  
  for(index in unique(eyeTrackedData$TRIAL_INDEX)){ #Iterate over the unique values in the trial index column
    
    #If the trial has multiple fixations, compare each fixation to the initial fixation and reject that trial if it falls outside of a 1º radius circle centered on the initial fix
    
    theseFixations = which(eyeTrackedData$TRIAL_INDEX==index) #Which fixations happened on this trial?? 
                                                              #These are in terms of rank order over the whole experiment, so if the first two trials 
                                                              #only had one fixation each, but the third trial had four fixations, this variable would be c(3,4,5,6) for the third trial
    nFixationsThisTrial <- length(theseFixations) #How many of these fixations were there?
    
    if(nFixationsThisTrial>1){ #If there was more than one fixation on this trial...
      
      initialFixationX = eyeTrackedData$CURRENT_FIX_X_DEG[theseFixations[1]] #get the X and Y coords of the first fixation in degrees of visual angle (the origin is, again, the bottom left)
      initialFixationY = eyeTrackedData$CURRENT_FIX_Y_DEG[theseFixations[1]]
      
      for(thisFixationRow in theseFixations){ #iterate over the rows corresponding to these fixations
        if(thisFixationRow == theseFixations[1]){
          #skip the first fixation,
        } else {
          
          xVector <- eyeTrackedData$CURRENT_FIX_X_DEG[thisFixationRow] - fixPosDeg[1] #get the distance and direction on the X axis between this fixation and the initial fixation
          yVector <- eyeTrackedData$CURRENT_FIX_Y_DEG[thisFixationRow] - fixPosDeg[2] #do the same for the Y axis
          
          fixationDistance <- sqrt(xVector^2 + yVector^2) #get the absolute distance (no direction info)
          
          eyeTrackedData$fixationDistance[thisFixationRow] <- fixationDistance #store absolute distance in the eyetracked data file on the row corresponding to this fixation

          if(fixationDistance>=criterion){ #Iff the distance is greater than the criterion value
            if(!eyeTrackedData$CURRENT_FIX_BLINK_AROUND[thisFixationRow] %in% c('BEFORE','AFTER') ){ #AND if there was no blink before or after the fixation
                RSVPData$fixationReject[index] <- TRUE #Then set the rejection variable to true (all values were set to false initially)
                #print(fixationDistance)
                #print(index)
            }
          }
        }
      }
    }
  }
  
  if(ID %in% c('AM3', 'HT5','EF6')){ #I was using math.atan to compute the angle, but it doesn't do quadrants
    for(row in 1:nrow(RSVPData)){
      RSVPData$angleOfWhichStream0[row] <- computeStreamPos(RSVPData$whichStream0[row], RSVPData$baseAngleCWfromEast[row])
    }
  }
  
  if(start==1){
    nrows <- nrow(RSVPData)*length(IDsWithEyeTracking)
    masterDF <- RSVPData[nrow(RSVPData)+1:nrows,]
  }

  
  end <- start+nrow(RSVPData)-1
  masterDF[start:end,] <- RSVPData
  start <- end + 1
  
  #uncomment these to save the eyetracking data for each participant to the workspace
  
  #assign(paste0(ID,'Eye'), eyeTrackedData) 
  #assign(paste0(ID,'RSVP'), RSVPData)
}
