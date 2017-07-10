##################################################
####Example for Psychopy discussion###############
####Alex O. Holcombe and Charlie Ludowici, 2017###
##################################################


from __future__ import print_function, division
from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import numpy as np
from math import atan, log, ceil, cos, sin, pi, atan2
from copy import deepcopy
import time, sys, os#, pylab
import string, random

eyetrackingOption = True #Include this so can turn it off, because Psychopy v1.83.01 mistakenly included an old version of pylink which prevents EyelinkEyetrackerForPsychopySUPA3 stuff from importing
#Eyetracking stuff
if eyetrackingOption: 
    from EyelinkEyetrackerForPsychopySUPA3 import Tracker_EyeLink #Chris Fajou integration
eyetracking = False
getEyeTrackingFileFromEyetrackingMachineAtEndOfExperiment = False #If True, can take up to 1.5 hrs in certain conditions
#End eyetracking stuff

try:
    from noiseStaircaseHelpers import printStaircase, toStaircase, outOfStaircase, createNoise, plotDataAndPsychometricCurve
except ImportError:
    print('Could not import from noiseStaircaseHelpers.py (you need that file to be in the same directory)')

try: 
    import stringResponse
except ImportError:  
    print('Could not import strongResponse.py (you need that file to be in the same directory)')

try: 
    import letterLineupResponse
except ImportError:  
    print('Could not import letterLineupResponse.py (you need that file to be in the same directory)')

try: #LetterToNumber NumberToLetter
    from alphabetHelpers import *
except ImportError:  
    print('Could not import alphabetHelpers.py (you need that file to be in the same directory)')
try:
    from corticalMagnification import *
except ImportError:
    print('Could not import corticalMagnification.py (you need that file to be in the same directory)')


widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #monitor width in cm
scrn=0 #0 to use main screen, 1 to use external screen connected to computer
fullscr=False #True to use fullscreen, False to not. Timing probably won't be quite right if fullscreen = False
allowGUI = False
units = 'deg'
waitBlank = False
bgColor = [-.7,-.7,-.7]
monitorname = 'testmonitor'
viewdist = 57
cueRadius = 2.5
autoLogging = False
refreshRate = 60
demo = False
exportImages = False
showRefreshMisses = True

logging.console.setLevel(logging.ERROR)  #only show this level  messages and higher

mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)

def openMyStimWindow(): #make it a function because have to do it several times, want to be sure is identical each time
    myWin = visual.Window(monitor=mon,size=(widthPix,heightPix),allowGUI=allowGUI,units=units,color=bgColor,colorSpace='rgb',fullscr=fullscr,screen=scrn,waitBlanking=waitBlank,
                   winType='pyglet' ) #pygame doesn't work, don't know why. Works in textLocationTest.py
    return myWin
myWin = openMyStimWindow()

numLettersToPresent = 24
font = 'Sloan'
letterColor = cueColor = [1,1,1]

fixColor = [1,1,1]
fixatnPtSize = 4
fixSizePix = 6
fixatnTextureWidth = np.round(fixSizePix/4).astype(int)
fixatnNoiseTexture = np.round( np.random.rand(fixatnTextureWidth,fixatnTextureWidth) ,0 )   *2.0-1 #Can counterphase flicker  noisetexture to create salient flicker if you break fixation

fixatn= visual.PatchStim(myWin, tex=fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False)
fixatnCounterphase= visual.PatchStim(myWin, tex= -1*fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False) #reverse contrast
fixatnPoint= visual.Circle(myWin,fillColorSpace='rgb',fillColor=(1,1,1),radius=fixatnPtSize,pos=[0,0],units='pix',autoLog=autoLogging)

cuePos = 10

nStreamsPossibilities = np.arange(2,21,3)
nStreamsPossibilities = np.append(nStreamsPossibilities,21)

streamsPerRing = 7

baseAngleCWfromEast = 0

stimListDualStream = []

numToCue = 1
numResponsesWanted = 1
maxNumRespsWanted = 1
task = 'T1'
targetLeftRightIfOne = 'right'

#For AB, minimum SOAms should be 84  because any shorter, I can't always notice the second ring when lag1.   71 in Martini E2 and E1b (actually he used 66.6 but that's because he had a crazy refresh rate of 90 Hz)
SOAms = 82.35 #82.35 Battelli, Agosta, Goodbourn, Holcombe mostly using 133
letterDurMs = 60 #60

ISIms = SOAms - letterDurMs
letterDurFrames = int( np.floor(letterDurMs / (1000./refreshRate)) )
cueDurFrames = letterDurFrames
ISIframes = int( np.floor(ISIms / (1000./refreshRate)) )
#have set ISIframes and letterDurFrames to integer that corresponds as close as possible to originally intended ms
rateInfo = 'total SOA=' + str(round(  (ISIframes + letterDurFrames)*1000./refreshRate, 2)) + ' or ' + str(ISIframes + letterDurFrames) + ' frames, comprising\n'
rateInfo+=  'ISIframes ='+str(ISIframes)+' or '+str(ISIframes*(1000./refreshRate))+' ms and letterDurFrames ='+str(letterDurFrames)+' or '+str(round( letterDurFrames*(1000./refreshRate), 2))+'ms'
logging.info(rateInfo); print(rateInfo)

trialDurFrames = int( numLettersToPresent*(ISIframes+letterDurFrames) ) #trial duration in frames

monitorname = 'testmonitor'
waitBlank = False
mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#relying on  monitorwidth cm (39 for Mitsubishi to do deg calculations) and gamma info in calibratn
mon.setSizePix( (widthPix,heightPix) )
units='deg' #'cm'

cueType = 'exogenousRing'

cues = list()
for cueN in xrange(max(nStreamsPossibilities)):
    if cueType == 'exogenousRing':
        cue = visual.Circle(myWin, 
                     radius=cueRadius,#Martini used circles with diameter of 12 deg
                     lineColorSpace = 'rgb',
                     lineColor=bgColor,
                     lineWidth=2.0, #in pixels
                     units = 'deg',
                     fillColorSpace = 'rgb',
                     fillColor=None, #beware, with convex shapes fill colors don't work
                     pos= [-5,-5], #the anchor (rotation and vertices are position with respect to this)
                     interpolate=True,
                     autoLog=False)#this stim changes too much for autologging to be useful
    elif cueType =='endogenous':  #tiny dot at fixation point
        cue = visual.Circle(myWin,
                units='pix',
                radius=1, #4
                fillColorSpace='rgb',
                fillColor=bgColor,
                lineWidth=0,
                interpolate=False,
                autoLog = False)
    cues.append(cue)
    


for nStreams in nStreamsPossibilities:
    for targetLeftRightIfOne in  ['right']: # ['left','right']: #If single target, should it be on the left or the right?
        anglesMustBeMultipleOf =  int( round(360 / max(nStreamsPossibilities) ) )
        randomlyAssignCuesToStreams = True
        for cueTemporalPos in [10]:
            for firstRespLRifTwo in ['left','right']:  #If dual target and lineup response, should left one or right one be queried first?
                stimListDualStream.append(         
                     {'streamsPerRing':streamsPerRing, 'nStreams':nStreams, 'numRespsWanted':numResponsesWanted, 'task':task, 'targetLeftRightIfOne':targetLeftRightIfOne, 
                        'cue0temporalPos':cuePos, 'firstRespLRifTwo': firstRespLRifTwo, 'cue1lag':0,'numToCue':numToCue,
                        'baseAngleCWfromEast':baseAngleCWfromEast} 
                  )  #cue1lag = 0, meaning simultaneous targets

trialsDualStream = data.TrialHandler(stimListDualStream,1) #constant stimuli method




trialClock = core.Clock()
numTrialsCorrect = 0; 
numTrialsApproxCorrect = 0;
numTrialsEachRespCorrect= np.zeros( maxNumRespsWanted )
numTrialsEachApproxCorrect= np.zeros( maxNumRespsWanted )


def timingCheckAndLog(ts,trialN):
    #check for timing problems and log them
    #ts is a list of the times of the clock after each frame
    interframeIntervs = np.diff(ts)*1000
    #print '   interframe intervs were ',around(interframeIntervs,1) #DEBUGOFF
    frameTimeTolerance=.2 #proportion longer than refreshRate that will not count as a miss
    longFrameLimit = np.round(1000/refreshRate*(1.0+frameTimeTolerance),2)
    idxsInterframeLong = np.where( interframeIntervs > longFrameLimit ) [0] #frames that exceeded 150% of expected duration
    numCasesInterframeLong = len( idxsInterframeLong )
    print(numCasesInterframeLong)
    logging.error('test')
    if numCasesInterframeLong >0 and (not demo):
       longFramesStr =  'ERROR,'+str(numCasesInterframeLong)+' frames were longer than '+str(longFrameLimit)+' ms'
       if demo: 
         longFramesStr += 'not printing them all because in demo mode'
       else:
           longFramesStr += ' apparently screen refreshes skipped, interframe durs were:'+\
                    str( np.around(  interframeIntervs[idxsInterframeLong] ,1  ) )+ ' and was these frames: '+ str(idxsInterframeLong)
       if longFramesStr != None:
                print( 'trialnum='+str(trialN)+' '+longFramesStr )
                if not demo:
                    flankingAlso=list()
                    for idx in idxsInterframeLong: #also print timing of one before and one after long frame
                        if idx-1>=0:
                            flankingAlso.append(idx-1)
                        else: flankingAlso.append(np.NaN)
                        flankingAlso.append(idx)
                        if idx+1<len(interframeIntervs):  flankingAlso.append(idx+1)
                        else: flankingAlso.append(np.NaN)
                    flankingAlso = np.array(flankingAlso)
                    flankingAlso = flankingAlso[np.negative(np.isnan(flankingAlso))]  #remove nan values
                    flankingAlso = flankingAlso.astype(np.integer) #cast as integers, so can use as subscripts
                    logging.info( 'flankers also='+str( np.around( interframeIntervs[flankingAlso], 1) )  ) #because this is not an essential error message, as previous one already indicates error
                      #As INFO, at least it won't fill up the console when console set to WARNING or higher
    return numCasesInterframeLong
    #end timing check


#Sets the position of RSVP streams on the screen
def calcStreamPos(nStreams, baseAngleCWfromEast,cueOffsets,streami,streamOrNoise):
    #streamOrNoise because noise coordinates have to be in deg, stream in pix
    #cueOffsets are in deg, for instance indicating the eccentricity of the streams/cues
    noiseOffsetKludge = 0.9 #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg for noise case

    thisRingNum =  int(streami / streamsPerRing)
    #print('This ring number: ', thisRingNum)
    ringStreami = streami % streamsPerRing
    #print('ringStreami: ',ringStreami)
    
    if nStreams - streamsPerRing * thisRingNum >= streamsPerRing:
        streamsThisRing = streamsPerRing
    else:
        streamsThisRing = nStreams - streamsPerRing * thisRingNum

    #print('streams this ring: ',streamsThisRing)



    if nStreams ==0:
        pos = np.array([0,0])
    else:
            #assume want them evenly spaced, counterclockwise starting from directly east
            halfAngle = 360/float(streamsThisRing)/2.0
            thisRingAngleOffset = (thisRingNum % 2) * halfAngle #offset odd-numbered rings by half the angle
            thisAngle = baseAngleCWfromEast + ringStreami/streamsThisRing * 360 + thisRingAngleOffset
            x = cueOffsets[thisRingNum]*cos(thisAngle/180*pi)
            y = cueOffsets[thisRingNum]*sin(thisAngle/180*pi)
            pos = np.array([x,y])
        
    if streamOrNoise:  #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg
        pos *= noiseOffsetKludge*pixelperdegree
        
    #pos = np.round(pos) #rounding or integer is a bad idea. Then for small radii, not equally spaced
    #pos = pos.astype(int)
    return pos


def oneFrameOfStim(n,cues,streamLtrSequences,cueDurFrames,letterDurFrames,ISIframes,cuesTemporalPos,whichStreamEachCue,nStreams,ltrStreams,baseAngleThisTrial,
                                      noiseEachStream,proportnNoise,noiseCoordsEachStream,numNoiseDotsEachStream):#draw letter and possibly cue and noise on top
#defining a function to draw each frame of stim. 

  SOAframes = letterDurFrames+ISIframes
  cueFrames = cuesTemporalPos*SOAframes

  letterN = int( np.floor(n/SOAframes) )

  frameOfThisLetter = n % SOAframes #every SOAframes, new letter
  showLetter = frameOfThisLetter < letterDurFrames #if true, it's not time for the blank ISI.  it's still time to draw the letter
  #print 'n=',n,' SOAframes=',SOAframes, ' letterDurFrames=', letterDurFrames, ' (n % SOAframes) =', (n % SOAframes)  #DEBUGOFF
  #so that any timing problems occur just as often for every frame, always draw the letter and the cue, but simply draw it in the bgColor when it's not meant to be on
  cuesTimeToDraw = list([False])*len(cues) #if don't use this, for AB task, bg color T2 cue will be drawn on top of T1 cue
  
  #cue graphics objects for all possible streams should be drawn (in bgColor or cueColor) in E N W S order
  for cue in cues: #might be at same time, or different times
    cue.setLineColor( bgColor )
  for cueN in xrange(len(cuesTemporalPos)): #For each cue, see whether it is time to draw it
    thisCueFrameStart = cueFrames[cueN]
    if n>=thisCueFrameStart and n<thisCueFrameStart+cueDurFrames:
         thisCue = whichStreamEachCue[cueN]
         if cueType=='endogenous':
            cues[thisCue].setFillColor( cueColor )
         else:
            cues[thisCue].setLineColor( cueColor )
         cuesTimeToDraw[thisCue] = True

  for cueN in xrange(len(cues)):
    if cuesTimeToDraw[cueN] == True:  ##if don't use this, for AB task, bg color T2 cue will be drawn on top of T1 cue
        cues[cueN].draw()
  
  for streami in xrange(nStreams):
    thisStream = ltrStreams[streami]
    thisLtrIdx = streamLtrSequences[streami][letterN] #which letter of the predecided sequence should be shown
    #setting the letter size (height) takes a lot of time, so each stream must be drawn in correct height before the trial starts
    if showLetter:
      thisStream[thisLtrIdx].setColor( letterColor )
    else: thisStream[thisLtrIdx].setColor( bgColor )

    thisStream[thisLtrIdx].draw()
    
    #noise
    refreshNoise = False #Not recommended because takes longer than a frame, even to shuffle apparently. Or may be setXYs step
    if proportnNoise>0:
        if refreshNoise:
            if frameOfThisLetter ==0: #refresh it if it's the first frame of the noise, so each letter gets a different noise but noise not dynamic within a letter
                np.random.shuffle(noiseCoordsEachStream[streami]) #refresh the noise by shuffling the possible locations of noise dots
                numNoiseDotsThis = numNoiseDotsEachStream[streami]
                dotCoords = noiseCoordsEachStream[streami][0:numNoiseDotsThis] #Take the first numNoiseDots random locations to plot the dots
                posThisDeg =  calcStreamPos(nStreams,cueOffsets,streami,streamOrNoise=1)
                #print('streami=',streami,'posThisDeg=',posThisDeg,'cueOffsets=',cueOffsets,'numStream=',numStreams)
                #Displace the noise to present it over the letter stream
                dotCoords[:,0] += posThisDeg[0]
                dotCoords[:,1] += posThisDeg[1]
                noiseEachStream[streami].setXYs(dotCoords)
        noiseEachStream[streami].draw()

  return True 


cues = list()
for cueN in xrange(max(nStreamsPossibilities)):
    if cueType == 'exogenousRing':
        cue = visual.Circle(myWin, 
                     radius=cueRadius,#Martini used circles with diameter of 12 deg
                     lineColorSpace = 'rgb',
                     lineColor=bgColor,
                     lineWidth=2.0, #in pixels
                     units = 'deg',
                     fillColorSpace = 'rgb',
                     fillColor=None, #beware, with convex shapes fill colors don't work
                     pos= [-5,-5], #the anchor (rotation and vertices are position with respect to this)
                     interpolate=True,
                     autoLog=False)#this stim changes too much for autologging to be useful
    elif cueType =='endogenous':  #tiny dot at fixation point
        cue = visual.Circle(myWin,
                units='pix',
                radius=1, #4
                fillColorSpace='rgb',
                fillColor=bgColor,
                lineWidth=0,
                interpolate=False,
                autoLog = False)
    cues.append(cue)

    
def do_RSVP_stim(nStreams, trial, proportnNoise,trialN):
    #relies on global variables:
    #   logging, bgColor
    #
    nStreams = trial['nStreams']
    print("nStreams = ",trial['nStreams'], 'task=',task)
    if task != 'allCued':
        print('targetLeftRightIfOne=',trial['targetLeftRightIfOne'], 'cue0temporalPos=',trial['cue0temporalPos'])
    
    #set up which temporal positions are cued
    cuesTemporalPos = [] #will contain the positions of all the cues (targets)

    if task =='T1': #target on only one side will be task 'T1' so only one cue
        numTargets = 1
        cuesTemporalPos.append(trial['cue0temporalPos'])

          
    cuesTemporalPos = np.array(cuesTemporalPos) #convert to np array
        
    #assign the letters to be shown in each stream
    streamLtrSequences = list() 
    for streami in xrange(nStreams):
        letterSeqThisStream =  np.arange(0,24)
        np.random.shuffle(letterSeqThisStream)
        streamLtrSequences.append( letterSeqThisStream )


    for streami in xrange(nStreams): #Let's set position and scale height outside of the frame loop
      thisStream = ltrStreams[streami]
      for letterN in range(numLettersToPresent):
          thisLtrIdx = streamLtrSequences[streami][letterN] #which letter of the predecided sequence should be shown
          #setting the letter size (height) takes a lot of time, so each stream must be drawn in correct height before the trial starts

          posThis = calcStreamPos(nStreams, thisTrial['baseAngleCWfromEast'],cueOffsets,streami,streamOrNoise=0)

          thisStream[thisLtrIdx].pos = posThis
          
          #print('thisStream[thisLtrIdx].pos: '+str(thisStream[thisLtrIdx].pos))

          thisStream[thisLtrIdx] = corticalMagnification(thisStream[thisLtrIdx], ltrHeight)
          
          thisStream[thisLtrIdx].text = thisStream[thisLtrIdx].text
      ltrStreams[streami] = thisStream

    #set up corrAnsEachResp, whichStreamEachResp, whichStreamEachCue, whichRespEachCue, and cues' positions 
    #change corrAnswers to answerEachResp everywhere. To recover stream, could use whichStreamEachResp. Sensible if only one stream is queried and just want to print out that on each trial.
    #whichStreamEachCue indicate which stream each cue refers to. These can be in different order than whichStreamEachResp, because randomised which queried first
    #whichStreamEachResp is which stream each response refers to (which stream was queried for 0th response, 1st response, etc)
    corrAnsEachResp = list(); whichStreamEachCue = list(); whichStreamEachResp = list(); whichRespEachCue = list()
    #For instance, if numRespsWanted = 1, then a random one is queried.
    if len(set(cuesTemporalPos)) > 1:
        print('ERROR: Expected for this task that all cues would cue the same temporal position. Randomisation of whichResp will be wrong, need to randomise within each temporalPos')
        core.quit()
    #For each cue, work out correct answer, what stream it is
    corrAnsEachCue = []  #because order of responses will be different than order of cues
    numCues = len(cuesTemporalPos) 
    #Randomly assign cues to streams (with no repeats, because assuming all cues identical)
    whichStreamEachCue = range(nStreams)
    if randomlyAssignCuesToStreams: #not doing it in nStreams versus 2 streams, because position stream 0, cued is randomised by baseAngle
        random.shuffle(whichStreamEachCue)
    whichStreamEachCue = whichStreamEachCue[:numCues] #Now will be random subset of streams, numCues long
    whichStreamEachResp = deepcopy(whichStreamEachCue)
    for cuei in xrange(numCues):  #work out correct answer for each cue
        whichStreamThisCue = whichStreamEachCue[cuei]
        letterIdxThisStream = streamLtrSequences[whichStreamThisCue][cuesTemporalPos[cuei]]
        corrAnsEachCue.append( letterIdxThisStream    )
        corrAnsEachResp.append( letterIdxThisStream )
        whichRespEachCue.append(cuei) #assume that responses are queried in the order of the cues. Note that above, which stream each cue corresponds to is random
    #Need to shuffle which stream is queried first, second, etc. Remember, we're assuming there's only one temporalPos
    #reduce whichStreamEachResp to numRespsWanted. Also whichRespEachCue. Leave corrAnsEachResp same length so can do error analysis checking for swaps.
    whichRespEachCue = np.array(whichRespEachCue) #so that behaves correctly with np.where
    #whichRespEachCue can be longer than number of responses. Otherwise may never get to cue (stream) corresponding to the single response cued.
    #So, go through and replace all that are greater than numRespsWanted with -999
    cuesNotQueriedIdxs= np.where( whichRespEachCue > trial['numRespsWanted']-1 )[0] #these streams were not to be responded to
    #print('cuesNotQueriedIdxs=',cuesNotQueriedIdxs,'whichRespEachCue=',whichRespEachCue, 'where output= ', np.where( whichRespEachCue > trial['numRespsWanted']-1 ),  'numRespsWanted=', trial['numRespsWanted'] ) #AHdebug
    if len( cuesNotQueriedIdxs ) > 0:
        whichRespEachCue[ cuesNotQueriedIdxs ] = -999 #Leaving those that actually were queried.
    whichStreamEachResp = whichStreamEachResp[ :trial['numRespsWanted'] ] #reduce to actual number of responses
    corrAnsEachResp = corrAnsEachResp[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
    whichRespEachCue = whichRespEachCue[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
    #print('whichStreamEachCue=',whichStreamEachCue,' whichStreamEachResp=',whichStreamEachResp,'whichRespEachCue=',whichRespEachCue,  ' corrAnsEachResp=',corrAnsEachResp)

    #Position the cue and scale its size
    for cuedStream in whichStreamEachResp: #Drawing the cues in the location they're supposed to be in
        #assume each cue the succeeding stream (usually all cues same temporal position)
        #assume only one response per time (Only one stream queried per temporalPos). Cut this down to one below.
        posThis =  calcStreamPos(nStreams, trial['baseAngleCWfromEast'],cueOffsets,cuedStream,streamOrNoise=0)
        if cueType =='endogenous':  #reduce radius to bring it right next to fixation center
    #                angleRad = streamI/numStreams * 2*pi
            desiredDistFromFixatn=2 #pixels
    #                newX = desiredDistFromFixatn*cos(angleRad)
    #                newY = desiredDistFromFixatn*sin(angleRad)
    #                print('newX=',newX,'newY=',newY)
    #                posThis = [newX,newY]
            desiredDistFromFixatnEachRing = [ desiredDistFromFixatn ]
            posThis = calcStreamPos(nStreams,desiredDistFromFixatnEachRing,streamI,streamOrNoise=0)
        cues[cuedStream].setPos( posThis )
        cues[cuedStream] = corticalMagnification(cues[cuedStream], 0.9810000000000002, cue = True) #this is the cuesize from the original experiment

    corrAnsEachResp = list(); whichStreamEachCue = list(); whichStreamEachResp = list(); whichRespEachCue = list()

#assume all len(cuesTemporalPos) streams cued at same time, with numRespsWanted to be reported, in random order.
    #For instance, if numRespsWanted = 1, then a random one is queried.
    if len(set(cuesTemporalPos)) > 1:
        print('ERROR: Expected for this task that all cues would cue the same temporal position. Randomisation of whichResp will be wrong, need to randomise within each temporalPos')
        core.quit()
    #For each cue, work out correct answer, what stream it is
    corrAnsEachCue = []  #because order of responses will be different than order of cues
    numCues = len(cuesTemporalPos) 
    #Randomly assign cues to streams (with no repeats, because assuming all cues identical)
    whichStreamEachCue = range(nStreams)
    if randomlyAssignCuesToStreams: #not doing it in nStreams versus 2 streams, because position stream 0, cued is randomised by baseAngle
        random.shuffle(whichStreamEachCue)
    whichStreamEachCue = whichStreamEachCue[:numCues] #Now will be random subset of streams, numCues long
    whichStreamEachResp = deepcopy(whichStreamEachCue)
    for cuei in xrange(numCues):  #work out correct answer for each cue
        whichStreamThisCue = whichStreamEachCue[cuei]
        letterIdxThisStream = streamLtrSequences[whichStreamThisCue][cuesTemporalPos[cuei]]
        corrAnsEachCue.append( letterIdxThisStream    )
        corrAnsEachResp.append( letterIdxThisStream )
        whichRespEachCue.append(cuei) #assume that responses are queried in the order of the cues. Note that above, which stream each cue corresponds to is random
    #Need to shuffle which stream is queried first, second, etc. Remember, we're assuming there's only one temporalPos
    #reduce whichStreamEachResp to numRespsWanted. Also whichRespEachCue. Leave corrAnsEachResp same length so can do error analysis checking for swaps.
    whichRespEachCue = np.array(whichRespEachCue) #so that behaves correctly with np.where
    #whichRespEachCue can be longer than number of responses. Otherwise may never get to cue (stream) corresponding to the single response cued.
    #So, go through and replace all that are greater than numRespsWanted with -999
    cuesNotQueriedIdxs= np.where( whichRespEachCue > trial['numRespsWanted']-1 )[0] #these streams were not to be responded to
    #print('cuesNotQueriedIdxs=',cuesNotQueriedIdxs,'whichRespEachCue=',whichRespEachCue, 'where output= ', np.where( whichRespEachCue > trial['numRespsWanted']-1 ),  'numRespsWanted=', trial['numRespsWanted'] ) #AHdebug
    if len( cuesNotQueriedIdxs ) > 0:
        whichRespEachCue[ cuesNotQueriedIdxs ] = -999 #Leaving those that actually were queried.
    whichStreamEachResp = whichStreamEachResp[ :trial['numRespsWanted'] ] #reduce to actual number of responses
    corrAnsEachResp = corrAnsEachResp[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
    whichRespEachCue = whichRespEachCue[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
    #print('whichStreamEachCue=',whichStreamEachCue,' whichStreamEachResp=',whichStreamEachResp,'whichRespEachCue=',whichRespEachCue,  ' corrAnsEachResp=',corrAnsEachResp)

    #Position the cue and scale its size
    for cuedStream in whichStreamEachResp: #Drawing the cues in the location they're supposed to be in
        #assume each cue the succeeding stream (usually all cues same temporal position)
        #assume only one response per time (Only one stream queried per temporalPos). Cut this down to one below.
        posThis =  calcStreamPos(nStreams, trial['baseAngleCWfromEast'],cueOffsets,cuedStream,streamOrNoise=0)
        if cueType =='endogenous':  #reduce radius to bring it right next to fixation center
    #                angleRad = streamI/numStreams * 2*pi
            desiredDistFromFixatn=2 #pixels
    #                newX = desiredDistFromFixatn*cos(angleRad)
    #                newY = desiredDistFromFixatn*sin(angleRad)
    #                print('newX=',newX,'newY=',newY)
    #                posThis = [newX,newY]
            desiredDistFromFixatnEachRing = [ desiredDistFromFixatn ]
            posThis = calcStreamPos(nStreams,desiredDistFromFixatnEachRing,streamI,streamOrNoise=0)
        cues[cuedStream].setPos( posThis )
        cues[cuedStream] = corticalMagnification(cues[cuedStream], 0.9810000000000002, cue = True) #this is the cuesize from the original experiment

    #debug printouts
    #print( 'streamLtrSequences[0]=',[numberToLetter(x) for x in streamLtrSequences[0]] )
    #if trial['numStreams']>1:
    #    print( 'streamLtrSequences[1]=',[numberToLetter(x) for x in streamLtrSequences[1]] )
    firstCueStream = whichStreamEachCue[0]
    firstCueItem = streamLtrSequences[firstCueStream][cuesTemporalPos[0]]
    #print( "corrAnsEachResp=", [numberToLetter(x) for x in corrAnsEachResp],  "First cue cues stream",firstCueStream,   " and letter ",numberToLetter(firstCueItem), end='')
    if trial['numToCue'] > 1:
        secondCueStream = whichStreamEachCue[1]
        secondCueItem = streamLtrSequences[secondCueStream][cuesTemporalPos[1]]
        print(  " while second cue cues stream",secondCueStream, " and letter ",numberToLetter(secondCueItem) )
    else: print('')
    #end debug printouts
            
    noiseEachStream = list(); noiseCoordsEachStream = list(); numNoiseDotsEachStream = list()
    if proportnNoise > 0: #generating noise is time-consuming, so only do it once per trial. Then shuffle noise coordinates for each letter
        for streami in xrange(nStreams):
            (noiseThis,allFieldCoordsThis,numNoiseDotsThis) = createNoise(proportnNoise,myWin,noiseFieldWidthPix, bgColor)  #for the left stream, or the only stream
            noiseEachStream.append(noiseThis)
            numNoiseDotsEachStream.append(numNoiseDotsThis)
            noiseCoordsEachStream.append(allFieldCoordsThis)
        #Work out how to displace the noise so it will be on top of the streams, and then displace it
        for streami in xrange(nStreams):
            numNoiseDotsThis = numNoiseDotsEachStream[streami]
            dotCoords = noiseCoordsEachStream[streami][0:numNoiseDotsThis] #Take the first numNoiseDots random locations to plot the dots
            posThisPix =  calcStreamPos(nStreams,trial['baseAngleCWfromEast'],cueOffsets,streami,streamOrNoise=1)
            #Displace the noise to present it over the letter stream
            dotCoords[:,0] += posThisPix[0]
            dotCoords[:,1] += posThisPix[1]
            noiseEachStream[streami].setXYs(dotCoords)
    #end prep of cuesTemporalPos, streamLtrSequences, corrAnsEachResp, noiseEachStream, numNoiseDotsEachStream, noiseCoordsEachStream
    
    preDrawStimToGreasePipeline = list() #I don't know why this works, but without drawing it I have consistent timing blip first time that draw ringInnerR for phantom contours
    for cue in cues:
      if cueType == 'endogenous':
        cue.setFillColor(bgColor)
      else:
        cue.setLineColor(bgColor)
    preDrawStimToGreasePipeline.extend([cue])
    for stim in preDrawStimToGreasePipeline:
        stim.draw()
    myWin.flip(); myWin.flip()
    #end preparation of stimuli
    
    core.wait(.1);
    trialClock.reset()
    fixatnPeriodMin = 0.3
    fixatnPeriodFrames = int(   (np.random.rand(1)/2.+fixatnPeriodMin)   *refreshRate)  #random interval between 800ms and 1.3s (changed when Fahed ran outer ring ident)
    ts = list(); #to store time of each drawing, to check whether skipped frames
    if (fixatnPeriodFrames-1) % 2 ==0:
        fixatnPeriodFrames +=1 #make it odd so that phase will be correct for transitioning to fixation flicker during the trial
    for i in range(fixatnPeriodFrames):  #prestim fixation interval
        if showRefreshMisses: #flicker fixation on and off at framerate to see when skip frame
            if i%2 or demo or exportImages: 
                  fixatn.draw()
            else: fixatnCounterphase.draw()
        fixatnPoint.draw() #small circle on top
        myWin.flip()  #end fixation interval
    #myWin.setRecordFrameIntervals(True);  #can't get it to stop detecting superlong frames
    t0 = trialClock.getTime()
    for n in range(trialDurFrames): #this is the loop for this trial's stimulus!
        if showRefreshMisses: #flicker fixation on and off at framerate to see when skip frame
            if n%2 or demo or exportImages: 
                  fixatn.draw()
            else: fixatnCounterphase.draw()
        fixatnPoint.draw()
        worked = oneFrameOfStim( n,cues,streamLtrSequences,cueDurFrames,letterDurFrames,ISIframes,cuesTemporalPos,whichStreamEachCue,
                                                      nStreams,ltrStreams,thisTrial['baseAngleCWfromEast'],
                                                     noiseEachStream,proportnNoise,noiseCoordsEachStream,numNoiseDotsEachStream) #draw letter and possibly cue and noise on top
        if exportImages:
            myWin.getMovieFrame(buffer='back') #for later saving
            framesSaved +=1              
        myWin.flip()
        t=trialClock.getTime()-t0;  ts.append(t);
    #end of big stimulus loop
    myWin.setRecordFrameIntervals(False);

    postCueNumBlobsAway=-999 #doesn't apply to non-tracking and click tracking task
    return streamLtrSequences,cuesTemporalPos,corrAnsEachResp,whichStreamEachCue,whichStreamEachResp,whichRespEachCue,ts  


ltrHeight = .9 #This is the cortically-scaled height at 3 degrees of eccentricity. This is what we used in 2vs8
cueOffsets = [3,7,11.5]
maxStreams = max(nStreamsPossibilities)
ltrStreams = list()
for streami in xrange(maxStreams):
    streamThis = list()
    #calc desired eccentricity and size
    #currently assumes nStreams and numRings and cueOffsets doesn't change
    
    #print('thisRingNum = ',thisRingNum,'nStreams=',nStreams, ' ltrHeightThis=',ltrHeightThis)
    for i in range(0,26):
        if i is not 2 and i is not 22:
            ltr = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb', font = font, color=letterColor,alignHoriz='center',alignVert='center',units='deg',autoLog=autoLogging)
                
            letter = numberToLetter(i)
            #print(letter)
            ltr.setText(letter,log=False)
            ltr.setColor(bgColor)
            streamThis.append( ltr )
    ltrStreams.append( streamThis )


noisePercent = 0
expStop = False

ABfirst = False
nDone =0
totalTrials = 0

msg = "Starting dual stream part of experiment"
trials = trialsDualStream
logging.info(msg); print(msg)
totalTrials += trialsDualStream.nTotal

nextText = visual.TextStim(myWin,pos=(0, .1),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)


while nDone < totalTrials and expStop==False:
    print("nDone = ", nDone, " out of ", totalTrials, " trials.nRemaining = ", trials.nRemaining)

    #Control which block we are in, AB or dualStream
    if trials.nRemaining == 0:  # trialsAB.nTotal:
        if trials == trialsAB:  #check if reached end of the AB part
            trials = trialsDualStream
            msg = "Starting dual stream part of experiment"
            logging.info(msg); print(msg)
        elif trials == trialsDualStream: #check if reached end of dual stream part
            trials = trialsAB
            msg='Starting AB part of experiment'
            logging.info(msg); print(msg)
    #end control of which block we are in 
    if eyetracking: 
        tracker.startEyeTracking(nDone,True,widthPix,heightPix) #start recording with eyetracker
    
    thisTrial = trials.next() #get a proper (non-staircase) trial
    streamLtrSequences,cuesTemporalPos,corrAnsEachResp,whichStreamEachCue,whichStreamEachResp,whichRespEachCue,ts  = \
                do_RSVP_stim(nStreams,thisTrial,noisePercent/100.,nDone)  #DO THE TRIAL!
    if eyetracking:
        tracker.stopEyeTracking()
    
    print(ts)
    
    numCasesInterframeLong = timingCheckAndLog(ts,nDone)

    responseDebug=False; responses = list(); responsesAutopilot = list();  #collect responses
    lineupResponse = False
    myWin.setMouseVisible(True)
    if lineupResponse:
        if len(whichStreamEachResp) != thisTrial['numRespsWanted']:
            print("len(whichStreamEachResp) does not match numRespsWanted")
        sideFirstLeftRightCentral=2 #default , respond to central
        showBothSides=False
        if thisTrial['numRespsWanted'] == 1:
            if nStreams == 2:
                showBothSides = False
                sideFirstLeftRightCentral= not whichStreamEachResp[0]  #have to flip it because 0 means East, right       # thisTrial['targetLeftRightIfOne']
        else: #numRespsWanted >1
            if nStreams ==2:
                showBothSides = True
                sideFirstLeftRightCentral =  not whichStreamEachResp[0]  #thisTrial['firstRespLR']
            else: #numStreams must be greater than 2. Probably only want to do lineup for 1. As stopgap measure, can put the lineup centrally on every trial
                showBothSides = False
        #print('sideFirstLeftRightCentral = ',sideFirstLeftRightCentral)
        alphabet = list(string.ascii_uppercase)
        possibleResps = alphabet 
        possibleResps.remove('C'); possibleResps.remove('W')
        expStop,passThisTrial,responses,buttons,responsesAutopilot = \
            letterLineupResponse.doLineup(myWin,bgColor,myMouse,clickSound,badKeySound,possibleResps,showBothSides,sideFirstLeftRightCentral,autopilot) #CAN'T YET HANDLE MORE THAN 2 LINEUPS
            

    #print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    if not expStop:
        print('nDone=', nDone,' trials.nTotal=',trials.nTotal) #' trials.thisN=',trials.thisN
        pctTrialsCompletedForBreak = np.array([.6,.8])  
        breakTrials = np.round(trials.nTotal*pctTrialsCompletedForBreak)
        timeForTrialsRemainingMsg = np.any(trials.thisN==breakTrials)
        if timeForTrialsRemainingMsg :
            pctDone = round(    (1.0*trials.thisN) / (1.0*trials.nTotal)*100,  0  )
            nextText.setText('Press "SPACE" to continue!')
            nextText.draw()
            progressMsg = 'Completed ' + str(trials.thisN) + ' of ' + str(trials.nTotal) + ' trials'  #EVA if this doesn't work, change it to progressMsg = ' '
            #NextRemindCountText.setText(progressMsg)
            #NextRemindCountText.draw()
            myWin.flip() # myWin.flip(clearBuffer=True) 
            waiting=True
            while waiting:
               if autopilot: break
               elif expStop == True:break
               for key in event.getKeys():      #check if pressed abort-type key
                     if key in ['space','ESCAPE']: 
                        waiting=False
                     if key in ['ESCAPE']:
                        expStop = False
            myWin.clearBuffer()
        core.wait(.2); time.sleep(.2)
    #end main trials loop
timeAndDateStr = time.strftime("%H:%M on %d %b %Y", time.localtime())
if eyetracking and getEyeTrackingFileFromEyetrackingMachineAtEndOfExperiment:
    tracker.closeConnectionToEyeTracker(eyeMoveFile)
msg = 'Finishing at '+timeAndDateStr
print(msg); logging.info(msg)
logging.flush()
if expStop:
    msg = 'user aborted experiment on keypress with trials done=' + str(nDone) + ' of ' + str(trials.nTotal+1)
    print(msg); logging.error(msg)

if (nDone >0):
    print('Of ',nDone,' trials, on ',numTrialsCorrect*1.0/nDone*100., '% of all trials all targets reported exactly correct',sep='')
    print('All targets approximately correct in ',round(numTrialsApproxCorrect*1.0/nDone*100,1),'% of trials',sep='')

myWin.close() #have to close window if want to show a plot
 