#Alex Holcombe alex.holcombe@sydney.edu.au
#See the README.md for more information: https://github.com/alexholcombe/attentional-blink/blob/master/README.md
#git remote add origin https://github.com/alexholcombe/nStream
from __future__ import print_function, division
from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import numpy as np
from math import atan, log, ceil, cos, sin, pi, atan2
from copy import deepcopy
import time, sys, os#, pylab
import string, random
import cv2
#from mss import mss
from PIL import Image

eyetrackingOption = True #Include this so can turn it off, because Psychopy v1.83.01 mistakenly included an old version of pylink which prevents EyelinkEyetrackerForPsychopySUPA3 stuff from importing
#Eyetracking stuff
if eyetrackingOption: 
    from EyelinkEyetrackerForPsychopySUPA3 import Tracker_EyeLink #Chris Fajou integration
eyetracking = False
getEyeTrackingFileFromEyetrackingMachineAtEndOfExperiment = False #If True, can take up to 1.5 hrs in certain conditions
#End eyetracking stuff

try: 
    import stringResponse
except ImportError:  
    print('Could not import strongResponse.py (you need that file to be in the same directory)')

try: 
    import letterLineupResponse
except ImportError:  
    print('Could not import letterLineupResponse.py (you need that file to be in the same directory)')

try: #LetterToNumber NumberToLetter
    import alphabetHelpers
except ImportError:  
    print('Could not import alphabetHelpers.py (you need that file to be in the same directory)')
try:
    import corticalMagnification
except ImportError:
    print('Could not import corticalMagnification.py (you need that file to be in the same directory)')
try:
    import setupHelpers
except ImportError:
    print('Could not import setupHelpers.py (you need that file to be in the same directory)')


#THINGS THAT COULD PREVENT SUCCESS ON A STRANGE MACHINE
#same screen or external screen? Set scrn=0 if one screen. scrn=1 means display stimulus on second screen.
#widthPix, heightPix
quitFinder = False #if checkRefreshEtc, quitFinder becomes True
autopilot=False
demo=False #False
exportImages= False #quits after one trial
subject='Hubert' #user is prompted to enter true subject name
if autopilot: subject='auto'
if os.path.isdir('.'+os.sep+'rawData/18Streams'):
    dataDir='rawData/18Streams'
else:
    print('"rawData" directory does not exist, so saving data in present working directory')
    dataDir='.'
timeAndDateStr = time.strftime("%d%b%Y_%H-%M", time.localtime())





####################################
####################################
## Display and stimuli parameters ##
####################################
####################################
demo = False
showRefreshMisses=True #flicker fixation at refresh rate, to visualize if frames missed
feedback=False
autoLogging=False
refreshRate = 100
if demo:
    refreshRate = 60.;  #100 LN: refresh rate for previous AB and RSVP task for gamers was 60

font = 'sloan'

threshCriterion = 0.58
bgColor = [0,0,0] # [-1,-1,-1]
cueColor = [1.,1.,1.]
cueType = 'exogenousRing' #'lowerCase' #'exogenousRing' #'endogenous':
if cueType == 'endogenous':
    cueColor = [1,-1,-1]
letterColor = [1.,0.,0.]
cueRadius = 2.5 #6 deg, as in Martini E2    Letters should have height of 2.5 deg

#There's no lowercase in sloan
if cueType is 'lowerCase':
    font = 'Arial'

viewdist = 30. #cm

monitorname = 'testmonitor'

waitBlank = False

widthPix = 1024 #monitor width in pixels of Agosta
heightPix = 1024 #800 #monitor height in pixels
monitorwidth = 40.5

mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#relying on  monitorwidth cm (39 for Mitsubishi to do deg calculations) and gamma info in calibratn

mon.setSizePix( (2880,1800) )

units='deg' #'cm'

scrn = 1

doStaircase = False

screenValues = {
    'widthPix': widthPix, #monitor width in pixels of Agosta
    'heightPix': heightPix, #800 #monitor height in pixels
    'monitorwidth' :40.5, #monitor width in cm
    'scrn':0, #0 to use main screen, 1 to use external screen connected to computer
    'fullscr':True, #True to use fullscreen, False to not. Timing probably won't be quite right if fullscreen = False
    'allowGUI' : False,
    'bgColor' : bgColor,
    'screen' : scrn,
    'units' : units,
    'waitBlank':waitBlank
}

if demo: monitorwidth = 23#18.0
if exportImages:
    widthPix = 400; heightPix = 400
    monitorwidth = 13.0
    fullscr=False; scrn=0
if demo:    
    scrn=0; fullscr=False
    widthPix = 800; heightPix = 600
    monitorname='testMonitor'
    allowGUI = True


pixelperdegree = widthPix/ (atan(monitorwidth/viewdist) /np.pi*180)
msg= 'pixelperdegree=' + str( round(pixelperdegree,2) )
logging.info(pixelperdegree)


#letter size 2.5 deg
numLettersToPresent = 24
#For AB, minimum SOAms should be 84  because any shorter, I can't always notice the second ring when lag1.   71 in Martini E2 and E1b (actually he used 66.6 but that's because he had a crazy refresh rate of 90 Hz)
SOAms = 83.25 #82.35 Battelli, Agosta, Goodbourn, Holcombe mostly using 133
letterDurMs = 60 #60

ISIms = SOAms - letterDurMs
letterDurFrames = int( np.floor(letterDurMs / (1000./refreshRate)) )
cueDurFrames = letterDurFrames
print('global cueDurFrames is ' + str(cueDurFrames))
ISIframes = int( np.floor(ISIms / (1000./refreshRate)) )
SOAFrames = int( np.floor(SOAms / (1000./refreshRate)) )

trialDurFrames = int( numLettersToPresent*(ISIframes+letterDurFrames) ) #trial duration in frames

#have set ISIframes and letterDurFrames to integer that corresponds as close as possible to originally intended ms

rateInfo = 'total SOA=' + str(round(  (ISIframes + letterDurFrames)*1000./refreshRate, 2)) + ' or ' + str(ISIframes + letterDurFrames) + ' frames, comprising\n'

rateInfo+=  'ISIframes ='+str(ISIframes)+' or '+str(ISIframes*(1000./refreshRate))+' ms and letterDurFrames ='+str(letterDurFrames)+' or '+str(round( letterDurFrames*(1000./refreshRate), 2))+'ms'

rateInfo+=  '\nThe number of frames in trial is ' + str(trialDurFrames)
logging.info(rateInfo); print(rateInfo)

 





#############################################
#############################################
###### Create a dialog from dictionary ######
#############################################
#############################################

fullscr, subject = setupHelpers.setupDialogue(mon, screenValues, refreshRate, quitFinder, demo)

screenValues['fullscr'] = fullscr

if not fullscr:
    screenValues['widthPix'] = 800
    screenValues['heightPix'] = 600

myWin = setupHelpers.openMyStimWindow(mon, screenValues)


#set up output data file, log file,  copy of program code, and logging
infix = ''
fileName = os.path.join(dataDir, subject + '_' + infix+ timeAndDateStr)
if not demo and not exportImages:
    dataFile = open(fileName+'.txt', 'w')
    saveCodeCmd = 'cp \'' + sys.argv[0] + '\' '+ fileName + '.py'
    os.system(saveCodeCmd)  #save a copy of the code as it was when that subject was run
    logFname = fileName+'.log'
    ppLogF = logging.LogFile(logFname, 
        filemode='w',#if you set this to 'a' it will append instead of overwriting
        level=logging.INFO)#errors, data and warnings will be sent to this logfile
if demo or exportImages: 
  dataFile = sys.stdout; logF = sys.stdout
  logging.console.setLevel(logging.ERROR)  #only show this level  messages and higher
logging.console.setLevel(logging.ERROR) #DEBUG means set  console to receive nearly all messges, INFO next level, EXP, DATA, WARNING and ERROR 

if fullscr and not demo and not exportImages:
    runInfo = psychopy.info.RunTimeInfo(
        # if you specify author and version here, it overrides the automatic detection of __author__ and __version__ in your script
        #author='<your name goes here, plus whatever you like, e.g., your lab or contact info>',
        #version="<your experiment version info>",
        win=myWin,    ## a psychopy.visual.Window() instance; None = default temp window used; False = no win, no win.flips()
        refreshTest='grating', ## None, True, or 'grating' (eye-candy to avoid a blank screen)
        verbose=False, ## True means report on everything 
        userProcsDetailed=True,  ## if verbose and userProcsDetailed, return (command, process-ID) of the user's processes
        #randomSeed='set:42', ## a way to record, and optionally set, a random seed of type str for making reproducible random sequences
            ## None -> default 
            ## 'time' will use experimentRuntime.epoch as the value for the seed, different value each time the script is run
            ##'set:time' --> seed value is set to experimentRuntime.epoch, and initialized: random.seed(info['randomSeed'])
            ##'set:42' --> set & initialize to str('42'), and will give the same sequence of random.random() for all runs of the script

        )
    logging.info(runInfo)
logging.flush()

#create click sound for keyboard
try:
    click=sound.Sound('406__tictacshutup__click-1-d.wav')
except: #in case file missing, create inferiro click manually
    logging.warn('Could not load the desired click sound file, instead using manually created inferior click')
    click=sound.Sound('D',octave=4, sampleRate=22050, secs=0.015, bits=8)

if showRefreshMisses:
    fixSizePix = 18 #2.6  #make fixation bigger so flicker more conspicuous
else: fixSizePix = 6

########################################
####Fixation and instruction stimuli####
########################################

fixColor = [1,1,1]
fixatnPtSize = 4
if exportImages: fixColor= [0,0,0]
fixatnTextureWidth = np.round(fixSizePix/4).astype(int)
fixatnNoiseTexture = np.round( np.random.rand(fixatnTextureWidth,fixatnTextureWidth) ,0 )   *2.0-1 #Can counterphase flicker  noise texture to create salient flicker if you break fixation

fixatn= visual.PatchStim(myWin, tex=fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False)
fixatnCounterphase= visual.PatchStim(myWin, tex= -1*fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False) #reverse contrast
fixatnPoint= visual.Circle(myWin,fillColorSpace='rgb',fillColor=(1,1,1),radius=fixatnPtSize,pos=[0,0],units='pix',autoLog=autoLogging)
                     
respPromptStim = visual.TextStim(myWin,pos=(0, -.8),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
acceptTextStim = visual.TextStim(myWin,pos=(0, -.7),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
acceptTextStim.setText('Hit ENTER to accept. Backspace to edit')
respStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)
clickSound, badKeySound = stringResponse.setupSoundsForResponse()
requireAcceptance = False
nextText = visual.TextStim(myWin,pos=(0, .1),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
NextRemindCountText = visual.TextStim(myWin,pos=(0,.2),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
screenshot= False; screenshotDone = False

instructions1 = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='cm',autoLog=autoLogging)
instructions2 = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='cm',autoLog=autoLogging)

instructionText1 = """
This experiment is made up of several trials. On each trial you will fixate your eyes on a central point on the screen. Then several rapid, randomly-ordered sequences of letters will appear at two, six or 18 locations on the screen. You must not move your eyes from the fixation point while these sequences are playing.

One of the letters will appear with a white ring around it. Your job is to tell us which of the letters appeared within the white ring. Again, you must not move your eyes from the fixation point in the centre of the screen while the letter streams are shown.

Press space to read more instructions
"""

instructionText2 ="""
At the end of each trial you will see a number of letters. You should click on the letter that you saw within the circle.

Please tell the experimenter you have read the instructions. Be sure to ask him if you have any questions. 
"""
instructions1.text = instructionText1
instructions2.text = instructionText2

### Near the rest of the instruction stimuli ###
startTrialStimuli = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='cm',autoLog=autoLogging)
startTrialStimuli.text = 'Click here to start the trial'

startTrialBox = visual.Rect(myWin, height = .75, width = 6, units = 'deg', lineColor = bgColor, lineColorSpace = 'rgb')



######################################
### Buffered image size dimensions ###
######################################

'''
Element array stim textures must be a power of two. I'm doing this here to keep it out of the doRSVPstim loop
'''

new_size = max(  [
                int(np.power(2, np.ceil(np.log(dim_size) / np.log(2))))
                for dim_size in (screenValues['heightPix'], screenValues['widthPix'])
            ]
        )

pad_amounts = []

for dim in ['heightPix','widthPix']:

    first_offset = int((new_size - screenValues[dim]) / 2.0)
    second_offset = new_size - screenValues[dim] - first_offset

    pad_amounts.append([first_offset, second_offset])
    
    
###########################################################
### Mask covering only fixation so fixation can flicker ###
###########################################################


mask = np.full((screenValues['widthPix'],screenValues['heightPix']), 1, int)

mask = np.pad(
    array = mask,
    pad_width=pad_amounts,
    mode="constant",
    constant_values= 1
)

centreX, centreY = [int(dimension/2) for dimension in mask.shape]

mask[centreX-fixSizePix/2:centreX+fixSizePix/2, centreY-fixSizePix/2:centreY+fixSizePix/2] = -(np.round( np.random.rand(fixSizePix,fixSizePix) ,0 )   *2.0-1)


######################################
####### SETTING THE CONDITIONS #######
######################################

#For the optional attentional blink
    
#For the dual-stream simultaneous target
stimList=[]
possibleCueTemporalPositions =  np.array([10]) #debugAH np.array([6,7,8,9,10]) 
tasks=['T1','T1T2','allCued','oneCued','nStreams']
numResponsesWanted=1; maxNumRespsWanted=1
streamsPerRing = 6
pairAngles = range(0,180,int(360/streamsPerRing)) #angles of possible cued streams
nStreamsPossibilities = [2,6,18] #np.arange(2,21,3) #this needs to be listed here so when print header can work out the maximum value
rings = range(int(np.ceil(float(max(nStreamsPossibilities))/streamsPerRing)))


for nStreams in nStreamsPossibilities:
    for cueTemporalPos in possibleCueTemporalPositions: #5 or 6 temporal serial positions for  the cue
        for ring in rings:
            for whichInPair in [0,1]:
                pairAngle = np.random.choice(range(0,180,int(360/streamsPerRing)),1)[0]
                #Randomise it rather than making the demo compute all possible combinations
                halfAngle = 360/float(streamsPerRing)/2.0
                thisRingAngleOffset = (ring % 2) * halfAngle #offset odd-numbered rings by half the angle
                whichStreamCuedAngle = (pairAngle + whichInPair * 180) + thisRingAngleOffset  #either the stream at pairangle or the one opposite
                #print(whichStreamCuedAngle)
                stimList.append(         
                         {'nStreams':nStreams,  
                            'cue0temporalPos':cueTemporalPos, 
                            'pairAngle':pairAngle, 'ring':ring, 'whichStreamCuedAngle' : whichStreamCuedAngle, 'whichInPair':whichInPair}
                            )


trialsPerCondition = 1
trials = data.TrialHandler(stimList,trialsPerCondition) #constant stimuli method
print('There are ' + str(trials.nTotal) + ' trials.')

#################################################################
###Print cued stream to check calculations                    ###
###This iterates the trials object, so the main loop won't run###
#################################################################

testCuedStreams = False

testCuedStreamsResults = {
   '2' : [0]*2,
   '6' : [0]*6,
   '18' : [0]*18
}

if testCuedStreams:
    n = 0
    while n < trials.nTotal:
        trial = trials.next()
        nStreams = trial['nStreams']
        #print(nStreams)

        ring = trial['ring']
        thisRingAngleOffset = (ring % 2) * halfAngle 
        
            
        whichStreamCuedAngle = trial['whichStreamCuedAngle']
        #print('whichStreamCuedAngle = ' + str(whichStreamCuedAngle))
        
        cuedFrame = trial['cue0temporalPos']
        if nStreams > streamsPerRing:
            #print(ring*streamsPerRing)
            cuedStream = (ring*streamsPerRing) + ((whichStreamCuedAngle-thisRingAngleOffset)  / (360/streamsPerRing))
            cuedStream = int(cuedStream)
        elif nStreams == streamsPerRing:
            cuedStream= ((whichStreamCuedAngle-thisRingAngleOffset)  / (360/streamsPerRing))
            cuedStream = int(cuedStream)
        elif nStreams == 2:
            cuedStream = trial['whichInPair']
        #print('nStreams =' + str(nStreams))
        #print('cuedStream =' + str(cuedStream))
        testCuedStreamsResults[str(nStreams)][cuedStream] += 1
        n+=1
    myWin.close()
print(testCuedStreamsResults)

logging.info( ' each trialDurFrames='+str(trialDurFrames)+' or '+str(trialDurFrames*(1000./refreshRate))+ \
               ' ms' )

print('Trials computed')


####################################
#### Print header for data file ####
####################################

print('experimentPhase\ttrialnum\tsubject\t',file=dataFile,end='')
print('nStreams\t',end='',file=dataFile)
print('ring\t',end='',file=dataFile)
printInOrderOfResponses = True
assert (printInOrderOfResponses==True), "Sorry, feature not supported"
if printInOrderOfResponses:
    for i in range(maxNumRespsWanted):
       dataFile.write('resp'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('button'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('cuePos'+str(i)+'\t')
       dataFile.write('answer'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('correct'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('pairAngle'+str(i)+'\t') 
       dataFile.write('whichStreamCuedAngle'+str(i)+'\t')
       dataFile.write('whichRespCue'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('whichInPair'+str(i)+'\t')
       dataFile.write('cuedStream'+str(i)+'\t')
       dataFile.write('responsePosRelative'+str(i)+'\t')
for i in xrange(max(nStreamsPossibilities)):
    dataFile.write('streamLtrSequence'+str(i)+'\t')#have to use write to avoid ' ' between successive text, at least until Python 3
print('timingBlips',file=dataFile)
#end of header

################################
#### Cue and Stream Stimuli ####
################################

for cueN in xrange(maxNumRespsWanted):
    if cueType == 'exogenousRing':
        cue = visual.Circle(myWin, 
                     radius=cueRadius,#Martini used circles with diameter of 12 deg
                     lineColorSpace = 'rgb',
                     lineColor=letterColor,
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
    
#In each stream, predraw all 26 letters
ltrHeight = .9 #This is the cortically-scaled height at 3 degrees of eccentricity. This is what we used in 2vs8
cueOffsets = [3,7,11.5]
maxStreams = max(nStreamsPossibilities)

lettersToExclude = ['C','W'] #lets exclude these

potentialLetters = [letter for letter in string.ascii_uppercase if letter not in lettersToExclude]

streamTextObjects = list() #A text object for every stream. I'll update the text for each frame

nextLargestMultiple = int(np.ceil(float(maxStreams)/streamsPerRing)*streamsPerRing) #The next largest multiple of streamsPerRing after maxStreams
#print('nextLargestMultiple is ' + str(nextLargestMultiple))

for stream in xrange(nextLargestMultiple): 
    thisStream = list()
    for letter in potentialLetters:
        streamText = visual.TextStim(
            myWin,
            pos=(0,0),
            colorSpace='rgb', 
            font = font, 
            color=letterColor,
            alignHoriz='center',
            alignVert='center',
            units='deg',
            text = letter,
            autoLog=autoLogging)
        '''
        Because each stream has a different angular offset from the x axis, I can't use set pos here. 
        In order to scale the object according to magnification - which I'm doing here to save doing it on every trial -
        I need to fake the position of the object so that it's the same eccentricity as it will be later on
        Objects will never been drawn in the position I'm about to calculate. It's just here for the cortical magnification function
        '''
        ringOfThisStim = int(stream/streamsPerRing)
        x = 0
        y = cueOffsets[ringOfThisStim] #We're using cue offsets to set the position of the rings
        
        streamText.pos = (x,y)

        streamText = corticalMagnification.corticalMagnification( #Scale the height (and thus the width for a monospaced font like Sloan) based on cortical magnification estimate. This function returns the stimulus, not its size
            stimulus = streamText,
            ltrHeight = ltrHeight,
            cue = False)
        #print('For stream %(streamN)d and letter %(letter)s the height is %(height)s' % {'streamN':stream, 'letter':letter, 'height':streamText.height})
        thisStream.append(streamText)

    streamTextObjects.append( thisStream )

streamTextObjects = np.array(streamTextObjects)

#print('streamTextObjects shape')
#print(streamTextObjects.shape)
#All noise dot coordinates ultimately in pixels, so can specify each dot is one pixel 
noiseFieldWidthDeg=ltrHeight *0.9  #1.0 makes noise sometimes intrude into circle
noiseFieldWidthPix = int( round( noiseFieldWidthDeg*pixelperdegree ) )

###############
#### Mouse ####
###############
myMouse = event.Mouse()

##############
### Sounds ###
##############

clickSound, badKeySound = stringResponse.setupSoundsForResponse()

###########
###Clock###
###########

trialClock = core.Clock()

################
###Eyetracker###
################

if eyetracking:
    if getEyeTrackingFileFromEyetrackingMachineAtEndOfExperiment:
        eyeMoveFile=('EyeTrack_'+subject+'_'+timeAndDateStr+'.EDF')
    tracker=Tracker_EyeLink(myWin,trialClock,subject,1, 'HV5',(255,255,255),(0,0,0),False,(screenValues['widthPix'],screenValues['heightPix']))


#################
### Functions ###
#################

def calcStreamPos(trial,cueOffsets,streami,streamOrNoise):
    '''
    Calculates the xy position of the stream in degrees of visual angle relative to the centre of the screen
    '''
    #streamOrNoise because noise coordinates have to be in deg, stream in pix
    #cueOffsets are in deg, for instance indicating the eccentricity of the streams/cues
    nStreams = trial['nStreams']
    pairAngle = trial['pairAngle']
    ring = trial['ring']

    noiseOffsetKludge = 0.9 #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg for noise case

    thisRingNum =  int(streami / streamsPerRing)

    ringStreami = streami % streamsPerRing
    
    if nStreams - streamsPerRing * thisRingNum >= streamsPerRing:
        streamsThisRing = streamsPerRing
    else:
        streamsThisRing = nStreams - streamsPerRing * thisRingNum
    
    if nStreams <= streamsPerRing:
        thisRingNum = ring #innermost ring is now the outermost ring
    else:
        pairAngle = 0 #if 18 streams, no need for angular offset
    #print('streams this ring: ',streamsThisRing)



    if nStreams ==0:
        pos = np.array([0,0])
    else:
            #assume want them evenly spaced, counterclockwise starting from directly east
            halfAngle = 360/float(streamsThisRing)/2.0
            thisRingAngleOffset = (thisRingNum % 2) * halfAngle #offset odd-numbered rings by half the angle
            thisAngle = pairAngle + ringStreami/streamsThisRing * 360 + thisRingAngleOffset
            x = cueOffsets[thisRingNum]*cos(thisAngle/180*pi)
            y = cueOffsets[thisRingNum]*sin(thisAngle/180*pi)
            pos = np.array([x,y])
        
    if streamOrNoise:  #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg
        pos *= noiseOffsetKludge*pixelperdegree
        
    #pos = np.round(pos) #rounding or integer is a bad idea. Then for small radii, not equally spaced
    #pos = pos.astype(int)
    return pos

def checkTiming(ts):
    interframeIntervals = np.diff(ts) * 1000
    #print(interframeIntervals)
    frameTimeTolerance=.3 #proportion longer than refreshRate that will not count as a miss
    longFrameLimit = np.round(1000/refreshRate*(1.0+frameTimeTolerance),2)
    idxsInterframeLong = np.where( interframeIntervals > longFrameLimit ) [0] #frames that exceeded 150% of expected duration
    numCasesInterframeLong = len( idxsInterframeLong )
    if numCasesInterframeLong > 0:
        print(numCasesInterframeLong,'frames of', trialDurFrames,'were longer than',str(1000/refreshRate*(1.0+frameTimeTolerance)))
    return numCasesInterframeLong

def oneFrameOfStim(n, frameStimuli):
    thisFrameN = int(n/SOAFrames) #Which object in frameStimuli do we select?
    drawFrame = (n % SOAFrames) < letterDurFrames

    thisFrame = frameStimuli[thisFrameN]

    if drawFrame:
        if n % 2 != 0:
            thisFrame[1].draw() #dimmed fixation
        else:
            thisFrame[0].draw() #Normal fixation
    else:
        if n % 2 == 0:
            fixatnPoint.draw()
        else:
            fixatnPoint.draw()

    return True

def doRSVPStim(trial):
    '''
    ONLY DOES ONE CUE AT THE MOMENT

    This function generates the stimuli for each trial. The word "frame" here refers to a set of simultaneous RSVP stimuli. I'll use "refresh" to refer to monitor refreshes.
    Using the parameters for the current trial:
        - Work out the temporal position of the cue(s)
        - Shuffle the order of the letters
        - Calculate the position and cortically-magnified size for each stream
        - Capture each stream's pixels on each frame using bufferImageStim
        - Collate each frame's pixels so that all stimuli are represented by the same matrix of pixels. Put the cue in there if it's the cued frame
        - pass the matrix of pixels to elementarraystim
    '''
    
    global cue
    global tracker
    
    nStreams = trial['nStreams']
    #print(nStreams)

    ring = trial['ring']
    thisRingAngleOffset = (ring % 2) * halfAngle 
    
    whichStreamCuedAngle = trial['whichStreamCuedAngle']
    
    cuedFrame = trial['cue0temporalPos']
    if nStreams > streamsPerRing:
        #print(ring*streamsPerRing)
        cuedStream = (ring*streamsPerRing) + ((whichStreamCuedAngle-thisRingAngleOffset)  / (360/streamsPerRing))
        cuedStream = int(cuedStream)
    elif nStreams == streamsPerRing:
        cuedStream= ((whichStreamCuedAngle-thisRingAngleOffset)  / (360/streamsPerRing))
        cuedStream = int(cuedStream)
    elif nStreams == 2:
        cuedStream = trial['whichInPair']
    
    #print('cueFrame = ' + str(cuedFrame))
    #print('cuedStream = ' + str(cuedStream))
    #print('whichinPair = ' + str(trial['whichInPair']))
    streamPositions = list() #Might need to pass this to elementArrayStim as xys. Might not though


    streamLetterIdxs = np.empty( #use an array so I can select columns (frames) for buffering. This is an empty array that will eventually have the
                                 #letter indexes for each stream. Selecting a column gives us all streams at a particular frame. Selecting a row gives us
                                 #all frames for a particular stream
        shape = (nStreams,numLettersToPresent),
        dtype = int
        )

    streamLetterIdentities = np.empty( #Letter identities for these streams
        shape = (nStreams,numLettersToPresent),
        dtype = str
        )
       

    for thisStream in xrange(nStreams):
        thisSequence = np.arange(24)
        np.random.shuffle(thisSequence)
        theseIdentities = [potentialLetters[idx] for idx in thisSequence]
        streamLetterIdxs[thisStream,:] = thisSequence
        streamLetterIdentities[thisStream,:] = theseIdentities
        #print('For stream %(streamN)d the letters are: %(theseLetters)s' % {'streamN':thisStream, 'theseLetters':''.join(theseIdentities)})

    #print(streamLetterIdentities)

    correctIdx = streamLetterIdxs[cuedStream,cuedFrame] 
    #print('correctIdx')
    #print(correctIdx)
    correctLetter = alphabetHelpers.numberToLetter(correctIdx, potentialLetters) #potentialLetters is global

    frameStimuli = list() #A list of elementArrayStim objects, each represents a frame. Drawing one of these objects will draw the letters and the cue for that frame

    for thisFrame in xrange(numLettersToPresent):
        theseStimuli = streamLetterIdxs[:,thisFrame] #The alphabetical indexes of stimuli to be shown on this frame
        
        ### IN DO RSVP STIM ###
        stimuliToDraw = list() #Can pass a list to bufferimageStim!
        stimuliToDraw.append(fixatnPoint)
        stimuliToDrawCounterPhase = list()
        stimuliToDrawCounterPhase.append(fixatnPoint)

        for thisStream in xrange(nStreams):
            
            thisPos = calcStreamPos(
                trial = trial, 
                cueOffsets = cueOffsets, 
                streami = thisStream, 
                streamOrNoise = False
                )
            
            cueThisFrame = thisStream == cuedStream and thisFrame == cuedFrame #If true, draw the cue and capture that too
            
            thisLetterIdx = theseStimuli[thisStream] #The letter index for this particular stream on this particular frame
            
            if nStreams <= streamsPerRing and max(nStreamsPossibilities)>2:
                #print('Stream was' + str(thisStream) +', but is now' + str(trial['ring']*streamsPerRing+thisStream))
                thisStreamStimulus = streamTextObjects[trial['ring']*streamsPerRing+thisStream, thisLetterIdx]
            else:
                thisStreamStimulus = streamTextObjects[thisStream, thisLetterIdx]

            thisStreamStimulus.pos = thisPos
            #print('For stream %(thisStream)d the height is: %(letterHeight)s' % {'thisStream':thisStream, 'letterHeight':thisStreamStimulus.height})

            stimuliToDraw.append(thisStreamStimulus)
            stimuliToDrawCounterPhase.append(thisStreamStimulus)

            if cueThisFrame and cueType == 'exogenousRing':
                cue.setPos( thisPos )
                cue = corticalMagnification.corticalMagnification(cue, 0.9810000000000002, cue = True) #this is the cuesize from the original experiment
                stimuliToDraw.append(cue)
                stimuliToDrawCounterPhase.append(cue)
        
        buff = visual.BufferImageStim( #Buffer these stimuli
            win = myWin,
            stim = stimuliToDraw
            )
        
        
        buff = np.flipud(np.array(buff.image)[..., 0]) / 255.0 * 2.0 - 1.0 #Via djmannion. This converts the pixel values from [0,255] to [-1,1]. I think 0 is middle grey. I'll need to change this to match the background colour eventually
        
        buff = np.pad(
            array=buff,
            pad_width=pad_amounts, #See 'Buffered image size dimensions' section
            mode="constant",
            constant_values=0.0
        )
        
        thisFrameStimuli = visual.ElementArrayStim( #A stimulus representing this frame with the fixation at full luminance
            win = myWin,
            units = 'pix',
            nElements=1,
            xys = [[0,0]],
            sizes=buff.shape,
            elementTex=buff,
            elementMask = 'none'
            )
            
        buff = visual.BufferImageStim( #Buffer these stimuli
            win = myWin,
            stim = stimuliToDrawCounterPhase
            )
        
        
        buff = np.flipud(np.array(buff.image)[..., 0]) / 255.0 * 2.0 - 1.0 #Via djmannion. This converts the pixel values from [0,255] to [-1,1]. I think 0 is middle grey. I'll need to change this to match the background colour eventually
        
        buff = np.pad(
            array=buff,
            pad_width=pad_amounts, #See 'Buffered image size dimensions' section
            mode="constant",
            constant_values=0.0
        )
        

        thisFrameStimuliCounterPhase = visual.ElementArrayStim( #A stimulus representing this frame with the fixation phase reversed
            win = myWin,
            units = 'pix',
            nElements=1,
            xys = [[0,0]],
            sizes=buff.shape,
            elementTex=buff,
            elementMask = 'none'
            )        

        frameStimuli.append([thisFrameStimuli, thisFrameStimuliCounterPhase])

    ts = []
    
    waiting = True
    myMouse.setVisible(waiting)
    
    if nDone is not 0:
        if  trials.nTotal/float(nDone) in [1.,1.5,2.,3.]:
            startTrialStimuli.text = startTrialStimuli.text + '\n\rThis is trial ' + str(nDone) + ' of ' + str(trials.nTotal)    
        else:
            startTrialStimuli.text = 'Click here to start the trial'
        
    while waiting and not autopilot:
        startTrialStimuli.draw()
        startTrialBox.draw()
        myWin.flip()
        if myMouse.isPressedIn(startTrialBox):
            waiting = False

    myMouse.setVisible(waiting)

    if eyetracking: 
        tracker.startEyeTracking(nDone,True,widthPix,heightPix) #start recording with eyetracker  

    ts = []
    myWin.flip(); myWin.flip()#Make sure raster at top of screen (unless not in blocking mode), and give CPU a chance to finish other tasks
    fixatnPoint.draw()
    myWin.flip()
    core.wait(1)
    t0 = trialClock.getTime()
    for n in xrange(trialDurFrames):
        oneFrameOfStim(n, frameStimuli)
        myWin.flip()
        core.wait(.01) #mimics 100Hz refresh
        ts.append(trialClock.getTime() - t0)

    if eyetracking:
        tracker.stopEyeTracking()
        print('stopped tracking')

    return streamLetterIdxs, streamLetterIdentities, correctLetter, ts, cuedStream, cuedFrame


instructions1.draw()
myWin.flip()
waiting = True
while waiting:
   for key in event.getKeys():      #check if pressed abort-type key
         if key in ['space','ESCAPE']: 
            waiting=False
         if key in ['ESCAPE']:
            expStop = True

instructions2.draw()
myWin.flip()
waiting = True
while waiting:
    for key in event.getKeys():
        if key in ['m', 'ESCAPE']:
            waiting = False
        if key in ['Escape']:
            expStop = True
myWin.flip()



allBlips = list()
expStop = False #If True, end experiment
nDone = 0 #Which trial?

while nDone < trials.nTotal and not expStop:
    trial = trials.next()
    showBothSides = False #Need to modify this if doing 2 streams only, that way we can replicate the G&H lineups
    sideFirstLeftRightCentral=2 #default , respond to central. Charlie: I guess we need this to replicate other experiments

    streamLetterIdxs, streamLetterIdentities, correctLetter, ts, cuedStream, cuePos = doRSVPStim(trial)
    myMouse.setVisible(True)
    if not autopilot:
        expStop,passThisTrial,responses,buttons,responsesAutopilot = \
                letterLineupResponse.doLineup( #doLineup(myWin,bgColor,myMouse,clickSound,badClickSound,possibleResps,bothSides,leftRightCentral,autopilot):
                myWin,
                bgColor,
                myMouse,
                clickSound,
                badKeySound,
                potentialLetters,
                showBothSides,
                sideFirstLeftRightCentral,
                autopilot
                )
        myWin.flip()
    else:
        responses = ['Z']
        buttons = [0]
    accuracy = responses[0] == correctLetter
    #print('printing accuracy vars')
    #print(streamLetterIdentities[cuedStream,:])
    #print(responses[0])
    #print(np.where(streamLetterIdentities[cuedStream,:]==responses[0])[0][0])
    responseLetterIdx = np.where(streamLetterIdentities[cuedStream,:]==responses[0]) #need index on where because it treats sliced streamLetterIdentities as a ndarray
    if len(responseLetterIdx) is not 0:
        responseLetterIdx = responseLetterIdx[0]
        SPE = responseLetterIdx[0] - cuePos
        #print('SPE ' + str(SPE))
    else:
        responseLetterIdx = 999
        SPE = 999
    
    cuedStreamPos = calcStreamPos(trial, cueOffsets, cuedStream, False)
    #print(cuedStreamPos)
    cuedStreamAngle = atan2(cuedStreamPos[1],cuedStreamPos[0])*(180/np.pi)
    #print(cuedStreamAngle)

    timingBlips = checkTiming(ts)
    allBlips.append(timingBlips)

    dataFile.write(
        'main\t' + 
        str(nDone) + '\t' + 
        subject + '\t' +
        str(trial['nStreams']) + '\t' +
        str(trial['ring']) + '\t' +
        responses[0] + '\t' +
        str(buttons[0]) + '\t' +
        str(cuePos) + '\t' +
        correctLetter + '\t' +
        str(accuracy) + '\t' +
        str(trial['pairAngle']) + '\t' +
        str(trial['whichStreamCuedAngle']) + '\t' +
        '0' + '\t' +
        str(trial['whichInPair'])+'\t'+
        str(cuedStream)+'\t'+
        str(SPE) + '\t'
        )

    for stream in xrange(max(nStreamsPossibilities)):
        if stream < trial['nStreams']:
            dataFile.write(''.join(streamLetterIdentities[stream,:]) + '\t')
        else:
            dataFile.write('999\t')
            
    dataFile.write(str(timingBlips) + '\n')
    dataFile.flush()
    nDone += 1
dataFile.flush()
dataFile.close()
myWin.close()

