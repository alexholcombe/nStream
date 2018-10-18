#Alex Holcombe alex.holcombe@sydney.edu.au
#See the README.md for more information: https://github.com/alexholcombe/attentional-blink/blob/master/README.md
#git remote add origin https://github.com/alexholcombe/nStream
from __future__ import print_function, division
from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import numpy as np
from math import atan, log, ceil, cos, sin, pi, atan2, tan
from copy import deepcopy
import time, sys, os#, pylab
import string, random

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
if os.path.isdir('.'+os.sep+'rawData'):
    dataDir='rawData'
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
bgColor = [-.7,-.7,-.7] # [-1,-1,-1]
cueColor = [1.,1.,1.]
cueType = 'exogenousRing' #'lowerCase' #'exogenousRing' #'endogenous':
if cueType == 'endogenous':
    cueColor = [1,-1,-1]
letterColor = [1.,1.,1.]
cueRadius = 2.5 #6 deg, as in Martini E2    Letters should have height of 2.5 deg

#There's no lowercase in sloan
if cueType is 'lowerCase':
    font = 'Arial'

viewdist = 57 #cm

monitorname = 'testmonitor'

waitBlank = False

widthPix = 1024 #monitor width in pixels of Agosta
heightPix = 768 #800 #monitor height in pixels
monitorwidth = 40.5

mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#relying on  monitorwidth cm (39 for Mitsubishi to do deg calculations) and gamma info in calibratn

mon.setSizePix( (widthPix,heightPix) )

units='deg' #'cm'

scrn = 1

doStaircase = False

screenValues = {
    'widthPix': 1024, #monitor width in pixels of Agosta
    'heightPix': 768, #800 #monitor height in pixels
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


#have set ISIframes and letterDurFrames to integer that corresponds as close as possible to originally intended ms

rateInfo = 'total SOA=' + str(round(  (ISIframes + letterDurFrames)*1000./refreshRate, 2)) + ' or ' + str(ISIframes + letterDurFrames) + ' frames, comprising\n'

rateInfo+=  'ISIframes ='+str(ISIframes)+' or '+str(ISIframes*(1000./refreshRate))+' ms and letterDurFrames ='+str(letterDurFrames)+' or '+str(round( letterDurFrames*(1000./refreshRate), 2))+'ms'

logging.info(rateInfo); print(rateInfo)
 





#############################################
#############################################
###### Create a dialog from dictionary ######
#############################################
#############################################

fullscr, subject = setupHelpers.setupDialogue(mon, screenValues, refreshRate, quitFinder, demo)



trialDurFrames = int( numLettersToPresent*(ISIframes+letterDurFrames) ) #trial duration in frames


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
requireAcceptance = False
nextText = visual.TextStim(myWin,pos=(0, .1),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
NextRemindCountText = visual.TextStim(myWin,pos=(0,.2),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
screenshot= False; screenshotDone = False

instructions1 = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='deg',autoLog=autoLogging)
instructions2 = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='deg',autoLog=autoLogging)

instructionText1 = """
This experiment is made up of several trials. On each trial you will fixate your eyes on a central point on the screen. Then several rapid, randomly-ordered sequences of letters will appear at two or 18 locations on the screen. You must not move your eyes from the fixation point while these sequences are playing.

One of the letters will appear with a white ring around it. Your job is to tell us which of the letters appeared within the white ring. Again, you must not move your eyes from the fixation point in the centre of the screen while the letter streams are shown.

Press space to read more instructions
"""

instructionText2 ="""
At the end of each trial you will see a screen in which the whole alphabet is displayed. You should click on the letter that you saw within the circle.

Please tell the experimenter you have read the instructions. Be sure to ask him if you have any questions. 
"""
instructions1.text = instructionText1
instructions2.text = instructionText2

### Near the rest of the instruction stimuli ###
startTrialStimuli = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.5,units='deg',autoLog=autoLogging)
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

mask[centreX-fixSizePix/2:centreX+fixSizePix/2, centreY-fixSizePix/2:centreY+fixSizePix/2] = - (np.round( np.random.rand(fixSizePix,fixSizePix) ,0 )   *2.0-1)

streamsPerRing = 6
nStreams = 18


#In each stream, predraw all 26 letters
ltrHeight = 1 #This is the cortically-scaled height at 3 degrees of eccentricity. This is what we used in 2vs8
cueOffsets = [3,7,11.5]
edgeRadii = [1.3, 1.95, 2.8] #.1E + .5ltrheight + .5
scaledLtrHeights = list()
letterObjects = list()
cueObjects = list()

def calcStreamPos(thisStream, streamsPerRing):
    thisRingNum =  int(thisStream / streamsPerRing)

    ringStreami = thisStream % streamsPerRing
    
    if nStreams - streamsPerRing * thisRingNum >= streamsPerRing:
        streamsThisRing = streamsPerRing
    else:
        streamsThisRing = nStreams - streamsPerRing * thisRingNum
    
    if nStreams == 2:
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
    return pos

for thisStream in range(nStreams):
    thisPos = calcStreamPos(thisStream, streamsPerRing)

    dummyStimulus = streamText = visual.TextStim(
                myWin,
                pos=thisPos,
                colorSpace='rgb', 
                font = font, 
                color=letterColor,
                alignHoriz='center',
                alignVert='center',
                units='deg',
                text = 'X',
                autoLog=autoLogging)

    dummyStimulus = corticalMagnification.corticalMagnification(streamText, ltrHeight, cue = False, sizeOut = False)
    dummySize = corticalMagnification.corticalMagnification(dummyStimulus, ltrHeight, cue = False, sizeOut = True)
    scaledLtrHeights.append(dummySize)
    letterObjects.append(streamText)
print('scaledLtrHeights =', scaledLtrHeights)
boumaRadii = [cueOffsets[i]*.5+.5*scaledLtrHeights[i]+1 for i in range(3)]

cuedRings = [0,10,13]

for index in range(3):
    cuedRing = cuedRings[index]
    thisPos = calcStreamPos(cuedRing, streamsPerRing)
    cue =  visual.Circle(myWin, 
                radius=boumaRadii[index],#Martini used circles with diameter of 12 deg
                lineColorSpace = 'rgb',
                lineColor=letterColor,
                lineWidth=2.0, #in pixels
                units = 'deg',
                fillColorSpace = 'rgb',
                fillColor=None, #beware, with convex shapes fill colors don't work
                pos= thisPos, #the anchor (rotation and vertices are position with respect to this)
                interpolate=True,
                autoLog=False)
    cue = corticalMagnification.corticalMagnification(cue, 0.9810000000000002, cue = True, sizeOut = False)
    cueObjects.append(cue)
    
#Stimulus sizes on screen in CM
sizeOneRadians = scaledLtrHeights[0]*(pi/180)
sizeOneCM = tan(sizeOneRadians)*viewdist

sizeTwoRadians = scaledLtrHeights[streamsPerRing]*(pi/180)
sizeTwoCM = tan(sizeTwoRadians)*viewdist

sizeThreeRadians = scaledLtrHeights[streamsPerRing*2]*(pi/180)
sizeThreeCM = tan(sizeThreeRadians)*viewdist

sizeOneText = visual.TextStim(
    win = myWin,
    pos = [-15,9],
    colorSpace='rgb', 
    color=letterColor,
    alignHoriz='center',
    alignVert='center',
    units='deg',
    text = 'Ring 0 = ' + str(sizeOneCM),
    autoLog=autoLogging,
    height = 1
)

sizeTwoText = visual.TextStim(
    win = myWin,
    pos = [-15,7.75],
    colorSpace='rgb', 
    color=letterColor,
    alignHoriz='center',
    alignVert='center',
    units='deg',
    text = 'Ring 1 = ' + str(sizeTwoCM),
    autoLog=autoLogging,
    height = 1
)

sizeThreeText = visual.TextStim(
    win = myWin,
    pos = [-15,6.5],
    colorSpace='rgb', 
    color=letterColor,
    alignHoriz='center',
    alignVert='center',
    units='deg',
    text = 'Ring 2 = ' + str(sizeThreeCM),
    autoLog=autoLogging,
    height = 1
)

for index in range(18):
    letterObjects[index].draw()
for index in range(3):
    cueObjects[index].draw()
sizeOneText.draw()
sizeTwoText.draw()
sizeThreeText.draw()
myWin.flip()

displaying = True
while displaying:
	for key in event.getKeys():
		if key == 'm':
			displaying = False
myWin.close()
core.quit()