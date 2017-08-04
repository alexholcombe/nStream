from __future__ import print_function, division
from psychopy import monitors, visual, event, data, logging, core, sound, gui
import time, sys, os
from timeit import default_timer as timer
from math import atan, cos, sin, pi
import numpy as np
import string
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
refreshRate = 60
if demo:
	refreshRate = 60.;  #100 LN: refresh rate for previous AB and RSVP task for gamers was 60

font = 'sloan'

threshCriterion = 0.58
bgColor = [-.7,-.7,-.7] # [-1,-1,-1]
cueColor = [1.,1.,1.]
cueType = 'exogenousRing' #'lowerCase' #'exogenousRing' #'endogenous':
if cueType == 'endogenous':
	cueColor = [1,-1,-1]
letterColor = [1.,0.,0.]
cueRadius = 2.5 #6 deg, as in Martini E2    Letters should have height of 2.5 deg

#There's no lowercase in sloan
if cueType is 'lowerCase':
	font = 'Arial'

viewdist = 57. #cm

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
	'fullscr':False, #True to use fullscreen, False to not. Timing probably won't be quite right if fullscreen = False
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
SOAms = 600 #82.35 Battelli, Agosta, Goodbourn, Holcombe mostly using 133
letterDurMs = 500 #60

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

trialDurFrames = int( numLettersToPresent*(ISIframes+letterDurFrames) ) #trial duration in frames
 





#############################################
#############################################
###### Create a dialog from dictionary ######
#############################################
#############################################

fullscr, subject = setupHelpers.setupDialogue(mon, screenValues, refreshRate, quitFinder, demo)

if not fullscr:
	screenValues['widthPix'] = 800
	screenValues['heightPix'] = 600

myWin = setupHelpers.openMyStimWindow(mon, screenValues)


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
This experiment is made up of 200 trials. On each trial you will fixate your eyes on a central point on the screen.Then several rapid, randomly-ordered sequences of letters will appear at two or 8 locations on the screen.You must not move your eyes from the fixation point while these sequences are playing.

One of the letters will appear with a white ring around it.Your job is to tell us which of the letters appeared within the white ring. Again, you must not move your eyes from the fixation point in the centre of the screen while the letter streams are shown.

Press space to read more instructions
"""

instructionText2 ="""
At the end of each trial you will see a screen in which the whole alphabet is displayed. You should click on the letter that you saw within the circle.

However, you may have been UNSURE about the letter that you saw. If you were UNSURE about the letter you saw, click the letter using the LEFT mouse button. The letter you selected will turn YELLOW. If you were SURE about  the letter that you saw, click the letter with the RIGHT mouse button. The letter you clicked will turn GREEN.\n

Please tell the experimenter you have read the instructions. Be sure to ask him if you have any questions. 
"""
instructions1.text = instructionText1
instructions2.text = instructionText2

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



################################
#### Cue and Stream Stimuli ####
################################
maxNumRespsWanted = 1

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
nStreamsPossibilities = np.arange(2,21,3)
nStreamsPossibilities = np.append(nStreamsPossibilities, 21)
ltrHeight = .9 #This is the cortically-scaled height at 3 degrees of eccentricity. This is what we used in 2vs8
cueOffsets = [3,7,11.5]
maxStreams = max(nStreamsPossibilities)
streamsPerRing = 7

lettersToExclude = ['C','W'] #lets exclude these

potentialLetters = [letter for letter in string.ascii_uppercase if letter not in lettersToExclude]

streamTextObjects = list() #A text object for every stream. I'll update the text for each frame

for stream in xrange(max(nStreamsPossibilities)):
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
		thisStream.append(streamText)

	streamTextObjects.append( thisStream )

streamTextObjects = np.array(streamTextObjects)

print('streamTextObjects shape')
print(streamTextObjects.shape)
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

#################
### Functions ###
#################

def calcStreamPos(nStreams, baseAngleCWfromEast,cueOffsets,streami,streamOrNoise):
	'''
	Calculates the xy position of the stream in degrees of visual angle relative to the centre of the screen
	'''
	#streamOrNoise because noise coordinates have to be in deg, stream in pix
	#cueOffsets are in deg, for instance indicating the eccentricity of the streams/cues

	noiseOffsetKludge = 0.9 #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg for noise case

	thisRingNum =  int(streami / streamsPerRing)

	ringStreami = streami % streamsPerRing

	
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

##################
###Timing tests###
##################

nReps = 4
anglesMustBeMultipleOf =  int( round(360 / max(nStreamsPossibilities) ) )
possibleAngles = range(0,360,anglesMustBeMultipleOf)
possibleCueTemporalPositions =  np.array([6,7,8,9,10])
numTargets = 1


timing = np.full(shape = (len(nStreamsPossibilities), nReps), fill_value = -1)

for row, nStreams in enumerate(nStreamsPossibilities):
	print('Row is ' + str(row))
	print('Nstreams is ' + str(nStreams))
	for repetition in range(nReps):
		baseAngleCWfromEast = np.random.choice(possibleAngles, 1)
		print('baseAngleCWfromEast is' + str(baseAngleCWfromEast))
		cuedFrame = np.random.choice(possibleCueTemporalPositions, 1)
		cuedStream = np.random.choice(np.arange(nStreams), 1)

		start = timer()

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

		correctIdx = streamLetterIdxs[cuedStream,cuedFrame] 
		print('correctIdx')
		print(correctIdx)
		correctLetter = alphabetHelpers.numberToLetter(correctIdx, potentialLetters) #potentialLetters is global

		frameStimuli = list() #A list of elementArrayStim objects, each represents a frame. Drawing one of these objects will draw the letters and the cue for that frame

		for thisFrame in xrange(numLettersToPresent):
			theseStimuli = streamLetterIdxs[:,thisFrame] #The alphabetical indexes of stimuli to be shown on this frame
			
			stimuliToDraw = list() #Can pass a list to bufferimageStim!
			stimuliToDraw.append(fixatn)

			for thisStream in xrange(nStreams):
				cueThisFrame = thisStream == cuedStream and thisFrame == cuedFrame #If true, draw the cue and capture that too

				thisLetterIdx = theseStimuli[thisStream] #The letter index for this particular stream on this particular frame
				
				thisStreamStimulus = streamTextObjects[thisStream,thisLetterIdx] #The text object for this stream
				
				thisPos = calcStreamPos(
					nStreams = nStreams, 
                   baseAngleCWfromEast = baseAngleCWfromEast,
					cueOffsets = cueOffsets, 
					streami = thisStream, 
					streamOrNoise = False
					)

				thisStreamStimulus.pos = thisPos

				stimuliToDraw.append(thisStreamStimulus)

				if cueThisFrame and cueType == 'exogenousRing':
					cue.setPos( thisPos )
					cue = corticalMagnification.corticalMagnification(cue, 0.9810000000000002, cue = True) #this is the cuesize from the original experiment
					stimuliToDraw.append(cue)
			
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
			
			thisFrameStimuliNormalFix = visual.ElementArrayStim( #A stimulus representing this frame with the fixation at full luminance
				win = myWin,
				units = 'pix',
				nElements=1,
				xys = [[0,0]],
				sizes=buff.shape,
				elementTex=buff,
				elementMask = 'none'
				)

			thisFrameStimuliDimFix = visual.ElementArrayStim( #A stimulus representing this frame with the fixation at half luminance
				win = myWin,
				units = 'pix',
				nElements=1,
				xys = [[0,0]],
				sizes=buff.shape,
				elementTex=buff,
				elementMask = mask
				)        

			frameStimuli.append([thisFrameStimuliNormalFix, thisFrameStimuliDimFix])

		end = timer()
		timing[row,repetition] = end - start

print(np.mean(timing, axis = 1))

print(timing)



