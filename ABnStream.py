#Alex Holcombe alex.holcombe@sydney.edu.au
#See the README.md for more information: https://github.com/alexholcombe/attentional-blink/blob/master/README.md
#git remote add origin https://github.com/alexholcombe/nStream
from __future__ import print_function, division
#from __future__ import division
from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import numpy as np
from math import atan, log, ceil, cos, sin, pi
from copy import deepcopy
import time, sys, os#, pylab
import string, random
try:
    from noiseStaircaseHelpers import printStaircase, toStaircase, outOfStaircase, createNoise, plotDataAndPsychometricCurve
except ImportError:
    print('Could not import from noiseStaircaseHelpers.py (you need that file to be in the same directory)')
try: import stringResponse
except ImportError:  print('Could not import strongResponse.py (you need that file to be in the same directory)')
try: import letterLineupResponse
except ImportError:  print('Could not import letterLineupResponse.py (you need that file to be in the same directory)')
descendingPsycho = True
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

showRefreshMisses=True #flicker fixation at refresh rate, to visualize if frames missed
feedback=True
autoLogging=False
if demo:
    refreshRate = 60.;  #100 LN: refresh rate for previous AB and RSVP task for gamers was 60

staircaseTrials = 25
prefaceStaircaseTrialsN = 20 #22
prefaceStaircaseNoise = np.array([5,20,20,20, 50,50,50,5,80,80,80,5,95,95,95]) #will be recycled / not all used, as needed
threshCriterion = 0.58
bgColor = [-.7,-.7,-.7] # [-1,-1,-1]
cueColor = [1.,1.,1.]
letterColor = [1.,1.,1.]
cueRadius = 2.5 #6 deg, as in Martini E2    Letters should have height of 2.5 deg

widthPix= 1280 #monitor width in pixels of Agosta
heightPix= 1024 #800 #monitor height in pixels
monitorwidth = 40.5 #monitor width in cm
scrn=1 #0 to use main screen, 1 to use external screen connected to computer
fullscr=False #True to use fullscreen, False to not. Timing probably won't be quite right if fullscreen = False
allowGUI = False
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
viewdist = 57. #cm
pixelperdegree = widthPix/ (atan(monitorwidth/viewdist) /np.pi*180)
msg= 'pixelperdegree=' + str( round(pixelperdegree,2) )
logging.info(pixelperdegree)
    
# create a dialog from dictionary 
infoFirst = { 'Do staircase (only)': False, 'Check refresh etc':False, 'Fullscreen (timing errors if not)': False, 'Screen refresh rate': 60 }
OK = gui.DlgFromDict(dictionary=infoFirst, 
    title='AB or dualstream experiment OR staircase to find thresh noise level for T1 performance criterion', 
    order=['Do staircase (only)', 'Check refresh etc', 'Fullscreen (timing errors if not)'], 
    tip={'Check refresh etc': 'To confirm refresh rate and that can keep up, at least when drawing a grating'},
    #fixed=['Check refresh etc'])#this attribute can't be changed by the user
    )
if not OK.OK:
    print('User cancelled from dialog box'); core.quit()
doStaircase = infoFirst['Do staircase (only)']
checkRefreshEtc = infoFirst['Check refresh etc']
fullscr = infoFirst['Fullscreen (timing errors if not)']
refreshRate = infoFirst['Screen refresh rate']
if checkRefreshEtc:
    quitFinder = True 
if quitFinder:
    import os
    applescript="\'tell application \"Finder\" to quit\'"
    shellCmd = 'osascript -e '+applescript
    os.system(shellCmd)

#letter size 2.5 deg
numLettersToPresent = 26
SOAms =  233#133 #Battelli, Agosta, Goodbourn, Holcombe mostly using 133
#Minimum SOAms should be 84  because any shorter, I can't always notice the second ring when lag1.   71 in Martini E2 and E1b (actually he used 66.6 but that's because he had a crazy refresh rate of 90 Hz)
letterDurMs = 80 #23.6  in Martini E2 and E1b (actually he used 22.2 but that's because he had a crazy refresh rate of 90 Hz)

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
def openMyStimWindow(): #make it a function because have to do it several times, want to be sure is identical each time
    myWin = visual.Window(monitor=mon,size=(widthPix,heightPix),allowGUI=allowGUI,units=units,color=bgColor,colorSpace='rgb',fullscr=fullscr,screen=scrn,waitBlanking=waitBlank) #Holcombe lab monitor
    return myWin
myWin = openMyStimWindow()
refreshMsg2 = ''
if not checkRefreshEtc:
    refreshMsg1 = 'REFRESH RATE WAS NOT CHECKED'
    refreshRateWrong = False
else: #checkRefreshEtc
    runInfo = psychopy.info.RunTimeInfo(
            # if you specify author and version here, it overrides the automatic detection of __author__ and __version__ in your script
            #author='<your name goes here, plus whatever you like, e.g., your lab or contact info>',
            #version="<your experiment version info>",
            win=myWin,    ## a psychopy.visual.Window() instance; None = default temp window used; False = no win, no win.flips()
            refreshTest='grating', ## None, True, or 'grating' (eye-candy to avoid a blank screen)
            verbose=True, ## True means report on everything 
            userProcsDetailed=True  ## if verbose and userProcsDetailed, return (command, process-ID) of the user's processes
            )
    #print(runInfo)
    logging.info(runInfo)
    print('Finished runInfo- which assesses the refresh and processes of this computer') 
    #check screen refresh is what assuming it is ##############################################
    Hzs=list()
    myWin.flip(); myWin.flip();myWin.flip();myWin.flip();
    myWin.setRecordFrameIntervals(True) #otherwise myWin.fps won't work
    print('About to measure frame flips') 
    for i in range(50):
        myWin.flip()
        Hzs.append( myWin.fps() )  #varies wildly on successive runs!
    myWin.setRecordFrameIntervals(False)
    # end testing of screen refresh########################################################
    Hzs = np.array( Hzs );     Hz= np.median(Hzs)
    msPerFrame= 1000./Hz
    refreshMsg1= 'Frames per second ~='+ str( np.round(Hz,1) )
    refreshRateTolerancePct = 3
    pctOff = abs( (np.median(Hzs)-refreshRate) / refreshRate)
    refreshRateWrong =  pctOff > (refreshRateTolerancePct/100.)
    if refreshRateWrong:
        refreshMsg1 += ' BUT'
        refreshMsg1 += ' program assumes ' + str(refreshRate)
        refreshMsg2 =  'which is off by more than' + str(round(refreshRateTolerancePct,0)) + '%!!'
    else:
        refreshMsg1 += ', which is close enough to desired val of ' + str( round(refreshRate,1) )
    myWinRes = myWin.size
    myWin.allowGUI =True
myWin.close() #have to close window to show dialog box

defaultNoiseLevel = 0#90.0 #to use if no staircase, can be set by user
dlgLabelsOrdered = list()
if doStaircase:
    myDlg = gui.Dlg(title="Staircase to find appropriate noisePercent", pos=(200,400))
else:
    myDlg = gui.Dlg(title="RSVP experiment", pos=(200,400))
if not autopilot:
    myDlg.addField('Subject name (default="Hubert"):', 'Hubert', tip='or subject code')
    dlgLabelsOrdered.append('subject')
if doStaircase:
    easyTrialsCondText = 'Num preassigned noise trials to preface staircase with (default=' + str(prefaceStaircaseTrialsN) + '):'
    myDlg.addField(easyTrialsCondText, tip=str(prefaceStaircaseTrialsN))
    dlgLabelsOrdered.append('easyTrials')
    myDlg.addField('Staircase trials (default=' + str(staircaseTrials) + '):', tip="Staircase will run until this number is reached or it thinks it has precise estimate of threshold")
    dlgLabelsOrdered.append('staircaseTrials')
    pctCompletedBreak = 101
else:
    myDlg.addField('\tPercent noise dots=',  defaultNoiseLevel, tip=str(defaultNoiseLevel))
    dlgLabelsOrdered.append('defaultNoiseLevel')
    #myDlg.addField('Trials per condition (default=' + str(trialsPerCondition) + '):', trialsPerCondition, tip=str(trialsPerCondition))
    #dlgLabelsOrdered.append('trialsPerCondition')
    pctCompletedBreak = 50
    
myDlg.addText(refreshMsg1, color='Black')
if refreshRateWrong:
    myDlg.addText(refreshMsg2, color='Red')
if refreshRateWrong:
    logging.error(refreshMsg1+refreshMsg2)
else: logging.info(refreshMsg1+refreshMsg2)

if checkRefreshEtc and (not demo) and (myWinRes != [widthPix,heightPix]).any():
    msgWrongResolution = 'Screen apparently NOT the desired resolution of '+ str(widthPix)+'x'+str(heightPix)+ ' pixels!!'
    myDlg.addText(msgWrongResolution, color='Red')
    logging.error(msgWrongResolution)
    print(msgWrongResolution)
dimGreyForDlgBox = 'DimGrey'
if psychopy.__version__ != '1.84.2': #really want to cover multiple versions, but would need to import distutils.version for that
    dimGreyForDlgBox = [-1.,1.,-1.] #color names stopped working along the way, for unknown reason
myDlg.addText('Note: to abort press ESC at a trials response screen', color=dimGreyForDlgBox) 
myDlg.show()

if myDlg.OK: #unpack information from dialogue box
   thisInfo = myDlg.data #this will be a list of data returned from each field added in order
   if not autopilot:
       name=thisInfo[dlgLabelsOrdered.index('subject')]
       if len(name) > 0: #if entered something
         subject = name #change subject default name to what user entered
   if doStaircase:
       if len(thisInfo[dlgLabelsOrdered.index('staircaseTrials')]) >0:
           staircaseTrials = int( thisInfo[ dlgLabelsOrdered.index('staircaseTrials') ] ) #convert string to integer
           print('staircaseTrials entered by user=',staircaseTrials)
           logging.info('staircaseTrials entered by user=',staircaseTrials)
       if len(thisInfo[dlgLabelsOrdered.index('easyTrials')]) >0:
           prefaceStaircaseTrialsN = int( thisInfo[ dlgLabelsOrdered.index('easyTrials') ] ) #convert string to integer
           print('prefaceStaircaseTrialsN entered by user=',thisInfo[dlgLabelsOrdered.index('easyTrials')])
           logging.info('prefaceStaircaseTrialsN entered by user=',prefaceStaircaseTrialsN)
   else: #not doing staircase
       #trialsPerCondition = int( thisInfo[ dlgLabelsOrdered.index('trialsPerCondition') ] ) #convert string to integer
       #print('trialsPerCondition=',trialsPerCondition)
       defaultNoiseLevel = int (thisInfo[ dlgLabelsOrdered.index('defaultNoiseLevel') ])
else: 
   print('User cancelled from dialog box.')
   logging.flush()
   core.quit()
if not demo: 
    allowGUI = False

myWin = openMyStimWindow()
#set up output data file, log file,  copy of program code, and logging
infix = ''
if doStaircase:
    infix = 'staircase_'
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
    fixSizePix = 32 #2.6  #make fixation bigger so flicker more conspicuous
else: fixSizePix = 32
fixColor = [1,1,1]
if exportImages: fixColor= [0,0,0]
fixatnTextureWidth = np.round(fixSizePix/4).astype(int)
fixatnNoiseTexture = np.round( np.random.rand(fixatnTextureWidth,fixatnTextureWidth) ,0 )   *2.0-1 #Can counterphase flicker  noise texture to create salient flicker if you break fixation

fixatn= visual.PatchStim(myWin, tex=fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False)
fixatnBlank= visual.PatchStim(myWin, tex= -1*fixatnNoiseTexture, size=(fixSizePix,fixSizePix), units='pix', mask='circle', interpolate=False, autoLog=False) #reverse contrast
fixatnPoint= visual.PatchStim(myWin,tex='none',colorSpace='rgb',color=(1,1,1),size=10,units='pix',autoLog=autoLogging)

respPromptStim = visual.TextStim(myWin,pos=(0, -.8),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
acceptTextStim = visual.TextStim(myWin,pos=(0, -.7),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
acceptTextStim.setText('Hit ENTER to accept. Backspace to edit')
respStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)
clickSound, badKeySound = stringResponse.setupSoundsForResponse()
requireAcceptance = False
nextText = visual.TextStim(myWin,pos=(0, .1),colorSpace='rgb',color = (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
NextRemindCountText = visual.TextStim(myWin,pos=(0,.2),colorSpace='rgb',color= (1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
screenshot= False; screenshotDone = False

#SETTING THE CONDITIONS
#For the optional attentional blink
    
#For the dual-stream simultaneous target
stimListDualStream=[]
possibleCueTemporalPositions =  np.array([6]) #debugAH np.array([6,7,8,9,10]) 
tasks=['T1','T1T2','allCued','oneCued']
numResponsesWanted=1; maxNumRespsWanted=1
numStreamPossibilities = np.array([4]) #this needs to be listed here so when print header can work out the maximum value
for numStreams in numStreamPossibilities:
    for task in [ tasks[3] ]:  #T1 task is just for the single-target tasks, but both streams are presented
       if task=='T1T2':
            numResponsesWanted=2; numToCue=-999
       elif task=='allCued':
            numToCue = numStreams
       elif task=='oneCued':
            numToCue = 1
       print('task=',task)
       for targetLeftRightIfOne in  ['left','right']: #If single target, should it be on the left or the right?
        for cueTemporalPos in possibleCueTemporalPositions:
          for firstRespLRifTwo in ['left','right']:  #If dual target and lineup response, should left one or right one be queried first?
            stimListDualStream.append( 
                 {'numStreams':numStreams, 'numRespsWanted':numResponsesWanted, 'task':task, 'targetLeftRightIfOne':targetLeftRightIfOne, 
                    'cue0temporalPos':cueTemporalPos, 'firstRespLRifTwo': firstRespLRifTwo, 'cue1lag':0,'numToCue':numToCue } 
              )  #cue1lag = 0, meaning simultaneous targets

trialsPerConditionDualStream = 1 #10 #max(1, trialsAB.nTotal / len(stimListDualStream) )
trialsDualStream = data.TrialHandler(stimListDualStream,trialsPerConditionDualStream) #constant stimuli method

logging.info( ' each trialDurFrames='+str(trialDurFrames)+' or '+str(trialDurFrames*(1000./refreshRate))+ \
               ' ms' )

def numberToLetter(number): #0 = A, 25 = Z
    #if it's not really a letter, return @
    #if type(number) != type(5) and type(number) != type(np.array([3])[0]): #not an integer or numpy.int32
    #    return ('@')
    if number < 0 or number > 25:
        return ('@')
    else: #it's probably a letter
        try:
            return chr( ord('A')+number )
        except:
            return('@')

def letterToNumber(letter): #A = 0, Z = 25
    #if it's not really a letter, return -999
    #HOW CAN I GENERICALLY TEST FOR LENGTH. EVEN IN CASE OF A NUMBER THAT' SNOT PART OF AN ARRAY?
    try:
        #if len(letter) > 1:
        #    return (-999)
        if letter < 'A' or letter > 'Z':
            return (-999)
        else: #it's a letter
            return ord(letter)-ord('A')
    except:
        return (-999)

#print header for data file
print('experimentPhase\ttrialnum\tsubject\ttask\t',file=dataFile,end='')
print('noisePercent\t',end='',file=dataFile)
print('targetLeftRightIfOne\t',end='',file=dataFile)
printInOrderOfResponses = True
assert (printInOrderOfResponses==True), "Sorry, feature not supported"
if printInOrderOfResponses:
    for i in range(maxNumRespsWanted):
       dataFile.write('resp'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('answer'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('correct'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('whichStream'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('whichRespCue'+str(i)+'\t')   #have to use write to avoid ' ' between successive text, at least until Python 3
       dataFile.write('responsePosRelative'+str(i)+'\t')
print('timingBlips',file=dataFile)
#end of header

            
def calcStreamPos(numStreams,streami,cueOffset,streamOrNoise):
    #streamOrNoise because noise coordinates have to be in deg, stream in pix
    #cueOffset is in deg
    noiseOffsetKludge = 0.9 #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg for noise case

    if numStreams ==0:
        pos = np.array([0,0])
    else:
        #assume want them evenly spaced, counterclockwise starting from directly east
        thisAngle = streami/numStreams * 360
        x = cueOffset*cos(thisAngle/180*pi)
        y = cueOffset*sin(thisAngle/180*pi)
        pos = np.array([x,y])
        
    if streamOrNoise:  #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg
        pos *= noiseOffsetKludge*pixelperdegree
        
    pos = np.round(pos)
    pos = pos.astype(int)
    return pos

def oneFrameOfStim( n,cues,streamLtrSequences,cueDurFrames,letterDurFrames,ISIframes,cuesTemporalPos,whichStreamEachCue,
                                      numStreams,ltrStreams,
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
         cues[thisCue].setLineColor( cueColor )
         cuesTimeToDraw[thisCue] = True

  for cueN in xrange(len(cues)):
    if cuesTimeToDraw[cueN] == True:  ##if don't use this, for AB task, bg color T2 cue will be drawn on top of T1 cue
        cues[cueN].draw()
  
  for streami in xrange(numStreams):
    thisStream = ltrStreams[streami]
    thisLtrIdx = streamLtrSequences[streami][letterN] #which letter of the predecided sequence should be shown
    if showLetter:
      thisStream[thisLtrIdx].setColor( letterColor )
    else: thisStream[thisLtrIdx].setColor( bgColor )
    posThis = calcStreamPos(numStreams,streami,cueOffset,streamOrNoise=0)
    thisStream[thisLtrIdx].pos = posThis
    thisStream[thisLtrIdx].draw()
    
    #noise
    refreshNoise = False #Not recommended because takes longer than a frame, even to shuffle apparently. Or may be setXYs step
    if proportnNoise>0:
        if refreshNoise:
            if frameOfThisLetter ==0: #refresh it if it's the first frame of the noise, so each letter gets a different noise but noise not dynamic within a letter
                np.random.shuffle(noiseCoordsEachStream[streami]) #refresh the noise by shuffling the possible locations of noise dots
                numNoiseDotsThis = numNoiseDotsEachStream[streami]
                dotCoords = noiseCoordsEachStream[streami][0:numNoiseDotsThis] #Take the first numNoiseDots random locations to plot the dots
                posThisDeg =  calcStreamPos(numStreams,streami,cueOffset,streamOrNoise=1)
                print('streami=',streami,'posThisDeg=',posThisDeg,'cueOffset=',cueOffset,'numStream=',numStreams)
                #Displace the noise to present it over the letter stream
                dotCoords[:,0] += posThisDeg[0]
                dotCoords[:,1] += posThisDeg[1]
                noiseEachStream[streami].setXYs(dotCoords)
        noiseEachStream[streami].draw()

  return True 
# #######End of function definition that displays the stimuli!!!! #####################################
#############################################################################################################################

cues = list()
for cueN in xrange(max(numStreamPossibilities)):
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
    cues.append(cue)

#predraw all 26 letters 
ltrHeight = 3 #Martini letters were 2.5deg high
cueOffset = 6
maxStreams = 4
ltrStreams = list()
for streami in xrange(maxStreams):
    streamThis = list()
    for i in range(0,26):
        ltr = visual.TextStim(myWin,pos=(-cueOffset,0),colorSpace='rgb',color=letterColor,alignHoriz='center',alignVert='center',units='deg',autoLog=autoLogging)
        ltr.setHeight( ltrHeight )
        letter = numberToLetter(i)
        ltr.setText(letter,log=False)
        ltr.setColor(bgColor)
        streamThis.append( ltr )
    ltrStreams.append( streamThis )
#print('streamThis=',streamThis,'ltr=',ltr)
#print('ltrStreams=',ltrStreams)
#All noise dot coordinates ultimately in pixels, so can specify each dot is one pixel 
noiseFieldWidthDeg=ltrHeight *0.9  #1.0 makes noise sometimes intrude into circle
noiseFieldWidthPix = int( round( noiseFieldWidthDeg*pixelperdegree ) )

def timingCheckAndLog(ts,trialN):
    #check for timing problems and log them
    #ts is a list of the times of the clock after each frame
    interframeIntervs = np.diff(ts)*1000
    #print '   interframe intervs were ',around(interframeIntervs,1) #DEBUGOFF
    frameTimeTolerance=.3 #proportion longer than refreshRate that will not count as a miss
    longFrameLimit = np.round(1000/refreshRate*(1.0+frameTimeTolerance),2)
    idxsInterframeLong = np.where( interframeIntervs > longFrameLimit ) [0] #frames that exceeded 150% of expected duration
    numCasesInterframeLong = len( idxsInterframeLong )
    if numCasesInterframeLong >0 and (not demo):
       longFramesStr =  'ERROR,'+str(numCasesInterframeLong)+' frames were longer than '+str(longFrameLimit)+' ms'
       if demo: 
         longFramesStr += 'not printing them all because in demo mode'
       else:
           longFramesStr += ' apparently screen refreshes skipped, interframe durs were:'+\
                    str( np.around(  interframeIntervs[idxsInterframeLong] ,1  ) )+ ' and was these frames: '+ str(idxsInterframeLong)
       if longFramesStr != None:
                logging.error( 'trialnum='+str(trialN)+' '+longFramesStr )
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
    
trialClock = core.Clock()
numTrialsCorrect = 0; 
numTrialsApproxCorrect = 0;
numTrialsEachRespCorrect= np.zeros( maxNumRespsWanted )
numTrialsEachApproxCorrect= np.zeros( maxNumRespsWanted )

def shuffleArraysIdentically(a, b):
    assert len(a) == len(b)
    aList = False; bList = False
    if type(a).__name__=='list':
        a = np.array(a)
        aList = True
    if type(b).__name__=='list':
        b = np.array(b)
        bList = True
       
    p = np.random.permutation(len(a))
    a = a[p]
    b = b[p]
    if aList:
        a = list(a)
    if bList:
        b = list(b)
    return a, b
    
def do_RSVP_stim(numStreams, trial, proportnNoise,trialN):
    #relies on global variables:
    #   logging, bgColor
    #
    numStreams = trial['numStreams']
    print("numStreams = ",trial['numStreams'], 'task=',task)
    if task != 'allCued':
        print('targetLeftRightIfOne=',trial['targetLeftRightIfOne'], 'cue0temporalPos=',trial['cue0temporalPos'])
    
    #set up which temporal positions are cued
    cuesTemporalPos = [] #will contain the positions of all the cues (targets)
    if task=='T1T2': #AB
        cuesTemporalPos.append(trial['cue0temporalPos'])
        cuesTemporalPos.append(trial['cue0temporalPos']+thisTrial['cue1lag'])
        numTargets = 2
    elif task =='T1': #target on only one side will be task 'T1' so only one cue
        numTargets = 1
        cuesTemporalPos.append(trial['cue0temporalPos'])
    else: #generic task
        numTargets = trial['numToCue']  #numToCue is global variable
        for streami in xrange(trial['numToCue']):
            cuesTemporalPos.append(trial['cue0temporalPos']) #all cues at the same time
          
    cuesTemporalPos = np.array(cuesTemporalPos) #convert to np array
        
    #assign the letters to be shown in each stream
    streamLtrSequences = list() 
    for streami in xrange(numStreams):
        letterSeqThisStream =  np.arange(0,26)
        np.random.shuffle(letterSeqThisStream)
        streamLtrSequences.append( letterSeqThisStream )
    avoidDuplicates = False
    if avoidDuplicates: #between first two streams
      while (streamLtrSequences[0]==streamLtrSequences[1]).any():
        np.random.shuffle(streamLtrSequences[0])

    #set up corrAnsEachResp, whichStreamEachResp, whichStreamEachCue, whichRespEachCue, and cues' positions 
    #change corrAnswers to answerEachResp everywhere. To recover stream, could use whichStreamEachResp. Sensible if only one stream is queried and just want to print out that on each trial.
    #whichStreamEachCue indicate which stream each cue refers to. These can be in different order than whichStreamEachResp, because randomised which queried first
    #whichStreamEachResp is which stream each response refers to (which stream was queried for 0th response, 1st response, etc)
    corrAnsEachResp = list(); whichStreamEachCue = list(); whichStreamEachResp = list(); whichRespEachCue = list()
    if trial['task'] == 'T1':
        if trial['targetLeftRightIfOne']=='right':
            corrAnsEachResp.append( np.array( streamLtrSequences[0][cuesTemporalPos[0]] )  )
            whichStreamEachResp.append(0) #first drawn is East
            whichStreamEachCue.append(0)
            whichRespEachCue.append(0)
            stream=0
            cues[0].setPos( calcStreamPos(numStreams,stream,cueOffset,streamOrNoise=0) )  
        elif trial['targetLeftRightIfOne']=='left':
            corrAnsEachResp.append( np.array( streamLtrSequences[1][cuesTemporalPos[0]] )  ) #which streams are targets? Need variable for that.
            whichStreamEachResp.append(1)
            whichStreamEachCue.append(0)
            whichRespEachCue.append(0)
            stream=1
            cues[0].setPos( calcStreamPos(numStreams,stream,cueOffset,streamOrNoise=0) )
        else: 
            print("UNEXPECTED targetLeftRightIfOne value!")
    elif trial['task'] =='T1T2': #attentional blink
        if numStreams==1:
            corrAnsEachResp.append( np.array( streamLtrSequences[0][cuesTemporalPos[0]] )   )
            whichStreamEachCue.append(0)
            whichStreamEachResp.append(0)
            whichRespEachCue.append(0)
            corrAnsEachResp.append( np.array( streamLtrSequences[0][cuesTemporalPos[1]] )   )
            whichStreamEachCue.append(0)
            whichStreamEachResp.append(0)
            whichRespEachCue.append(1)
            if len(cuesTemporalPos) > 2:
                print("WARNING: Expected only 2 temporal positions for cues with T1T2 task, but have ", len(cuesTemporalPos))
            cues[0].setPos([0,0])
            cues[1].setPos([0,0])
        elif numStreams==2:  #2 streams with targets in different streams
            print("ERROR: Not set up for 2-stream AB currently, but seems like that's what you are asking for")
            #stream=1
            #cues[0].setPos(  calcStreamPos(numStreams,stream,cueOffset,streamOrNoise=0)  )
            #stream=0
            #cues[1].setPos( calcStreamPos(numStreams,stream,cueOffset,streamOrNoise=0)  )
            whichRespEachCue.append(0)
            whichRespEachCue.append(1)
    else: #assume all len(cuesTemporalPos) streams cued at same time, with numRespsWanted to be reported, in random order.
        #For instance, if numRespsWanted = 1, then a random one is queried.
        if len(set(cuesTemporalPos)) > 1:
            print('ERROR: Expected for this task that all cues would cue the same temporal position. Randomisation of whichResp will be wrong, need to randomise within each temporalPos')
            core.quit()
        #For each cue, work out correct answer, what stream it is
        corrAnsEachCue = []  #because order of responses will be different than order of cues
        numCues = len(cuesTemporalPos) 
        #Randomly assign cues to streams (with no repeats, because assuming all cues identical)
        whichStreamEachCue = range(numStreams)
        random.shuffle(whichStreamEachCue)
        whichStreamEachCue = whichStreamEachCue[:numCues] #Now will be random subset of streams, numCues long
        whichStreamEachResp = deepcopy(whichStreamEachCue)
        for streamI in xrange( numStreams ): #Drawing the cues in the location they're supposed to be in
            #assume each cue the succeeding stream (usually all cues same temporal position)
            #assume only one response per time (Only one stream queried per temporalPos). Cut this down to one below.
            posThis = calcStreamPos(numStreams,streamI,cueOffset,streamOrNoise=0)
            cues[streamI].setPos( posThis )
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
        print('cuesNotQueriedIdxs=',cuesNotQueriedIdxs,'whichRespEachCue=',whichRespEachCue, 'where output= ', np.where( whichRespEachCue > trial['numRespsWanted']-1 ),  'numRespsWanted=', trial['numRespsWanted'] ) #AHdebug
        if len( cuesNotQueriedIdxs ) > 0:
            whichRespEachCue[ cuesNotQueriedIdxs ] = -999 #Leaving those that actually were queried.
        whichStreamEachResp = whichStreamEachResp[ :trial['numRespsWanted'] ] #reduce to actual number of responses
        corrAnsEachResp = corrAnsEachResp[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
        whichRespEachCue = whichRespEachCue[ :trial['numRespsWanted'] ]  #reduce to actual number of responses.
        print('whichStreamEachCue=',whichStreamEachCue,' whichStreamEachResp=',whichStreamEachResp,'whichRespEachCue=',whichRespEachCue,  ' corrAnsEachResp=',corrAnsEachResp)
    
    #debug printouts
    print( 'streamLtrSequences[0]=',[numberToLetter(x) for x in streamLtrSequences[0]] )
    print( 'streamLtrSequences[1]=',[numberToLetter(x) for x in streamLtrSequences[1]] )
    firstCueStream = whichStreamEachCue[0]
    firstCueItem = streamLtrSequences[firstCueStream][cuesTemporalPos[0]]
    print( "corrAnsEachResp=", [numberToLetter(x) for x in corrAnsEachResp],  "First cue cues stream",firstCueStream,   " and letter ",numberToLetter(firstCueItem), end='')
    if trial['numToCue'] > 1:
        secondCueStream = whichStreamEachCue[1]
        secondCueItem = streamLtrSequences[secondCueStream][cuesTemporalPos[1]]
        print(  " while second cue cues stream",secondCueStream, " and letter ",numberToLetter(secondCueItem) )
    else: print('')
    #end debug printouts
    
    noiseEachStream = list(); noiseCoordsEachStream = list(); numNoiseDotsEachStream = list()
    if proportnNoise > 0: #generating noise is time-consuming, so only do it once per trial. Then shuffle noise coordinates for each letter
        for streami in xrange(numStreams):
            (noiseThis,allFieldCoordsThis,numNoiseDotsThis) = createNoise(proportnNoise,myWin,noiseFieldWidthPix, bgColor)  #for the left stream, or the only stream
            noiseEachStream.append(noiseThis)
            numNoiseDotsEachStream.append(numNoiseDotsThis)
            noiseCoordsEachStream.append(allFieldCoordsThis)
        #Work out how to displace the noise so it will be on top of the streams, and then displace it
        for streami in xrange(numStreams):
            numNoiseDotsThis = numNoiseDotsEachStream[streami]
            dotCoords = noiseCoordsEachStream[streami][0:numNoiseDotsThis] #Take the first numNoiseDots random locations to plot the dots
            posThisPix =  calcStreamPos(numStreams,streami,cueOffset,streamOrNoise=1) 
            #Displace the noise to present it over the letter stream
            dotCoords[:,0] += posThisPix[0]
            dotCoords[:,1] += posThisPix[1]
            noiseEachStream[streami].setXYs(dotCoords)
    #end prep of cuesTemporalPos, streamLtrSequences, corrAnsEachResp, noiseEachStream, numNoiseDotsEachStream, noiseCoordsEachStream
    
    preDrawStimToGreasePipeline = list() #I don't know why this works, but without drawing it I have consistent timing blip first time that draw ringInnerR for phantom contours
    for cue in cues:
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
    for i in range(fixatnPeriodFrames+20):  #prestim fixation interval
        if showRefreshMisses: #flicker fixation on and off at framerate to see when skip frame
            if i%4>=2 or demo or exportImages: 
                  fixatn.draw()
            else: fixatnBlank.draw()
        else:
            fixatnPoint.draw()
        myWin.flip()  #end fixation interval
    #myWin.setRecordFrameIntervals(True);  #can't get it to stop detecting superlong frames
    t0 = trialClock.getTime()
    
    for n in range(trialDurFrames): #this is the loop for this trial's stimulus!
        if showRefreshMisses: #flicker fixation on and off at framerate to see when skip frame
            if n%4>=2 or demo or exportImages: 
                  fixatn.draw()
            else: fixatnBlank.draw()
        else:
            fixatnPoint.draw()
        worked = oneFrameOfStim( n,cues,streamLtrSequences,cueDurFrames,letterDurFrames,ISIframes,cuesTemporalPos,whichStreamEachCue,
                                                     numStreams,ltrStreams,
                                                     noiseEachStream,proportnNoise,noiseCoordsEachStream,numNoiseDotsEachStream) #draw letter and possibly cue and noise on top
        if exportImages:
            myWin.getMovieFrame(buffer='back') #for later saving
            framesSaved +=1              
        myWin.flip()
        t=trialClock.getTime()-t0;  ts.append(t);
    #end of big stimulus loop
    myWin.setRecordFrameIntervals(False);

    if task=='T1':
        respPromptStim.setText('Which letter was circled?',log=False)
    elif task=='T1T2':
        respPromptStim.setText('Which two letters were circled?',log=False)
    else: respPromptStim.setText('Error: unexpected task',log=False)
    postCueNumBlobsAway=-999 #doesn't apply to non-tracking and click tracking task
    return streamLtrSequences,cuesTemporalPos,corrAnsEachResp,whichStreamEachCue,whichStreamEachResp,whichRespEachCue,ts  
 
def handleAndScoreResponse(passThisTrial,responses,responsesAutopilot,task,numStreams,streamLtrSequences,
                                                     cuesTemporalPos,whichStreamEachCue,whichStreamEachResp,corrAnsEachResp,whichRespEachCue):
    #Handle response, calculate whether correct, ########################################
    #global printInOrderOfResponses
    if autopilot or passThisTrial:
        responses = responsesAutopilot
        if autopilot: print("autopilot and fake responses are:",responses)

    print( "Inside handleAndScoreResponse corrAnsEachResp=", [numberToLetter(x) for x in corrAnsEachResp], " whichRespEachCue=",whichRespEachCue)
    #for eachRespCorrect etc., tension between calculating %corr for each response position and separating them out by target

    #assume it's single- or multi-target thing where each target in a different stream
    eachRespCorrect = np.zeros( len(responses) ) #number of responses
    eachApproxCorrect = np.zeros( len(responses) )
    posEachResponse = np.zeros( len(responses) )
    responsePosRelative = np.zeros( len(responses) )
    for respI in range(len(responses)): #score each response
        if corrAnsEachResp[respI] == letterToNumber( responses[respI] ):
            eachRespCorrect[respI] = 1 #doesn't compensate for different response query orders
        thisStream = whichStreamEachResp[respI]
        thisRespLetterSeq = streamLtrSequences[thisStream]
        thisResponse = responses[respI]
        posThisResponse= np.where( letterToNumber(thisResponse)==thisRespLetterSeq ) #what position in the stream is the letter the person reported
        posThisResponse= posThisResponse[0] #list with potentially two entries, want first which will be array of places where the response was found in the letter sequence
        if len(posThisResponse) > 1:
            logging.error('Expected response to have occurred in only one position in stream')
        if np.alen(posThisResponse)==0: #response not found in letter sequence
            posThisResponse = -999
            logging.warn('Response was not present in the stimulus stream')
        else: 
            posThisResponse = posThisResponse[0]
        posEachResponse[respI]= posThisResponse
        #determine which cue cued thisStream
        thisCue = np.where( thisStream == np.array(whichStreamEachCue) )[0]
        print("thisStream = ",thisStream, " whichStreamEachCue =", whichStreamEachCue, " thisCue=", thisCue) #AHdebug
        if len(thisCue) > 1:
            logging.error('Expected thisStream to have been cued only once')
        if np.alen(thisCue)==0: #thisStream seemingly not cued
            posThisResponse = -999
            logging.error('thisStream seemingly not cued'); print("ERROR: thisStream seemingly not cued")
        else: 
            thisCue = thisCue[0]
        cueTemporalPos = cuesTemporalPos[ thisCue ]
        responsePosRelative[respI] = posThisResponse - cueTemporalPos
        print(" thisCue idx=", thisCue, " cueTemporalPos = ",cueTemporalPos, " temporal posThisResponse= ", posThisResponse, "responsePosRelative[respI]=",responsePosRelative[respI])
        eachApproxCorrect[respI] += abs(responsePosRelative[respI]) <= 3 #Vul efficacy measure of getting it right to within plus/minus

    print("eachRespCorrect=",eachRespCorrect,"posEachResponse=",posEachResponse,"responsePosRelative=",responsePosRelative)
    if printInOrderOfResponses: #Trying to print each response, whether correct, and which stream it corresponds to. Also cue temporal pos
        #Too hard because mapping between response and eachRespCorrect and cuesTemporalPos a bit complicated, don't have whichRespEachCue variable
        for respI in range(len(responses)): #give info about each response. responses seem to be in the order they are made
            #header should be resp0, eachRespCorrect0, whichStream0,
            print(responses[respI], '\t', end ='', file=dataFile) #respN
            answerCharacter = numberToLetter( corrAnsEachResp[respI] )
            print(answerCharacter, 'answerCharacter\t', end='', file=dataFile) #answer0
            print(eachRespCorrect[respI],'eachRespCorrect\t', end='', file=dataFile) #eachRespCorrect0.  This is in order of responses
            print(whichStreamEachResp[respI], 'whichStream\t', end='', file=dataFile) #whichStream0
            print(whichRespEachCue[respI], 'whichRespEachCue\t', end='', file=dataFile) #whichRespEachCue0
            #Ideally, determine temporal position of cue corresponding to this response, with help of whichStreamEachCue
            #But maybe there is not necessarily any unique mapping between them and the cuesTemporalPos. But, can rely on experiment design nums (cue1lag, etc.)
            #Are cuesTemporalPos always in the order of the responses? (with the proviso that those with same temporal pos will be different streams)
        while respI < maxNumRespsWanted-1:
            respI+=1
            print('-999','\t', end='', file=dataFile) #response N/A for this trialClock
            if respI < len(corrAnsEachResp): #more answers than responses stored, to correspond to other streams to allow analysis of swaps, using whichStreamEachResp
                answerCharacter = numberToLetter( corrAnsEachResp[respI] )
                print(answerCharacter, '\t', end='', file=dataFile) #answerN
            else: print('-999','\t', end='', file=dataFile) #answer0 N/A
            print('-999','\t', end='', file=dataFile) #eachRespCorrectN N/A
            if respI < len(whichStreamEachResp): 
                print(whichStreamEachResp[respI], '\t', end='', file=dataFile) #whichStreamN
            else: print('-999','\t', end='', file=dataFile) #whichStreamN N/A
            print('-999','\t', end='', file=dataFile) #whichRespEachCue N/A
    else: #print in order of cues
        #print('whichStreamEachCue=',whichStreamEachCue,' whichStreamEachResp=',whichStreamEachResp,' corrAnsEachResp=',corrAnsEachResp, ' whichStreamEachCue=',whichStreamEachCue,'whichRespEachCue same as whichStreamEachResp')
        #responses, corrAnsEachResp, whichStreamEachResp will still line up
        uniquethTemporalPosThisCue = 0
        for cueI in range(len(cuesTemporalPos)): #print response stuff to dataFile
            #header was answerPos0, answer0, response0, correct0, responsePosRelative0
            #header should be answerStream0, answer0    
            print(cuesTemporalPos[cueI],'\t', end='', file=dataFile) 
            thisCueLetterSeq = streamLtrSequences[cueI]
            answerCharacter = numberToLetter( corrAnsEachResp[cueI] )
            print(answerCharacter, '\t', end='', file=dataFile) #answer0
            thisStream = whichStreamEachCue[cueI]
            print(thisStream,'\t', end='', file=dataFile) #stream0
            #Find this stream in whichStreamEachResp to reveal which response this was.
            #But, only consider responses from this cuesTemporalPos grouping.
            #Don't have a whichRespEachCue variable
            
            if cueI < numRespsWanted:     #assuming order of responses same as order of cues. Remedy with whichStreamEachCue and whichStreamEachResp
                print(responses[cueI], '\t', end='', file=dataFile) #response0 etc.
            else:
                print('-999','\t', end='', file=dataFile)
                whichStreamEachCue,whichStreamEachRes
                      
    for respI in range(len(responses)):
        print(responsePosRelative[respI], '\t', end='',file=dataFile) #responsePosRelative0
    #print('for cueI=',cueI,' cuesTemporalPos[cueI]=',cuesTemporalPos[cueI], ' answerCharacter=',answerCharacter, ' responses[cueI]=',responses[cueI], 
    #          ' responsePosRelative[cueI]= ',responsePosRelative[cueI], ' eachCorrect[cueI]=',eachCorrect[cueI], 'eachApproxCorrect[cueI]=', eachApproxCorrect[cueI])
    if len(eachRespCorrect)>1:
        allCorrect = eachRespCorrect.all()
    else:
        allCorrect = eachRespCorrect[0]
    T1approxCorrect = eachApproxCorrect[0]
        
    return allCorrect,eachRespCorrect,eachApproxCorrect,T1approxCorrect,passThisTrial,expStop
    #end handleAndScoreResponses

def play_high_tone_correct_low_incorrect(correct, passThisTrial=False):
    highA = sound.Sound('G',octave=5, sampleRate=6000, secs=.3, bits=8)
    low = sound.Sound('F',octave=3, sampleRate=6000, secs=.3, bits=8)
    highA.setVolume(0.9)
    low.setVolume(1.0)
    if correct:
        highA.play()
    elif passThisTrial:
        high= sound.Sound('G',octave=4, sampleRate=2000, secs=.08, bits=8)
        for i in range(2): 
            high.play();  low.play(); 
    else: #incorrect
        low.play()

       
myMouse = event.Mouse()
expStop=False; framesSaved=0
nDone = -1 #change to zero once start main part of experiment
if doStaircase:
    #create the staircase handler
    useQuest = True
    if  useQuest:
        staircase = data.QuestHandler(startVal = 95, 
                              startValSd = 80,
                              stopInterval= 1, #sd of posterior has to be this small or smaller for staircase to stop, unless nTrials reached
                              nTrials = staircaseTrials,
                              #extraInfo = thisInfo,
                              pThreshold = threshCriterion, #0.25,    
                              gamma = 1./26,
                              delta=0.02, #lapse rate, I suppose for Weibull function fit
                              method = 'quantile', #uses the median of the posterior as the final answer
                              stepType = 'log',  #will home in on the 80% threshold. But stepType = 'log' doesn't usually work
                              minVal=1, maxVal = 100
                              )
        print('created QUEST staircase')
    else:
        stepSizesLinear = [.2,.2,.1,.1,.05,.05]
        stepSizesLog = [log(1.4,10),log(1.4,10),log(1.3,10),log(1.3,10),log(1.2,10)]
        staircase = data.StairHandler(startVal = 0.1,
                                  stepType = 'log', #if log, what do I want to multiply it by
                                  stepSizes = stepSizesLog,    #step size to use after each reversal
                                  minVal=0, maxVal=1,
                                  nUp=1, nDown=3,  #will home in on the 80% threshold
                                  nReversals = 2, #The staircase terminates when nTrials have been exceeded, or when both nReversals and nTrials have been exceeded
                                  nTrials=1)
        print('created conventional staircase')
        
    if prefaceStaircaseTrialsN > len(prefaceStaircaseNoise): #repeat array to accommodate desired number of easyStarterTrials
        prefaceStaircaseNoise = np.tile( prefaceStaircaseNoise, ceil( prefaceStaircaseTrialsN/len(prefaceStaircaseNoise) ) )
    prefaceStaircaseNoise = prefaceStaircaseNoise[0:prefaceStaircaseTrialsN]
    
    phasesMsg = ('Doing '+str(prefaceStaircaseTrialsN)+'trials with noisePercent= '+str(prefaceStaircaseNoise)+' then doing a max '+str(staircaseTrials)+'-trial staircase')
    print(phasesMsg); logging.info(phasesMsg)
    
    #staircaseStarterNoise PHASE OF EXPERIMENT
    corrEachTrial = list() #only needed for easyStaircaseStarterNoise
    staircaseTrialN = -1; mainStaircaseGoing = False
    while (not staircase.finished) and expStop==False: #staircase.thisTrialN < staircase.nTrials
        if staircaseTrialN+1 < len(prefaceStaircaseNoise): #still doing easyStaircaseStarterNoise
            staircaseTrialN += 1
            noisePercent = prefaceStaircaseNoise[staircaseTrialN]
        else:
            if staircaseTrialN+1 == len(prefaceStaircaseNoise): #add these non-staircase trials so QUEST knows about them
                mainStaircaseGoing = True
                print('Importing ',corrEachTrial,' and intensities ',prefaceStaircaseNoise)
                staircase.importData(100-prefaceStaircaseNoise, np.array(corrEachTrial))
                printStaircase(staircase, descendingPsycho, briefTrialUpdate=False, printInternalVal=True, alsoLog=False)
            try: #advance the staircase
                printStaircase(staircase, descendingPsycho, briefTrialUpdate=True, printInternalVal=True, alsoLog=False)
                noisePercent = 100. - staircase.next()  #will step through the staircase, based on whether told it (addResponse) got it right or wrong
                staircaseTrialN += 1
            except StopIteration: #Need this here, even though test for finished above. I can't understand why finished test doesn't accomplish this.
                print('stopping because staircase.next() returned a StopIteration, which it does when it is finished')
                break #break out of the trials loop
        #print('staircaseTrialN=',staircaseTrialN)

        streamLtrSequences, cuesTemporalPos,corrAnsEachResp, whichStreamEachCue, whichStreamEachResp, whichRespEachCue, ts  = do_RSVP_stim(
                                                                    thisTrial, noisePercent/100.,staircaseTrialN)
        numCasesInterframeLong = timingCheckAndLog(ts,staircaseTrialN)

        responseDebug=False; responses = list(); responsesAutopilot = list();  #collect responses
        expStop,passThisTrial,responses,responsesAutopilot = \
                stringResponse.collectStringResponse(trial['numRespsWanted'],respPromptStim,respStim,acceptTextStim,myWin,clickSound,badKeySound,
                                                                               requireAcceptance,autopilot,responseDebug=True)

        if not expStop:
            if mainStaircaseGoing:
                print('staircase\t', end='', file=dataFile)
            else: 
                print('staircase_preface\t', end='', file=dataFile)
             #header start      'trialnum\tsubject\ttask\t'
            print(staircaseTrialN,'\t', end='', file=dataFile) #first thing printed on each line of dataFile
            print(subject,'\t',thisTrial['task'],'\t', round(noisePercent,2),'\t', end='', file=dataFile)
            allCorrect,eachRespCorrect,eachApproxCorrect,T1approxCorrect,passThisTrial,expStop = handleAndScoreResponse(
                            passThisTrial,responses,responsesAutopilot,thisTrial['task'],numStreams,streamLtrSequences,
                            cuesTemporalPos,whichStreamEachCue,whichStreamEachResp,corrAnsEachResp,whichRespEachCue )
                                                      
            print(numCasesInterframeLong, file=dataFile) #timingBlips, last thing recorded on each line of dataFile
            core.wait(.06)
            if feedback: 
                play_high_tone_correct_low_incorrect(allCorrect, passThisTrial=False)
            print('staircaseTrialN=', staircaseTrialN,' noisePercent=',round(noisePercent,3),' T1approxCorrect=',T1approxCorrect) #debugON
            corrEachTrial.append(T1approxCorrect)
            if mainStaircaseGoing: 
                staircase.addResponse(T1approxCorrect, intensity = 100-noisePercent) #Add a 1 or 0 to signify a correct/detected or incorrect/missed trial
                #print('Have added an intensity of','{:.3f}'.format(100-noisePercent), 'T1approxCorrect =', T1approxCorrect, ' to staircase') #debugON
    #ENDING STAIRCASE PHASE #################################################################
    ##  ##  ##  #########################################################
    if staircaseTrialN+1 < len(prefaceStaircaseNoise) and (staircaseTrialN>=0): #exp stopped before got through staircase preface trials, so haven't imported yet
        print('Importing ',corrEachTrial,' and intensities ',prefaceStaircaseNoise[0:staircaseTrialN+1])
        staircase.importData(100-prefaceStaircaseNoise[0:staircaseTrialN], np.array(corrEachTrial)) 

    timeAndDateStr = time.strftime("%H:%M on %d %b %Y", time.localtime())
    msg = ('prefaceStaircase phase' if expStop else '')
    msg += ('ABORTED' if expStop else 'Finished') + ' staircase part of experiment at ' + timeAndDateStr
    logging.info(msg); print(msg)
    printStaircase(staircase, descendingPsycho, briefTrialUpdate=True, printInternalVal=True, alsoLog=False)
    #print('staircase.quantile=',round(staircase.quantile(),2),' sd=',round(staircase.sd(),2))
    threshNoise = round(staircase.quantile(),3)
    if descendingPsycho:
        threshNoise = 100- threshNoise
    threshNoise = max( 0, threshNoise ) #e.g. ff get all trials wrong, posterior peaks at a very negative number
    msg= 'Staircase estimate of threshold = ' + str(threshNoise) + ' with sd=' + str(round(staircase.sd(),2))
    logging.info(msg); print(msg)
    myWin.close()
    #Fit and plot data
    fit = None
    try:
        intensityForCurveFitting = staircase.intensities
        if descendingPsycho: 
            intensityForCurveFitting = 100-staircase.intensities #because fitWeibull assumes curve is ascending
        fit = data.FitWeibull(intensityForCurveFitting, staircase.data, expectedMin=1/26., sems = 1.0/len(staircase.intensities))
    except:
        print("Fit failed.")
    plotDataAndPsychometricCurve(staircase,fit,descendingPsycho,threshCriterion)
    #save figure to file
    pylab.savefig(fileName+'.pdf')
    print('The plot has been saved, as '+fileName+'.pdf')
    pylab.show() #must call this to actually show plot
else: #not staircase
    noisePercent = defaultNoiseLevel
    
    ABfirst = False
    nDone =0
    totalTrials = 0
    if ABfirst and trialsAB != None:
        msg='Starting AB part of experiment'
        trials = trialsAB
        totalTrials += trialsAB.nTotal
        logging.info(msg); print(msg)
    else:
        msg = "Starting dual stream part of experiment"
        trials = trialsDualStream
        logging.info(msg); print(msg)
        totalTrials += trialsDualStream.nTotal
    
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
        
        thisTrial = trials.next() #get a proper (non-staircase) trial
        streamLtrSequences,cuesTemporalPos,corrAnsEachResp,whichStreamEachCue,whichStreamEachResp,whichRespEachCue,ts  = do_RSVP_stim(
                                    numStreams,thisTrial,noisePercent/100.,nDone)
        numCasesInterframeLong = timingCheckAndLog(ts,nDone)

        responseDebug=False; responses = list(); responsesAutopilot = list();  #collect responses
        lineupResponse = True
        if lineupResponse:
            if len(whichStreamEachResp) != thisTrial['numRespsWanted']:
                print("len(whichStreamEachResp) does not match numRespsWanted")
            sideFirstLeftRightCentral=2 #default , respond to central
            showBothSides=False
            if thisTrial['numRespsWanted'] == 1:
                if numStreams == 2:
                    showBothSides = False
                    sideFirstLeftRightCentral= not whichStreamEachResp[0]  #have to flip it because 0 means East, right       # thisTrial['targetLeftRightIfOne']
            else: #numRespsWanted >1
                if numStreams ==2:
                    showBothSides = True
                    sideFirstLeftRightCentral =  not whichStreamEachResp[0]  #thisTrial['firstRespLR']
                else: #numStreams must be greater than 2. Probably only want to do lineup for 1. As stopap measure, can put the lineup centrally on every trial
                    showBothSides = False
            #print('sideFirstLeftRightCentral = ',sideFirstLeftRightCentral)
            alphabet = list(string.ascii_uppercase)
            possibleResps = alphabet #possibleResps.remove('C'); possibleResps.remove('V')
            expStop,passThisTrial,responses,responsesAutopilot = \
                letterLineupResponse.doLineup(myWin,myMouse,clickSound,badKeySound,possibleResps,showBothSides,sideFirstLeftRightCentral,autopilot) #CAN'T YET HANDLE MORE THAN 2 LINEUPS
        else:
            expStop,passThisTrial,responses,responsesAutopilot = \
                    stringResponse.collectStringResponse(thisTrial['numRespsWanted'],respPromptStim,respStim,acceptTextStim,myWin,clickSound,badKeySound,
                                                                                    requireAcceptance,autopilot,responseDebug=True)
        print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
        if not expStop:
            print('main\t', end='', file=dataFile) #first thing printed on each line of dataFile
            print(nDone,'\t', end='', file=dataFile)
            print(subject,'\t',thisTrial['task'],'\t', round(noisePercent,3),'\t', thisTrial['targetLeftRightIfOne'],'\t', end='', file=dataFile)
            allCorrect,eachRespCorrect,eachApproxCorrect,T1approxCorrect,passThisTrial,expStop = handleAndScoreResponse(
                    passThisTrial,responses,responsesAutopilot,thisTrial['task'],numStreams,streamLtrSequences,cuesTemporalPos,whichStreamEachCue,
                    whichStreamEachResp,corrAnsEachResp,whichRespEachCue)
            print('Scored response.   allCorrect=', allCorrect) #debugAH
            print(numCasesInterframeLong, file=dataFile) #timingBlips, last thing recorded on each line of dataFile
        
            numTrialsCorrect += allCorrect #so count -1 as 0
            numTrialsApproxCorrect += eachApproxCorrect.all()

            if thisTrial['task']=="T1T2":
                numTrialseachRespCorrect += eachRespCorrect
                numTrialsEachApproxCorrect += eachApproxCorrect
                if numStreams==1:
                        cue2lagIdx = list(possibleCue2lags).index(cue2lag)
                        nTrialsCorrectT2eachLag[cue2lagIdx] += eachRespCorrect[1]
                        nTrialsApproxCorrectT2eachLag[cue2lagIdx] += eachApproxCorrect[1]
                        nTrialsEachLag[cue2lagIdx] += 1
                
            if exportImages:  #catches one frame of response
                 myWin.getMovieFrame() #I cant explain why another getMovieFrame, and core.wait is needed
                 framesSaved +=1; core.wait(.1)
                 myWin.saveMovieFrames('exported/frames.mov')  
                 expStop=True
            core.wait(.1)
            if feedback:
                play_high_tone_correct_low_incorrect(allCorrect, passThisTrial=False)
            nDone+=1
            
            dataFile.flush(); logging.flush()
            print('nDone=', nDone,' trials.nTotal=',trials.nTotal) #' trials.thisN=',trials.thisN
            if (trials.nTotal > 6 and nDone > 2 and nDone %
                 ( trials.nTotal*pctCompletedBreak/100. ) ==1):  #dont modulus 0 because then will do it for last trial
                    nextText.setText('Press "SPACE" to continue!')
                    nextText.draw()
                    progressMsg = 'Completed ' + str(trials.thisN) + ' of ' + str(trials.nTotal) + ' trials'  #EVA if this doesn't work, change it to progressMsg = ' '
                    NextRemindCountText.setText(progressMsg)
                    NextRemindCountText.draw()
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
msg = 'Finishing at '+timeAndDateStr
print(msg); logging.info(msg)
logging.flush()
if expStop:
    msg = 'user aborted experiment on keypress with trials done=' + str(nDone) + ' of ' + str(trials.nTotal+1)
    print(msg); logging.error(msg)

if (nDone >0):
    print('Of ',nDone,' trials, on ',numTrialsCorrect*1.0/nDone*100., '% of all trials all targets reported exactly correct',sep='')
    print('All targets approximately correct in ',round(numTrialsApproxCorrect*1.0/nDone*100,1),'% of trials',sep='')

dataFile.close()
myWin.close() #have to close window if want to show a plot
