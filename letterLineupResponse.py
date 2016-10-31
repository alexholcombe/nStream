from __future__ import print_function
from psychopy import event, sound, logging
import numpy as np
import string

def calcRespYandBoundingBox(possibleResps, i):
    spacingCtrToCtr = 2.0 / len(possibleResps)
    charHeight = spacingCtrToCtr
    yStart = 1-charHeight/2
    y = yStart - i*spacingCtrToCtr
    boxWidth = 0.1
    boxHeight = spacingCtrToCtr
    return y, boxWidth, boxHeight
    
def drawArray(myWin,possibleResps,x,lightness,drawBoundingBox):
    '''Draw possibleResps in position x with RGB lightness    '''
    #Draw it vertically, from top to bottom

    boundingBoxes = list()
    for i in xrange(len(possibleResps)):
        y, w, h = calcRespYandBoundingBox( possibleResps, i )
        possibleRespStim = visual.TextStim(myWin,colorSpace='rgb',color=(lightness,lightness,lightness),alignHoriz='center', alignVert='center',
                                                                    height=h,units='norm',autoLog=autoLogging)
        possibleRespStim.setText(possibleResps[i])
        possibleRespStim.pos = (x, y)
        if drawBoundingBox:
            boundingBox = visual.Rect(myWin,width=w,height=h, pos=(x,y))
            boundingBox.draw()
        possibleRespStim.draw()
    
def drawResponseArrays(myWin,possibleResps,bothSides,leftRight):
    '''If bothSides, draw array on both sides, with one side dimmed
    If leftRight=0, collect response from left side, and draw other side dim. Otherwise if =1, from right side.
    possibleResps is usually an array of all the letters to populate the array with.
    '''
    numResps = len(possibleResps)
    x = -.6
    dimRGB = 0.3
    drawBoundingBox = True
    if bothSides:
        lightnessLR = (dimRGB,1) if leftRight else (1,dimRGB) #lightness on left and right sides
        drawArray(myWin,possibleResps, x, lightnessLR[0],drawBoundingBox)
        drawArray(myWin,possibleResps, x*-1, lightnessLR[1],drawBoundingBox)
    else: #only draw one side
        x = x if leftRight else -1*x
        drawArray(myWin,possibleResps, x, lightnessLR[leftRight],drawBoundingBox)
    

def collectLineupResponses(myWin,myMouse,clickSound,badClickSound,requireAcceptance,autopilot):
    expStop = False
    waitingForClick = True
    passThisTrial = False
    drawResponseArrays(myWin,possibleResps,True,0)
    responsesAutopilot = ['Z','Z']
    while waitingForClick and not expStop:
        for key in event.getKeys():
            key = key.upper()
            if key in ['ESCAPE']:
                expStop = True
                #noResponseYet = False
            elif key in ['ENTER','RETURN']:
                waitingForAccept = False
                accepted = True
        pressed, times = myMouse.getPressed(getTime=True) #If getTime=True (False by default) then getPressed will return all buttons that have been pressed since the last call to mouse.clickReset as well as their time stamps:
        if any(pressed):
            #check if click is near response
            y, boxWidth, boxHeight = calcRespYandBoundingBox(possibleResps)
            print("pressed =", pressed," times=", times)
            waitingForClick = False
            
    return expStop,passThisTrial,responses,responsesAutopilot

def setupSoundsForResponse():
    fileName = '406__tictacshutup__click-1-d.wav'
    try:
        clickSound=sound.Sound(fileName)
    except:
        print('Could not load the desired click sound file, instead using manually created inferior click')
        try:
            clickSound=sound.Sound('D',octave=3, sampleRate=22050, secs=0.015, bits=8)
        except:
            clickSound = None
            print('Could not create a click sound for typing feedback')
    try:
        badKeySound = sound.Sound('A',octave=5, sampleRate=22050, secs=0.03, bits=8)
    except:
        badKeySound = None
        print('Could not create an invalid key sound for typing feedback')
        
    return clickSound, badKeySound

if __name__=='__main__':  #Running this file directly, must want to test functions in this file
    from psychopy import monitors, visual, event, data, logging, core, sound, gui
    myWin = visual.Window(colorSpace='rgb',color=(0,0,0))
    logging.console.setLevel(logging.WARNING)
    autoLogging=False
    autopilot = False
    clickSound, badClickSound = setupSoundsForResponse()

    respPromptStim = visual.TextStim(myWin,pos=(0, -.7),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
    acceptTextStim = visual.TextStim(myWin,pos=(0, -.8),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
    acceptTextStim.setText('Hit ENTER to accept. Backspace to edit')
    respStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)

    alphabet = list(string.ascii_uppercase)
    possibleResps = alphabet
    possibleResps.remove('C'); possibleResps.remove('V') #per Goodbourn & Holcombe, including backwards-ltrs experiments
        
    drawResponseArrays(myWin,possibleResps,True,0)
    myWin.flip()

    acceptTextStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)
    acceptTextStim.setText('OK')
    acceptTextStim.draw()
    
    responseDebug=False; responses = list(); responsesAutopilot = list();
    requireAcceptance = True
    expStop = False
    
    passThisTrial = False
    responsesAutopilot = ' '
    myMouse = event.Mouse() 

    expStop,passThisTrial,responses,responsesAutopilot = \
                collectLineupResponses(myWin,myMouse,clickSound,badClickSound,requireAcceptance,autopilot)

    print('responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    print('Finished') 