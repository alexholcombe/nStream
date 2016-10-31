from __future__ import print_function
from psychopy import event, sound, logging
import numpy as np
import string
from math import floor

def calcRespYandBoundingBox(possibleResps, i):
    spacingCtrToCtr = 2.0 / len(possibleResps)
    charHeight = spacingCtrToCtr
    yStart = 1-charHeight/2
    y = yStart - i*spacingCtrToCtr
    boxWidth = 0.1
    boxHeight = spacingCtrToCtr
    return y, boxWidth, boxHeight

def drawRespOption(myWin,x,color,drawBoundingBox,possibleResps,i):
        y, w, h = calcRespYandBoundingBox( possibleResps, i )
        option = visual.TextStim(myWin,colorSpace='rgb',color=color,alignHoriz='center', alignVert='center',
                                                                    height=h,units='norm',autoLog=autoLogging)
        option.setText(possibleResps[i])
        option.pos = (x, y)
        option.draw()
        if drawBoundingBox:
            boundingBox = visual.Rect(myWin,width=w,height=h, pos=(x,y))
            boundingBox.draw()
        
def drawArray(myWin,possibleResps,x,lightness,drawBoundingBox):
    '''Draw possibleResps in position x with RGB lightness    '''
    #Draw it vertically, from top to bottom
    boundingBoxes = list()
    for i in xrange(len(possibleResps)):
        drawRespOption(myWin,x,(lightness,lightness,lightness),drawBoundingBox,possibleResps,i)
    
def drawResponseArrays(myWin,xOffset,possibleResps,bothSides,leftRight):
    '''If bothSides, draw array on both sides, with one side dimmed
    If leftRight=0, collect response from left side, and draw other side dim. Otherwise if =1, from right side.
    possibleResps is usually an array of all the letters to populate the array with.
    xOffset is offset of center of response array relative to center of screen, in norm units
    '''
    numResps = len(possibleResps)
    dimRGB = 0.3
    drawBoundingBox = True
    if bothSides:
        lightnessLR = (dimRGB,1) if leftRight else (1,dimRGB) #lightness on left and right sides
        drawArray(myWin,possibleResps, xOffset*-1, lightnessLR[0],drawBoundingBox)
        drawArray(myWin,possibleResps, xOffset, lightnessLR[1],drawBoundingBox)
    else: #only draw one side
        x = xOffset if leftRight else -1*xOffset
        drawArray(myWin,possibleResps, x, lightnessLR[leftRight],drawBoundingBox)
    
def collectOneLineupResponse(myMouse,leftRight,possibleResps,xOffset,clickSound,badClickSound):
   myMouse.clickReset()
   whichResp = -1
   waitingForClick = True
   expStop = False
   while waitingForClick and not expStop:
        for key in event.getKeys():
            key = key.upper()
            if key in ['ESCAPE']:
                expStop = True
                #noResponseYet = False
        pressed, times = myMouse.getPressed(getTime=True) #If getTime=True (False by default) then getPressed will return all buttons that have been pressed since the last call to mouse.clickReset as well as their time stamps:
        mousePos = myMouse.getPos()
        if any(pressed):
            #check if click is near response array item
            y, w, h = calcRespYandBoundingBox(possibleResps,1)
            topmostY, topmostW, topmostH =  calcRespYandBoundingBox(possibleResps,0)
            btmmostY, btmmostW, btmmostH =  calcRespYandBoundingBox(possibleResps,len(possibleResps)-1)
            horizBounds = [-xOffset-w/2, -xOffset+w/2]
            vertBounds = [btmmostY + h/2, topmostY + h/2]
            xValid = horizBounds[0] <= mousePos[0] <= horizBounds[1]  #clicked in a valid x-position
            yValid = vertBounds[0] <= mousePos[1] <= vertBounds[1]  #clicked in a valid y-position
            if xValid and yValid:
                    clickSound.play()
                    relToBtm = mousePos[1] - btmmostY
                    whichResp = int (relToBtm / h)
                    #draw only that one in red
                    drawResponseArrays(myWin,xOffset,possibleResps,True,0)
                    drawRespOption(myWin,-xOffset,(1,-1,-1),False,possibleResps,whichResp)
                    myWin.flip()
                    waitingForClick = False
            else: 
                badClickSound.play()
                print("played badClickSound")
   return whichResp, expStop
            
def collectLineupResponses(myWin,myMouse,xOffset,clickSound,badClickSound,requireAcceptance,leftRightFirst,autopilot):
    expStop = False
    passThisTrial = False
    responsesAutopilot = ['Z','Z']
    drawResponseArrays(myWin,xOffset,possibleResps,bothSides=True,leftRight=leftRightFirst)
    #First collect one, then dim that one and collect the other
    whichResp0, expStop = collectOneLineupResponse(myMouse,leftRightFirst, possibleResps, xOffset, clickSound, badClickSound)
    if not expStop:
        #Draw arrays again, with that one dim
        drawResponseArrays(myWin, xOffset, possibleResps, bothSides=True, leftRight = not leftRightFirst)
        #collect the other response
        whichResp1, expStop = collectOneLineupResponse(myMouse,not leftRightFirst, possibleResps, xOffset, clickSound, badClickSound)
    else: whichResp1 = -1
    responses = [whichResp0, whichResp1]
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
        badKeySound = sound.Sound('A',octave=5, sampleRate=22050, secs=0.08, bits=8)
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
    
    xOffset = .7
    drawResponseArrays(myWin,xOffset,possibleResps,True,0)
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
    leftRightFirst = True
    expStop,passThisTrial,responses,responsesAutopilot = \
                collectLineupResponses(myWin, myMouse, xOffset, clickSound, badClickSound, requireAcceptance, leftRightFirst, autopilot)

    print('responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    print('Finished') 