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
    #print("lightness in drawArray=",lightness," x=",x)
    #Draw it vertically, from top to bottom
    for i in xrange(len(possibleResps)):
        drawRespOption(myWin,x,(lightness,lightness,lightness),drawBoundingBox,possibleResps,i)

def drawResponseArrays(myWin,xOffset,possibleResps,bothSides,leftRight):
    '''If bothSides, draw array on both sides, with one side dimmed
    If leftRight=0, collect response from left side, and draw other side dim. Otherwise if =1, from right side.
    possibleResps is usually an array of all the letters to populate the array with.
    xOffset is offset of center of response array relative to center of screen, in norm units
    '''
    print("leftRight=",leftRight, "xOffset=",xOffset)
    numResps = len(possibleResps)
    dimRGB = 0.3
    drawBoundingBox = False #to debug to visualise response regions, make True 
    if bothSides:
        lightnessLR = (dimRGB,1) if leftRight else (1,dimRGB) #lightness on left and right sides
        drawArray(myWin,possibleResps, xOffset*-1, lightnessLR[0],drawBoundingBox)
        drawArray(myWin,possibleResps, xOffset, lightnessLR[1],drawBoundingBox)
    else: #only draw one side
        x = xOffset if leftRight else -1*xOffset
        drawArray(myWin,possibleResps, x, lightnessLR[leftRight],drawBoundingBox)

def checkForOKclick(mousePos,respZone):
    OK = False
    if respZone.contains(mousePos):
            OK = True
    return OK

def collectOneLineupResponse(myWin,myMouse,leftRight,OKtextStim,possibleResps,xOffset,clickSound,badClickSound):
   xOffsetThis = xOffset if leftRight else -1*xOffset
   myMouse.clickReset()
   OKrespZone = visual.Circle(myWin, radius=.2, edges=30, fillColor=(-.2,-.2,-.2), fillColorSpace='rgb', lineColor=None, units='norm', autoLog=False)
   whichResp = -1
   state = 'waitingForFirstClick' 
   #waitingForClick means OK is on the screen, so can either click a lineup item, or click OK
   #'finished' exit this lineup, choice has been made
   expStop = False
   while state != 'finished' and not expStop:
        #draw everything corresponding to this state
        drawResponseArrays(myWin,xOffset,possibleResps,bothSides=True,leftRight=leftRight)
        if state == 'waitingForClick':
            #draw selected one in red
            drawRespOption(myWin,xOffsetThis,(1,-1,-1),False,possibleResps,whichResp)
            OKrespZone.draw()
            OKtextStim.draw()
        myWin.flip()
        #poll keyboard and mouse

        #Used to use pressed,times = myMouse.getPressed(getTime=True) because it's supposed to return all presses since last call to clickReset. But, doesn't seem to work. So, now block
        #If getTime=True (False by default) then getPressed will return all buttons that have been pressed since the last call to mouse.clickReset as well as their time stamps:
        pressed,times = myMouse.getPressed(getTime=True)
        while not any(pressed): #wait until pressed
            pressed = myMouse.getPressed() 
        mousePos = myMouse.getPos()
        #Check what was clicked, if anything
        OK = False
        if any(pressed):
            print('Clicked and state=',state)
            if state == 'waitingForClick':
                OK = checkForOKclick(mousePos,OKrespZone)
                print('OK=', OK)
                if OK:
                    state = 'finished'
            if not OK: #didn't click OK. Check whether clicked near response array item
                y, w, h = calcRespYandBoundingBox(possibleResps,1)
                topmostY, topmostW, topmostH =  calcRespYandBoundingBox(possibleResps,0)
                btmmostY, btmmostW, btmmostH =  calcRespYandBoundingBox(possibleResps,len(possibleResps)-1)
                horizBounds = [xOffsetThis-w/2, xOffsetThis+w/2]
                vertBounds = [btmmostY - h/2, topmostY + h/2]
                xValid = horizBounds[0] <= mousePos[0] <= horizBounds[1]  #clicked in a valid x-position
                yValid = vertBounds[0] <= mousePos[1] <= vertBounds[1]  #clicked in a valid y-position
                if xValid and yValid:
                        clickSound.play()
                        relToBtm = mousePos[1] - (btmmostY - h/2)
                        whichResp = int (relToBtm / h)
                        print("whichResp from bottom = ",whichResp)
                        whichResp = len(possibleResps) - 1- whichResp
                        print("whichResp from top = ",whichResp, "xOffsetThis=",xOffsetThis, " About to redraw and draw one item in red")
                        state = 'waitingForClick' 
                else: 
                    badClickSound.play()
            for key in event.getKeys(): #only checking keyboard if mouse was clicked, hoping to improve performance
                key = key.upper()
                if key in ['ESCAPE']:
                    expStop = True
                    #noResponseYet = False
   response = possibleResps[whichResp]
   print('Returning with response=',response,' expStop=',expStop)
   return response, expStop
            
def doLineup(myWin,myMouse,OKtextStim,xOffset,clickSound,badClickSound,requireAcceptance,leftRightFirst,autopilot):
    expStop = False
    passThisTrial = False
    responsesAutopilot = ['Z','Z']
    #drawResponseArrays(myWin,xOffset,possibleResps,bothSides=True,leftRight=leftRightFirst)
    #myWin.flip()
    #First collect one, then dim that one and collect the other
    print("xOffset in doLineup=",xOffset)
    whichResp0, expStop = collectOneLineupResponse(myWin,myMouse,leftRightFirst,OKtextStim,possibleResps, xOffset, clickSound, badClickSound)
    if not expStop:
        #Draw arrays again, with that one dim, to collect the other response
        whichResp1, expStop = collectOneLineupResponse(myWin,myMouse,not leftRightFirst,OKtextStim, possibleResps, xOffset, clickSound, badClickSound)
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
    OKtextStim = visual.TextStim(myWin,pos=(0, 0),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.2,units='norm',autoLog=autoLogging)
    OKtextStim.setText('OK')

    alphabet = list(string.ascii_uppercase)
    possibleResps = alphabet
    possibleResps.remove('C'); possibleResps.remove('V') #per Goodbourn & Holcombe, including backwards-ltrs experiments
    
    xOffset = .7
    #drawResponseArrays(myWin,xOffset,possibleResps,True,0)
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
    leftRightFirst = False
    expStop,passThisTrial,responses,responsesAutopilot = \
                doLineup(myWin, myMouse, OKtextStim, xOffset, clickSound, badClickSound, requireAcceptance, leftRightFirst, autopilot)

    print('responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    print('Finished') 