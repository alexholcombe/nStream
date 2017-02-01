from __future__ import print_function
from psychopy import event, sound, logging
from psychopy import visual, event, sound, tools
import numpy as np
import string
from math import floor

def calcRespYandBoundingBox(possibleResps, horizVert, i):
    spacingCtrToCtr = 2.0 / len(possibleResps)
    charHeight = spacingCtrToCtr
    #coordinate will be interpreted as y if horizVert, x otherwise
    startCoordinate = 1-charHeight/2 #top , to bottom
    if horizVert==0:
        startCoordinate*= -1 #left to right
    increment = i*spacingCtrToCtr
    if horizVert==1:
        increment*=- 1 #go down from top
    coordinate = startCoordinate + increment
    boxWidth = spacingCtrToCtr #0.1
    boxHeight = spacingCtrToCtr
    return coordinate, boxWidth, boxHeight

def drawRespOption(myWin,constantCoord,horizVert,color,drawBoundingBox,possibleResps,i):
        #constantCoord is x if horizVert=1 (vertical), y if horizontal
        coord, w, h = calcRespYandBoundingBox( possibleResps, horizVert, i )
        option = visual.TextStim(myWin,colorSpace='rgb',color=color,alignHoriz='center', alignVert='center',
                                                                    height=h,units='norm',autoLog=False)
        option.setText(possibleResps[i])
        x = constantCoord if horizVert else coord
        y = coord if horizVert else constantCoord
        option.pos = (x, y)
        option.draw()
        if drawBoundingBox:
            boundingBox = visual.Rect(myWin,width=w,height=h, pos=(x,y))
            boundingBox.draw()
        
def drawArray(myWin,possibleResps,horizVert,constCoord,lightness,drawBoundingBox):
    '''Draw possibleResps in position x with RGB lightness    
     constCoord is x if horizVert=1 (vertical), y if horizontal
    '''
    #print("lightness in drawArray=",lightness," x=",x)
    #Draw it vertically, from top to bottom
    for i in xrange(len(possibleResps)):
        drawRespOption(myWin,constCoord,horizVert,(lightness,lightness,lightness),drawBoundingBox,possibleResps,i)

def drawResponseArrays(myWin,horizVert,xOffset,possibleResps,bothSides,leftRightCentral):
    '''If bothSides, draw array on both sides, with one side dimmed
    If leftRight=0, collect response from left side, and draw other side dim. Otherwise if =1, from right side.
    possibleResps is usually an array of all the letters to populate the array with.
    xOffset is offset of center of response array relative to center of screen, in norm units
    '''
    #print("leftRight=",leftRight, "xOffset=",xOffset)
    numResps = len(possibleResps)
    dimRGB = -.3
    drawBoundingBox = False #to debug to visualise response regions, make True
    if bothSides:
        if leftRightCentral == 0:
            lightnessLR = (1,dimRGB) #lightness on left and right sides
        elif leftRightCentral ==1:
            lightnessLR = (dimRGB,1) 
        drawArray(myWin,possibleResps,horizVert, xOffset*-1, lightnessLR[0],drawBoundingBox)
        drawArray(myWin,possibleResps,horizVert, xOffset, lightnessLR[1],drawBoundingBox)
    else: #only draw one side
        lightness = 1
        x = xOffset if leftRightCentral==1 else -1*xOffset
        if leftRightCentral ==0:
            x = -1*xOffset
        elif leftRightCentral ==1:
            x = xOffset
            lightnessLR = (dimRGB,1) 
        elif leftRightCentral==2:
            x = 0
        drawArray(myWin,possibleResps,horizVert, x, lightness,drawBoundingBox)

def checkForOKclick(mousePos,respZone):
    OK = False
    if respZone.contains(mousePos):
            OK = True
    return OK

def convertXYtoNormUnits(XY,currUnits,win):
    if currUnits == 'norm':
        return XY
    else:
        widthPix = win.size[0]
        heightPix = win.size[1]
        if currUnits == 'pix':
            xNorm = XY[0]/ (widthPix/2)
            yNorm = XY[1]/ (heightPix/2)
        elif currUnits== 'deg':
            xPix = tools.monitorunittools.deg2pix(XY[0], win.monitor, correctFlat=False)
            yPix = tools.monitorunittools.deg2pix(XY[1], win.monitor, correctFlat=False)
            xNorm = xPix / (widthPix/2)
            yNorm = yPix / (heightPix/2)
            #print("Converted ",XY," from ",currUnits," units first to pixels: ",xPix,yPix," then to norm: ",xNorm,yNorm)
    return xNorm, yNorm

def collectOneLineupResponse(myWin,myMouse,drawBothSides,leftRightCentral,OKtextStim,OKrespZone,possibleResps,xOffset,clickSound,badClickSound):
   if leftRightCentral == 0: #left
        constCoord = -1*xOffset
        horizVert = 1 #vertical
   elif leftRightCentral == 1: #right
        constCoord = xOffset
        horizVert = 1 #vertical
   elif leftRightCentral == 2: #central
        constCoord = 0
        OKrespZone.pos += [0,-.6]
        OKtextStim.pos+= [0,-.6]
        horizVert = 0 #horizontal
   
   myMouse.clickReset()
   sideIndicator = visual.Rect(myWin, width=.14, height=.04, fillColor=(1,1,1), fillColorSpace='rgb', lineColor=None, units='norm', autoLog=False)
   sideIndicatorCoord = .77*constCoord
   sideIndicator.setPos( [sideIndicatorCoord, 0] )
   chosenLtr = visual.TextStim(myWin,colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.4,units='norm',autoLog=False)
   if horizVert: #vertical array
    chosenLtr.setPos( [sideIndicatorCoord,0] )  #big drawing of chosen letter, offset from lineup
   else: #horizontal array
    sideIndicatorCoord = -.3
    chosenLtr.setPos( [0,sideIndicatorCoord] )  #big drawing of chosen letter, offset from lineup
   
   whichResp = -1
   state = 'waitingForFirstClick' 
   #waitingForClick means OK is on the screen, so can either click a lineup item, or click OK
   #'finished' exit this lineup, choice has been made
   expStop = False
   while state != 'finished' and not expStop:
        #draw everything corresponding to this state
        drawResponseArrays(myWin,horizVert,xOffset,possibleResps,drawBothSides,leftRightCentral=leftRightCentral)
        if state == 'waitingForClick':
            #draw selected one in red, and bigly
            drawRespOption(myWin,constCoord,horizVert,(1,-1,-1),False,possibleResps,whichResp)
            chosenLtr.setText(possibleResps[whichResp])
            chosenLtr.draw()
            OKrespZone.draw()
            OKtextStim.draw()
        else:
            if leftRightCentral != 2:
                sideIndicator.draw()
            
        myWin.flip()
        #poll keyboard and mouse

        #Used to use pressed,times = myMouse.getPressed(getTime=True) because it's supposed to return all presses since last call to clickReset. But, doesn't seem to work. So, now block
        #If getTime=True (False by default) then getPressed will return all buttons that have been pressed since the last call to mouse.clickReset as well as their time stamps:
        pressed,times = myMouse.getPressed(getTime=True)
        while not any(pressed): #wait until pressed
            pressed = myMouse.getPressed() 
        mousePos = myMouse.getPos()
        mousePos = convertXYtoNormUnits(mousePos,myWin.units,myWin)
        #Check what was clicked, if anything
        OK = False
        if any(pressed):
            #print('Clicked and state=',state)
            if state == 'waitingForClick':
                OK = checkForOKclick(mousePos,OKrespZone)
                #print('OK=', OK)
                if OK:
                    state = 'finished'
            if not OK: #didn't click OK. Check whether clicked near response array item
                topmostCoord, topmostW, topmostH =  calcRespYandBoundingBox( possibleResps, horizVert, 0) #determine bounds of adjacent option
                topmostX = constCoord if horizVert else topmostCoord
                topmostY = topmostCoord if horizVert else constCoord
                btmmostCoord, btmmostW, btmmostH =  calcRespYandBoundingBox(possibleResps,horizVert, len(possibleResps)-1)
                btmmostX = constCoord if horizVert else btmmostCoord
                btmmostY = btmmostCoord if horizVert else constCoord
                w = topmostW
                h = topmostH
                if horizVert:
                    horizBounds = [ constCoord-w/2, constCoord+w/2 ]
                    vertBounds = [btmmostY - h/2, topmostY + h/2]
                else: #horizontal
                    horizBounds = [topmostX-w/2, btmmostX+w/2,]  #top letter in vertical is first in horizontal
                    vertBounds =  [constCoord-h/2, constCoord+w/2 ]
                print("horizBounds=",horizBounds," vertBounds=",vertBounds, " constCoord=", constCoord)
                xValid = horizBounds[0] <= mousePos[0] <= horizBounds[1]  #clicked in a valid x-position
                yValid = vertBounds[0] <= mousePos[1] <= vertBounds[1]  #clicked in a valid y-position
                if xValid and yValid:
                        clickSound.play()
                        relToBtm = mousePos[1] - vertBounds[0] #mouse coordinates go up from -1 to +1
                        relToLeft = mousePos[0] - horizBounds[0]
                        if horizVert: #vertical
                            whichResp = int (relToBtm / h)
                            #change from relToBtm to relative to top
                            whichResp = len(possibleResps) - 1- whichResp 
                        else: #horizontal
                            whichResp = int(relToLeft / w)
                            print("whichResp from left hopefully = ",whichResp, " corresponding to ", possibleResps[whichResp])
                        #print("whichResp from top = ",whichResp, "xOffsetThis=",xOffsetThis, " About to redraw and draw one item in red")
                        state = 'waitingForClick' 
                else: 
                    badClickSound.play()
            for key in event.getKeys(): #only checking keyboard if mouse was clicked, hoping to improve performance
                key = key.upper()
                if key in ['ESCAPE']:
                    expStop = True
                    #noResponseYet = False
   response = possibleResps[whichResp]
   #print('Returning with response=',response,' expStop=',expStop)
   return response, expStop
        
def doLineup(myWin,myMouse,clickSound,badClickSound,possibleResps,bothSides,leftRightCentral,autopilot):
    #leftRightCentral is 0 if draw on left side first (or only), 1 if draw right side first (or only), 2 if draw centrally only
    if type(leftRightCentral) is str: #convert to 0/1
        if leftRightCentral == 'right':
            leftRightCentral = 1
        elif leftRightCentral == 'left':
            leftRightCentral = 0
        elif leftRightCentral == 'central':
            leftRightCentral = 2
        else:
            print("unrecognized leftRightCentral value")
    expStop = False
    passThisTrial = False
    responsesAutopilot = []
    responses = []
    #First collect one, then dim that one and collect the other
    xOffset = 0.7
    if autopilot: #I haven't bothered to make autopilot display the response screen
        responsesAutopilot.append('Z')
    else:
        OKrespZone = visual.GratingStim(myWin, tex="sin", mask="gauss", texRes=64, units='norm', size=[.5, .5], sf=[0, 0], name='OKrespZone')
        OKtextStim = visual.TextStim(myWin,pos=(0, 0),colorSpace='rgb',color=(-1,-1,-1),alignHoriz='center', alignVert='center',height=.13,units='norm',autoLog=False)
        OKtextStim.setText('OK')
        whichResp0, expStop = \
                collectOneLineupResponse(myWin,myMouse,bothSides,leftRightCentral,OKtextStim,OKrespZone,possibleResps, xOffset, clickSound, badClickSound)
        responses.append(whichResp0)
    if not expStop and bothSides:
        if autopilot:
            responsesAutopilot.append('Z')
        else:
            #Draw arrays again, with that one dim, to collect the other response
            whichResp1, expStop =  \
                collectOneLineupResponse(myWin,myMouse,bothSides,not leftRightCentral,OKtextStim,OKrespZone,possibleResps, xOffset, clickSound, badClickSound)
            responses.append(whichResp1)
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
    from psychopy import monitors
    monitorname = 'testmonitor'
    mon = monitors.Monitor(monitorname,width=40.5, distance=57)
    windowUnits = 'deg' #purely to make sure lineup array still works when windowUnits are something different from norm units
    myWin = visual.Window(monitor=mon,colorSpace='rgb',color=(0,0,0),units=windowUnits)
    #myWin = visual.Window(monitor=mon,size=(widthPix,heightPix),allowGUI=allowGUI,units=units,color=bgColor,colorSpace='rgb',fullscr=fullscr,screen=scrn,waitBlanking=waitBlank) #Holcombe lab monitor

    logging.console.setLevel(logging.WARNING)
    autopilot = False
    clickSound, badClickSound = setupSoundsForResponse()
    alphabet = list(string.ascii_uppercase)
    possibleResps = alphabet
    #possibleResps.remove('C'); possibleResps.remove('V') #per Goodbourn & Holcombe, including backwards-ltrs experiments
    myWin.flip()
    passThisTrial = False
    myMouse = event.Mouse()

    #Do horizontal lineups
    responseDebug=False; responses = list(); responsesAutopilot = list();
    expStop = False
    bothSides = False
    leftRightCentral = 2 #central
    expStop,passThisTrial,responses,responsesAutopilot = \
                doLineup(myWin, myMouse, clickSound, badClickSound, possibleResps, bothSides, leftRightCentral, autopilot)

    print('autopilot=',autopilot, 'responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    
    #Do vertical lineups
    responseDebug=False; responses = list(); responsesAutopilot = list();
    expStop = False
    
    bothSides = True
    leftRightFirst = False
    expStop,passThisTrial,responses,responsesAutopilot = \
                doLineup(myWin, myMouse, clickSound, badClickSound, possibleResps, bothSides, leftRightFirst, autopilot)

    print('autopilot=',autopilot, 'responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    
    
    print('Finished') 