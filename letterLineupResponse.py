from __future__ import print_function
from psychopy import event, sound, logging
import numpy as np
import string

def drawResponseArray(possibleResps,bothSides,leftRight):
    '''If bothSides, draw array on both sides, with one side dimmed
    If leftRight=0, collect response from left side. Otherwise if =1, from right side.
    possibleResps is usually an array of all the letters to populate the array with.
    '''
    numResps = len(possibleResps)
    x = -.6
    #Draw the list vertically, from top to bottom
    spacingCtrToCtr = 2.0 / numResps
    charHeight = spacingCtrToCtr
    yStart = 1-charHeight/2
    for i in xrange(numResps):
        possibleRespStim = visual.TextStim(myWin,colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=charHeight,units='norm',autoLog=autoLogging)
        possibleRespStim.setText(possibleResps[i])
        y = yStart - i*spacingCtrToCtr
        possibleRespStim.pos = (x, y)
        possibleRespStim.draw()

if __name__=='__main__':  #Running this file directly, must want to test functions in this file
    from psychopy import monitors, visual, event, data, logging, core, sound, gui
    myWin = visual.Window()
    autoLogging=False
    autopilot = False
    logging.console.setLevel(logging.WARNING)
    respPromptStim = visual.TextStim(myWin,pos=(0, -.7),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
    acceptTextStim = visual.TextStim(myWin,pos=(0, -.8),colorSpace='rgb',color=(1,1,1),alignHoriz='center', alignVert='center',height=.1,units='norm',autoLog=autoLogging)
    acceptTextStim.setText('Hit ENTER to accept. Backspace to edit')
    respStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)

    alphabet = list(string.ascii_uppercase)
    possibleResps = alphabet
    possibleResps.remove('C'); possibleResps.remove('V') #per Goodbourn & Holcombe, including backwards-ltrs experiments
        
    drawResponseArray(possibleResps,True,0)
    myWin.flip()

    acceptTextStim = visual.TextStim(myWin,pos=(0,0),colorSpace='rgb',color=(1,1,0),alignHoriz='center', alignVert='center',height=.16,units='norm',autoLog=autoLogging)
    acceptTextStim.setText('OK')

    responseDebug=False; responses = list(); responsesAutopilot = list();
    requireAcceptance = True
    expStop = False
    
    requireAcceptance = True
    if requireAcceptance:  #ask participant to HIT ENTER TO ACCEPT
        waitingForAccept = True
        while waitingForAccept and not expStop:
            acceptTextStim.draw()
            drawResponseArray(possibleResps,True,0)
            for key in event.getKeys():
                key = key.upper()
                if key in ['ESCAPE']:
                    expStop = True
                    #noResponseYet = False
                elif key in ['ENTER','RETURN']:
                    waitingForAccept = False
                    accepted = True
#                elif key in ['BACKSPACE','DELETE']:
#                    waitingForAccept = False
#                    numResponses -= 1
#                    responses.pop()
#                    drawResponses(responses,respStim,numCharsWanted,drawBlanks)
#                    myWin.flip() #draw again, otherwise won't draw the last key
            myWin.flip() #end of waitingForAccept loop
      #end of waiting until response is finished, all keys and acceptance if required
    
    passThisTrial = False
    responsesAutopilot = ' '
    #expStop,passThisTrial,responses,responsesAutopilot = \
     #          collectStringResponse(numCharsWanted,respPromptStim,respStim,acceptTextStim,myWin,clickSound,badKeySound,requireAcceptance,autopilot,responseDebug=True)
    print('responses=',responses)
    print('expStop=',expStop,' passThisTrial=',passThisTrial,' responses=',responses, ' responsesAutopilot =', responsesAutopilot)
    print('Finished') 