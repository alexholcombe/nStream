from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import os
import numpy as np


def openMyStimWindow(mon,screenValues): #make it a function because have to do it several times, want to be sure is identical each time
    myWin = visual.Window(monitor=mon,size=(screenValues['widthPix'],screenValues['heightPix']),allowGUI=screenValues['allowGUI'],units=screenValues['units'],
    color=screenValues['bgColor'],colorSpace='rgb',fullscr=screenValues['fullscr'],screen=screenValues['scrn'],waitBlanking=screenValues['waitBlank'],
                   winType='pyglet' ) #pygame doesn't work, don't know why. Works in textLocationTest.py
    return myWin

def setupDialogue(mon, screenValues, refreshRate = 60, quitFinder = True, demo = False):
    autopilot = False #for now
    
    widthPix = screenValues['widthPix']
    heightPix = screenValues['heightPix']
    
    fullscr = screenValues['fullscr']
    
    infoFirst = { 'Check refresh etc':False, 'Fullscreen (timing errors if not)': fullscr, 'Screen refresh rate': refreshRate }
    OK = gui.DlgFromDict(dictionary=infoFirst, 
        title='AB or dualstream experiment OR staircase to find thresh noise level for T1 performance criterion', 
        order=['Check refresh etc', 'Fullscreen (timing errors if not)'], 
        tip={'Check refresh etc': 'To confirm refresh rate and that can keep up, at least when drawing a grating'},
        #fixed=['Check refresh etc'])#this attribute can't be changed by the user
        )
    if not OK.OK:
        print('User cancelled from dialog box'); core.quit()

    checkRefreshEtc = infoFirst['Check refresh etc']

    fullscr = infoFirst['Fullscreen (timing errors if not)']

    refreshRate = infoFirst['Screen refresh rate']

    if checkRefreshEtc:
        quitFinder = True 

    if quitFinder:
        applescript="\'tell application \"Finder\" to quit\'"
        shellCmd = 'osascript -e '+applescript
        os.system(shellCmd)


    myWin = openMyStimWindow(mon, screenValues)


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
        
        ##########################################################
        ######check screen refresh is what we assume it is #######
        ##########################################################

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

    myDlg = gui.Dlg(title="RSVP experiment", pos=(200,400))

    if not autopilot:
        myDlg.addField('Subject name (default="Hubert"):', 'Hubert', tip='or subject code')
        dlgLabelsOrdered.append('subject')

        
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
    from distutils.version import LooseVersion
    if LooseVersion(psychopy.__version__) < LooseVersion("1.84.2"):
        dimGreyForDlgBox = [-1.,1.,-1.] #color names stopped working along the way, for unknown reason
    
    myDlg.addText('Note: to abort press ESC at a trials response screen', color=dimGreyForDlgBox) 
    myDlg.show()

    if myDlg.OK: #unpack information from dialogue box
       thisInfo = myDlg.data #this will be a list of data returned from each field added in order
       if not autopilot:
           name=thisInfo[dlgLabelsOrdered.index('subject')]
           if len(name) > 0: #if entered something
             subject = name #change subject default name to what user entered
       #trialsPerCondition = int( thisInfo[ dlgLabelsOrdered.index('trialsPerCondition') ] ) #convert string to integer
       #print('trialsPerCondition=',trialsPerCondition)
    else: 
       print('User cancelled from dialog box.')
       logging.flush()
       core.quit()
    if not demo: 
        allowGUI=False
    return fullscr, name
