from psychopy import monitors, visual, event, data, logging, core, sound, gui
import psychopy.info
import numpy as np
from timeit import default_timer as timer



widthPix= 1024 #monitor width in pixels of Agosta
heightPix= 768 #800 #monitor height in pixels
monitorwidth = 40.5 #monitor width in cm
scrn=0 #0 to use main screen, 1 to use external screen connected to computer
fullscr=False #True to use fullscreen, False to not. Timing probably won't be quite right if fullscreen = False
allowGUI = False
units = 'deg'
waitBlank = False
bgColor = [-1,-1,-1]
monitorname = 'testmonitor'
viewdist = 57

mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)

def openMyStimWindow(): #make it a function because have to do it several times, want to be sure is identical each time
    myWin = visual.Window(monitor=mon,size=(widthPix,heightPix),allowGUI=allowGUI,units=units,color=bgColor,colorSpace='rgb',fullscr=fullscr,screen=scrn,waitBlanking=waitBlank,
                   winType='pyglet' ) #pygame doesn't work, don't know why. Works in textLocationTest.py
    return myWin
myWin = openMyStimWindow()

stimOne = visual.Circle(myWin,fillColorSpace='rgb',fillColor=(1,1,1),radius=.5,pos=[-2,0],units='pix',autoLog=False)
stimTwo = visual.Circle(myWin,fillColorSpace='rgb',fillColor=(1,1,1),radius=.5,pos=[2,0],units='pix',autoLog=False)

stimuli = np.array([stimOne, stimTwo])

passed = 0

start = timer()
map(lambda x:x.draw(), stimuli)
end = timer()

print('lambda method timing: ' + str(end - start))

start = timer()
for item in stimuli:
	item.draw()
end = timer()

print('For loop method timing: ' + str(end - start))


