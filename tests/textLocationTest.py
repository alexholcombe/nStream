#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""
Demo of text rendering in pyglet, including:
- how to specify fonts
- unicode
- rotating text
- mirror-image
"""

from __future__ import division, print_function
from psychopy import visual, core, event, monitors
from math import atan, log, ceil, cos, sin, pi, atan2
import numpy as np

def calcStreamPos(numRings,streamsPerRing,cueOffsets,streami,streamOrNoise):
    #streamOrNoise because noise coordinates have to be in deg, stream in pix
    #cueOffsets are in deg, for instance indicating the eccentricity of the streams/cues
    noiseOffsetKludge = 0.9 #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg for noise case

    thisRingNum =  int( streami / streamsPerRing )
    ringStreami = streami % streamsPerRing
    
    if streamsPerRing ==0:
        pos = np.array([0,0])
    else:
            #assume want them evenly spaced, counterclockwise starting from directly east
            halfAngle = 360/streamsPerRing/2.0
            thisRingAngleOffset = (thisRingNum % 2) * halfAngle #offset odd-numbered rings by half the angle
            thisAngle = ringStreami/streamsPerRing * 360 + thisRingAngleOffset
            #print('thisAngle=',thisAngle)
            x = cueOffsets[thisRingNum]*1.0*cos(thisAngle/180*pi)
            y = cueOffsets[thisRingNum]*1.0*sin(thisAngle/180*pi)
            pos = np.array([x,y])
        
    if streamOrNoise:  #Because the noise coords were drawn in pixels but the cue position is specified in deg, I must convert pix to deg
        pos *= noiseOffsetKludge*pixelperdegree
        
    #pos = np.round(pos)
    #pos = pos.astype(int)
    return pos
    
widthPix= 1024 #monitor width in pixels
heightPix= 768 #800 #monitor height in pixels
viewdist = 57. #cm
monitorwidth = 40.5 #monitor width in cm
monitorname = 'testmonitor'
mon = monitors.Monitor(monitorname,width=monitorwidth, distance=viewdist)#relying on  monitorwidth cm (39 for Mitsubishi to do deg calculations) and gamma info in calibratn
mon.setSizePix( (widthPix,heightPix) )

# Create a window to draw in
win = visual.Window((800.0, 800.0), allowGUI=False, 
            winType='pyglet', #'pyglet',
            monitor='testMonitor', units ='deg', screen=0)
win.recordFrameIntervals = True

# Choose some fonts. If a list is provided, the first font found will be used.
fancy = ['Monotype Corsiva', 'Palace Script MT', 'Edwardian Script ITC']
sans = ['Gill Sans MT', 'Arial', 'Helvetica', 'Verdana']
serif = ['Times', 'Times New Roman']
comic = 'Comic Sans MS'  # the short name won't work

# Initialize some stimuli

ltrHeight = 0.642857142857

numRings = 3
streamsPerRing = 7
cueOffsets = [3,7,11.5]
streami = 0
streamOrNoise = 0


ltrObjs = list()
linesH =list(); linesV= list()
streamPoss = list()
for streami in xrange(streamsPerRing):
    posThis = calcStreamPos(numRings,streamsPerRing,cueOffsets,streami,streamOrNoise)
    aTxt = visual.TextStim(win, color='#FFFFFF',
        text = "K",
        units='deg', height=ltrHeight,
        pos=posThis, 
        alignHoriz='center', alignVert='center',
        font = 'Arial'
        )
    ltrObjs.append(aTxt)
    streamPoss.append(posThis)
    
    #create crosshairs
    lineH =  visual.Line(win, start=(-10, posThis[1]), end=(10, posThis[1]), units='deg')
    lineV =  visual.Line(win, start=(posThis[0], -10), end=(posThis[0],10), units='deg')
    linesH.append(lineH)
    linesV.append(lineV)

circles = list()
for i in xrange(streamsPerRing):
    thisAngle = i* 360./streamsPerRing
    x = cueOffsets[0]*cos(thisAngle/180*pi)
    y = cueOffsets[0]*sin(thisAngle/180*pi)
    circles.append( visual.Circle(win,radius=0.1,fillColor='black', pos=[x,y])  )
    print('streamPos, circle =',streamPoss[i], x,y) #shit , the problem was that streamPos is always integers


cartesianH = visual.Line(win, start=(-20, 0), end=(20, 0), units='deg', lineColor='red')
cartesianV = visual.Line(win, start=(0, -20), end=(0, 20), units='deg',lineColor='red')

numRadials = 3
radials = list()
orient = 0
for i in xrange(numRadials):
    radials.append(  visual.Line(win, start=(-20, 0), end=(20, 0), ori=orient, size=0.8, units='deg',lineColor='green') )
    orient +=360/7 
#work out where center of letter is so can compensate
centerOfLtrV = ltrHeight/2
centerOfLtrH = ltrHeight/2

trialClock = core.Clock()
t = lastFPSupdate = 0

gon = visual.Polygon(win, edges=7, ori=-6, radius=cueOffsets[0])

# Continues the loop until any key is pressed
while not event.getKeys():
    t = trialClock.getTime()
    cartesianH.draw()
    cartesianV.draw()
    #gon.draw()
    for i in xrange(numRadials):
        radials[i].draw()
    for i in xrange(streamsPerRing):
        ltrObjs[i].draw()
        linesH[i].draw()
        linesV[i].draw()
    for circle in circles:
        circle.draw()
    win.flip()

win.close()
core.quit()
