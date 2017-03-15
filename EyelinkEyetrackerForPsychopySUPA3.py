
# EyeLinkForPsychopyInSUPA.py
#
# Copyright (C) 2011 Wing (Wei-Ying Chen)  modified from pylink ATI and open source code
# Modified by Chris Fajou, pre-April 2015.
# Provides a standard set of functions for using an eye tracker that allows experiment code to be simple and tracker agnostic.( For EyeLink1000)

import pylink
import sys, os, gc
from psychopy import visual, info, misc, monitors, event, core
from numpy import array, hstack
 
RIGHT_EYE = 1
LEFT_EYE = 0
BINOCULAR = 2
HIGH = 1
LOW = 0
WHITE = (255,255,255)
GRAY = GREY = (128,128,128)
BLACK = (0,0,0)
spath = os.path.dirname(sys.argv[0])
if len(spath) !=0: os.chdir(spath)
 
class EyeLinkCoreGraphicsPsychopy(pylink.EyeLinkCustomDisplay):# Draw the Calibrating Display
    def __init__(self, tracker, display, displaySize):
        '''Initialize a Custom EyeLinkCoreGraphics for Psychopy 
        tracker: the TRACKER() object
        display: the Psychopy display window'''
        pylink.EyeLinkCustomDisplay.__init__(self)
        self.display = display
        self.displaySize = displaySize
        self.tracker = tracker
        self.mouse = event.Mouse(visible=False)
        self.target = visual.PatchStim(display, tex = None, mask = 'circle',units='pix', pos=(0,0),size=(6,6), color = [1,1,1] ) # calibrating dot
        print("Finished initializing custom graphics")
 
    def setup_cal_display(self):
        '''This function is called just before entering calibration or validation modes'''
        self.display.flip()
    def exit_cal_display(self):
        '''This function is called just before exiting calibration/validation mode'''
        self.display.flip()
    def record_abort_hide(self):
        '''This function is called if aborted'''
        pass
    def clear_cal_display(self):
        '''Clear the calibration display'''
        self.display.flip()
    def erase_cal_target(self):
        '''Erase the calibration or validation target drawn by previous call to draw_cal_target()'''
        self.display.flip()
    def draw_cal_target(self, x, y):
        '''Draw calibration/validation target'''
        self.target.setPos((x - 0.5*self.displaySize[0], 0.5*self.displaySize[1] - y))
        self.target.draw()
        self.display.flip()
    def play_beep(self, beepid):
        ''' Play a sound during calibration/drift correct.'''
        pass
    def draw_line(self, x1,y1,x2,y2,colorindex):
        '''Draw a line to the display screen. This is used for drawing crosshairs'''
        color = self.getColorFromIndex(colorindex)
        line = visual.ShapeStim(self.display, vertices = ( (x1,y1),(x2,y2) ),lineWidth=1.0, lineColor=color )
        line.draw()
    def draw_losenge(self, x,y,width,height,colorindex):
        '''Draw the cross hair at (x,y) '''
        color = self.getColorFromIndex(colorindex)
    def get_mouse_state(self):
        '''Get the current mouse position and status'''
        pos = self.mouse.getPos()
        state = self.mouse.getPressed()[0]
    def get_input_key(self):
        '''Check the event buffer for special keypresses'''
        k= event.getKeys([])
    def exit_image_display(self):
        '''Called to end camera display'''
        self.display.flip()
    def alert_printf(self,msg):
        '''Print error messages.'''
    def setup_image_display(self, width, height):
        self.display.flip()
    def image_title(self, text):
        '''Draw title text at the top of the screen for camera setup'''
        title = visual.TextStim(self.display, text = text, pos=(-10,0), units=cm)
        title.draw()
        self.display.flip()
        title.draw()
    def draw_image_line(self, width,line, totlines, buff):
        '''Display image given pixel by pixel'''
        pass 
    def set_image_palette(self, r,g,b): 
        '''Given a set of RGB colors, create a list of 24bit numbers representing the pallet.
        I.e., RGB of (1,64,127) would be saved as 82047, or the number 00000001 01000000 011111111'''
        self.imagebuffer = array.array('l')
        self.clear_cal_display()
        sz = len(r)
        i =0
        self.pal = []
        while i < sz:
            rf = int(b[i])
            gf = int(g[i])
            bf = int(r[i])
            self.pal.append((rf<<16) |  (gf<<8) | (bf)) 
            i = i+1        
 
class Tracker_EyeLink(): 
    def __init__(self, win, clock, sj = "TEST", saccadeSensitivity = HIGH, calibrationType = 'HV9',calibrationTargetColor = WHITE,calibrationBgColor = BLACK, CalibrationSounds = False,screen=(1024,768)):
        '''win: psychopy visual window used for the experiment
          clock: psychopy time clock recording time for whole experiment
          sj: Subject identifier string (affects EDF filename)
          saccadeSensitivity:
            HIGH: Pursuit and neurological work
            LOW:  Cognitive research
          calibrationType:
            H3: Horizontal 3-point
            HV3: 3-point calibration, poor linearization
            HV5: 5-point calibration, poor at corners
            HV9: 9-point calibration, best overall
        calibrationTargetColor and calibrationBgColor:
            RGB tuple, i.e., (255,0,0) for Red
            One of: BLACK, WHITE, GRAY
        calibrationSounds:
            True: enable feedback sounds when calibrating'''
        self.edfFileName = str(sj)+".EDF"   # Subject name only can put 8 characters
        print("Connecting to eyetracker.")
        self.tracker = pylink.EyeLink()
        self.timeCorrection = clock.getTime() - self.tracker.trackerTime()
        print("Loading custom graphics")
        #Initializes Experiment Graphics
        genv = EyeLinkCoreGraphicsPsychopy(self.tracker, win, screen)
        pylink.openGraphicsEx(genv)
        # opendatafile
        self.tracker.openDataFile(self.edfFileName)
        
        #EyeLink Tracker Configuration
        pylink.flushGetkeyQueue();# Initializes the key queue used by getkey(). It may be called at any time to get rid any of old keys from the queue.
        self.tracker.setOfflineMode();#Places EyeLink tracker in off-line (idle) mode. Wait till the tracker has finished the mode transition
        self.tracker.sendCommand("screen_pixel_coords =  0 0 %d %d"%( tuple(screen) ))
        self.tracker.setCalibrationType(calibrationType)
        self.tracker.sendCommand("driftcorrect_cr_disable=OFF") #CF - OFF: turns on drift CORRECT; AUTO: Turns on drift CHECK; ON: Turns off both
        #self.tracker.sendCommand("generate_default_targets = NO") 
        #self.tracker.sendCommand("calibration_targets = 512,384 512,417 512,351 402,384 622,384 402,417 622,417 402,351 622,351")
        #self.tracker.sendCommand("validation_targets = 512,384 512,417 512,351 402,384 622,384 402,417 622,417 402,351 622,351")

        self.tracker.sendMessage("DISPLAY_COORDS  0 0 %d %d"%( tuple(screen) ))
        eyelink_ver = self.tracker.getTrackerVersion()
        if eyelink_ver == 3:
            tvstr = self.tracker.getTrackerVersionString()
            vindex = tvstr.find("EYELINK CL")
            tracker_software_ver = int(float(tvstr[(vindex + len("EYELINK CL")):].strip()))
        else: tracker_software_ver = 0
        if eyelink_ver>=2:
            self.tracker.sendCommand("select_parser_configuration %d" %saccadeSensitivity)
        else:
            if saccadeSensitivity == HIGH:svt, sat = 22, 5000  
            else: svt, sat = 30, 9500
            self.tracker.sendCommand("saccade_velocity_threshold = %d" %svt)   
            self.tracker.sendCommand("saccade_acceleration_threshold = %d" %sat)
        if eyelink_ver == 2: #turn off scenelink camera stuff
            self.tracker.sendCommand("scene_camera_gazemap = NO")
 
        # set EDF file contents
        self.tracker.setFileEventFilter("LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON")
        if tracker_software_ver>=4:self.tracker.setFileSampleFilter("LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS,HTARGET")
        else:self.tracker.setFileSampleFilter("LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS")
        
        # set link data (used for gaze cursor)
        self.tracker.setLinkEventFilter("LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON")
        if tracker_software_ver>=4:self.tracker.setLinkSampleFilter("LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,HTARGET")
        else:self.tracker.setLinkSampleFilter("LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS")
        
        #self.tracker.setAcceptTargetFixationButton(1) # This programs a specific button for use in drift correction.
        
          #Set the calibration settings:
        #pylink.setCalibrationColors(WHITE, BLACK) # Sets the calibration target and background color(foreground_color, background_color)
        if CalibrationSounds:
            pylink.setCalibrationSounds("", "", "")
            pylink.setDriftCorrectSounds("", "off", "off")
        else:
            pylink.setCalibrationSounds("off", "off", "off")
            pylink.setDriftCorrectSounds("off", "off", "off")
            
        print("Beginning tracker setup")
        self.tracker.doTrackerSetup()
 
    def sendMessage(self, msg):
        '''Record a message to the tracker'''
        print(msg)
        self.tracker.sendMessage(msg)
 
    def sendCommand(self, msg):
        '''Send command to the tracker'''
        print(msg)
        self.tracker.sendCommand(message)
 
    def resetEventQue(self):
        '''Reset the eyetracker event cue
            usage: use this prior to a loop calling recordFixation() so
            that old fixations or other events are cleared from the 
            buffer.'''
        self.tracker.resetData()
 
    def getStatus(self):
        """Return the status of the connection to the eye tracker"""
        if self.tracker.breakPressed():
            return("ABORT_EXPT")
        if self.tracker.escapePressed():
            return("SKIP_TRIAL")
        if self.tracker.isRecording()==0:
            return("RECORDING")
        if self.tracker.isConnected():
            return("ONLINE")
        else:
            return("OFFLINE")
        return("UNKNOWN STATUS: " + str(self.tracker.getStatus()) )
 
    def startEyeTracking(self,trial, calibTrial,widthPix,heightPix):  
        #Set up each trial with the eye tracker
        if calibTrial: cond = "Test/Calibration Trial"
        else: cond = "Non-test/no calibration trial"
        message ="record_status_message 'Trial %d %s'"%(trial+1, cond)
        self.tracker.sendCommand(message)
        msg = "TRIALID %s"%trial
        self.tracker.sendMessage(msg)
        #The following does drift correction at the begin of each trial
        if calibTrial:# Does drift correction and handles the re-do camera setup situations
            while True:
                try:
                    error = self.tracker.doDriftCorrect(widthPix/2,heightPix/2,1,1) # 0: the fixation target must be drawn by the user
                    if error != 27:
                        #self.tracker.applyDriftCorrect
                        break
                    else:
                        self.tracker.doTrackerSetup()
                except:
                    print("Exception")
                    break
        #self.tracker.sendCommand('start_drift_correction DATA =1 1 1 1') #CF - start drift correct??
        #self.tracker.applyDriftCorrect() #CF - added to actually correct for drift
        self.tracker.setOfflineMode() #CF adds this to stop skipping trials due to not recording
        pylink.msecDelay(50)
        error = self.tracker.startRecording(1,1,1,1)#start to recording (File_samples, File_events, Link_samples, Link_events); if 1, writes something to EDF file. If 0, disables something recording.
        if error: return error;
        pylink.beginRealTimeMode(100)
 
    def stopEyeTracking(self):
        #Ends recording: adds 100 msec of data to catch final events
        pylink.endRealTimeMode()
        pylink.pumpDelay(100)
        self.tracker.stopRecording()
        
    def closeConnectionToEyeTracker(self,eyeMoveFile):
        #Clean everything up, save data and close connection to tracker
        if self.tracker != None:
            # File transfer and cleanup!
            self.tracker.setOfflineMode();
            core.wait(0.5)
            #Close the file and transfer it to Display PC
            self.tracker.closeDataFile()
            self.tracker.receiveDataFile(self.edfFileName,eyeMoveFile) 
            self.tracker.close();
            #Close the experiment graphics
            pylink.closeGraphics()
            return "Eyelink connection closed successfully"
        else:
            return "Eyelink not available, not closed properly"

 
