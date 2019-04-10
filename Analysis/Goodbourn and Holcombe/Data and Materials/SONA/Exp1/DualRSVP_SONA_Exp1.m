% Dual RSVP stream demo code
% PTG 11/07/12

Screen('CloseAll');
clear all; 

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));

AssertOpenGL;

% Initialise experiment parameters
saveDirectory = '/Users/experimentalmode/Documents/MATLAB/DualRSVP_SONA/Exp1/Data/';
userDirectory = '/Users/experimentalmode/Documents/MATLAB/DualRSVP_SONA/Exp1/UserData/';
nTrials = 25; %25
nPracticeTrials = 20;
nConditions = 2;
nSessions = 2;

% Randomise
randomSeed = sum(100*clock);
thisStream = RandStream.create('mt19937ar', 'seed', randomSeed);
RandStream.setGlobalStream(thisStream);


while true
    participantID = upper(input('Enter two-letter participant code: ', 's'));
    if length(participantID)==2
        break
    end
end

oldDirectory = cd(userDirectory);
userDataFile = [participantID '_DualRSVP_SONA_Exp1_UserData.mat'];

if ~exist(userDataFile,'file')
   % Make data file
   
   blockOrder = [1 2 1 2; 2 1 2 1]';
   
   if randi(2)==2
       blockOrder = flipud(blockOrder);
   end
   
   % Assign next session
   nextSession = 1;
   totalTime = [];
   
   % Save
   save(userDataFile,'blockOrder','nextSession','totalTime');
   skipInstructions = 0;
   
else
    
    load(userDataFile);
    skipInstructions = 1;
    
end

cd(oldDirectory);

if nextSession > nSessions
    fprintf('\n\nAll sessions completed!');
    return;
else
    theseBlocks = blockOrder(:,nextSession)';
end

% Initialise equipment parameters
runPriority = 1;        % Priority
viewDist = 500;         % Viewing distance in mm
ppm = 3.35;             % Pixels per mm (GDM, measured 18/7/12)

% Gamma correction
%   y   b   k
% R
% G
% B

% gammaVals = [   3.2173	0.1618  2.6817;
%                 2.4776  0.0096  6.4590;
%                 2.9307  0.0549  2.3531];
%             
% gammaVal_a = 2.005;

gammaVals = 1./[3.2173 2.4776 2.9307];

% Initialise text parameters
letterArray = [char(65:66) char(68:85) char(87:90)];      % A to Z
nLetters = length(letterArray)-1;   % Number of possible letters
letterFont = 'Sloan';      % Presentation font - currently MUST be fixed width for drawing accuracy
instructionFont = 'Menlo';
letterTest = 'O';               % Letter for testing letter subtense
letterNull = '-';               % Placeholder letter for response screen
letterSize = 3;                 % Height of letter in degrees of visual angle
letterPoints = 80;              % Startng font size in points (not important, adjusted later)
letterStyle = 1;                % 0 normal, 1 bold
letterSeparation = 8.5;          % Distance between letters in degrees of visual angle
letterEccentricity = 6;         % Eccentricity of letters in degrees of visual angle
letterAlpha = 1.0;              % Opacity of letters

instructionPoints = 16;         % Size of instruction text
textWrap = 50;                  % Number of characters at which to wrap instruction text

% Initialise other spatial parameters
nStreams = 2;                   % Number of letter streams. Currently can't change this parameter.
texturePix = 64;                % Height and width of colour texture, in pixels, is (2*texturePix)+1
textureSD = .6;                 % Standard deviation of Gabor Gaussian in degrees
cueSubtense = 5;              % Subtense of cue in degrees
cueWidth = .1;                  % Width of cue line in degrees
fixationOn = 1;                 % Fixation on (1) or off (0)
fixSubtense = .25;              % Fixation subtense in degrees

% Initialise spatial parameters for response
vSubtenseResponseL = 18;         % Vertical subtense of response array in degrees (letters)
hSubtenseResponse = 24;         % Horizontal subtense of response array
responsePoints = 20;            % Font size in points for response letters
okPoints = 14;                  % Font size for 'OK' on response screen
shrinkFactor = .5;              % Factor by which to shrink rect for response collection

% Initialise temporal parameters
simulTargets = 1;               % Simultaneous targets?
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nRepeats = 1;                   % Number of times to repeat unique items in a stream
itemRate = 12;                  % Item rate
itemBlankRatio = 1.5;           % Ratio of item presentation time to inter-item interval
flipProportion = .5;            % Proportion of inter-flip interval prior to flip to request draw
stimResponseWait = .5;          % Number of seconds to wait from offset of stimulus (including blank interval) and onset of response screen
ITI = 2;                        % Number of seconds to wait from response to onset of warning tone
warnWait = 1;                   % Number of seconds to wait after warning tone to first stimulus onset

% Initialise sound parameters
defaultToneLength = .25;        % Length in seconds of default tone
defaultToneFreq = 440;          % Frequency in Hz of default tone
audioSampleRate = 48000;        % Default audio sample rate

% Initialise colour parameters
blackVal = 0;
greyVal = 0;
whiteVal = .75;
fixVal = .75;
cueVal = .75;
letterVal = .75;
dimVal = 0.25;

% Initialise PsychToolbox Pipeline
screenID = max(Screen('Screens'));
PsychImaging('PrepareConfiguration');
%PsychImaging('AddTask', 'FinalFormatting', 'DisplayColorCorrection', 'SimpleGamma');
PsychImaging('AddTask', 'General', 'EnablePseudoGrayOutput');
PsychImaging('AddTask', 'General', 'NormalizedHighresColorRange');

[ptbWindow, winRect] = PsychImaging('OpenWindow', screenID, greyVal);
%PsychColorCorrection('SetEncodingGamma', ptbWindow, gammaVals);
[screenWidth, screenHeight] = RectSize(winRect);
screenCentreX = round(screenWidth/2);
screenCentreY = round(screenHeight/2);
flipInterval = Screen('GetFlipInterval', ptbWindow);

% Initialise PsychSound
InitializePsychSound;
ptbAudioPort = PsychPortAudio('Open', [], [], 0, audioSampleRate, 1);

% Instruction text
instructionText1 = ['On each trial, a rapid stream of letters ' ...
    'will appear at one or two locations on the screen.\n\n' ...
    'When there are two streams, they will be simultaneous, ' ...
    'but the order of letters will be different in each stream.\n\n' ...
    'Click the mouse button to continue.'];

instructionText2 = ['At some time during the trial, a white circle will appear ' ...
    'around a letter in one or both of the streams.\n\n' ...
    'Your task is to report the letters that were displayed ' ...
    'within the circles (the ''target letters'').\n\n' ...
    'The letters may be any in the English alphabet,\n' ...
    'except for C and V, which will never appear.\n\n' ...
    'Click the mouse button to continue.'];

instructionText3 = ['There will be two different conditions. ' ...
    'The differences between the conditions will be explained during the practice trials.\n\n' ...
    'Click the mouse button to continue.'];

instructionText4 = ['Please keep your eyes fixated on the white dot at ' ...
    'the centre of the screen at all times during the trial. ' ...
    'After each trial, please indicate your response by selecting the ' ...
    'target letters from the arrays on each side of the screen.\n\n' ...
    'If you are unsure, please make your best guess.\n\n' ...
    'Click the mouse button to continue.'];

instructionText5 = ['In this first set of practice trials, ' ...
    'there will be two streams. However, there '...
    'will be only one target letter, which may appear in either stream. ' ...
    'Your task is to report the target letter.\n\n' ...
    'Click the mouse button to begin this set of practice trials.'];
    

endText = ['Thank you.\n\n' ...
    'This part of the experiment is complete.\n\n' ...
    'Please click the mouse button to end the program.'];

skippedText = 'Please click the mouse button to begin the experiment.';
    
% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

% Calculate equipment parameters
mpd = (viewDist/2)*tan(deg2rad(2*letterEccentricity))/letterEccentricity; % Calculate mm per degree of visual angle to the eccentricity of the stimuli
ppd = ppm*mpd;                                              % Calculate pixels per degree of visual angle

% Calculate spatial parameters
cueSubtense_pix = cueSubtense*ppd;                              % Subtense of cue in pixels
cueWidth_pix = cueWidth*ppd;                                    % Width of cue in pixels (pen)
fixSubtense_pix = fixSubtense*ppd;                              % Subtense of fixation marker in pixels
letterHeight_pix = letterSize*ppd;                              % Height of letter in pixels
letterTheta = 2*asin(letterSeparation/(2*letterEccentricity));  % Angle between letters from fixation
letterEccentricity_pix = letterEccentricity*ppd;                % Eccentricity of letters in pixels
letterTheta = [-letterTheta/2 letterTheta/2]+(3*pi/2);
letterRad = [letterEccentricity_pix letterEccentricity_pix];
[letterX, letterY] = pol2cart(letterTheta, letterRad);
textureSD_pix = textureSD*ppd;                                  % Standard deviation of Gaussian in pixels

% Adjust font size to meet subtense requested
Screen('TextFont', ptbWindow, letterFont);
Screen('TextSize', ptbWindow, letterPoints);
Screen('TextStyle', ptbWindow, letterStyle);

defaultBounds = Screen('TextBounds',ptbWindow,upper(letterTest));
defaultHeight = defaultBounds(4)-defaultBounds(2);
scaleFactor = letterHeight_pix/defaultHeight;
letterPoints = round(letterPoints*scaleFactor);

% Verify
Screen('TextSize', ptbWindow, letterPoints);
defaultBounds = Screen('TextBounds',ptbWindow,upper(letterTest));
actualLetterHeight = (defaultBounds(4)-defaultBounds(2))/ppd;

% Calculate sound parameters
defaultTone = sin(linspace(0,2*pi*defaultToneFreq*defaultToneLength,audioSampleRate*defaultToneLength));

% Calculate colour parameters
halfRange = whiteVal-greyVal;

% Calculate variables for determining order of presentation
nItems = nLetters*nRepeats;                            % Number of items in RSVP stream
nPossTargets = nItems-(2*nLeadTrailItems);

% Calculate spatial parameters for response screen
vSubtenseResponseL_pix = vSubtenseResponseL*ppd;
hSubtenseResponse_pix = hSubtenseResponse*ppd;

yResponseArrayLetters_Centre = linspace(-vSubtenseResponseL_pix/2,vSubtenseResponseL_pix/2,nLetters)+screenCentreY;
xResponseArray = linspace(-hSubtenseResponse_pix/2,hSubtenseResponse_pix/2,9)+screenCentreX;
shrinkFactor_pix = texturePix*shrinkFactor;

% Enable alpha blending for typical drawing of masked textures:
%Screen('BlendFunction', ptbWindow, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

% Create Gaussian aperture
[xGrid,yGrid] = meshgrid(-texturePix:texturePix, -texturePix:texturePix);
gaussianStim = whiteVal*(exp(-(xGrid.^2)/(2*textureSD_pix^2)).*exp(-(yGrid.^2)/(2*textureSD_pix^2)));
%gStim = whiteVal*ones([size(gaussianStim) 4]);
%gStim(:,:,4) = gaussianStim;
%colourTex = Screen('MakeTexture', ptbWindow, gStim);
colourTex = Screen('MakeTexture', ptbWindow, gaussianStim, [], [], 1);

% Put the default tone into the audio buffer
PsychPortAudio('FillBuffer', ptbAudioPort, defaultTone);

% Allocate destination rectatngles
letterRect = Screen('TextBounds', ptbWindow, upper(letterTest));
Screen('TextSize', ptbWindow, responsePoints);
responseLetterRect = Screen('TextBounds', ptbWindow, upper(letterTest));
Screen('TextSize', ptbWindow, letterPoints);

letterXL = letterX(1)+screenCentreX;
letterXR = letterX(2)+screenCentreX;
letterY = letterY(1)+screenCentreY;
fixRect = [0 0 fixSubtense_pix fixSubtense_pix];
fixRect = CenterRectOnPoint(fixRect, screenCentreX, screenCentreY);
colourRect = Screen('Rect', colourTex);
cueRect = [0 0 cueSubtense_pix cueSubtense_pix];
cueRectL = CenterRectOnPoint(cueRect, letterXL, letterY);
cueRectR = CenterRectOnPoint(cueRect, letterXR, letterY);

letterXL = letterXL-letterRect(3)/2;
letterXR = letterXR-letterRect(3)/2;
letterY = letterY-letterRect(4)/2;

yResponseArrayLetters = yResponseArrayLetters_Centre-(responseLetterRect(4)/2);

allLetters_L = CenterRectOnPoint([0 0 responseLetterRect(3) vSubtenseResponseL_pix+responseLetterRect(4)], xResponseArray(1), screenCentreY);
allLetters_R = CenterRectOnPoint([0 0 responseLetterRect(3) vSubtenseResponseL_pix+responseLetterRect(4)], xResponseArray(9), screenCentreY);

xResponseArray = xResponseArray-[responseLetterRect(3) 0 0 letterRect(3) 0 letterRect(3) 0 0 responseLetterRect(3)]./2;
goRect = CenterRectOnPoint(colourRect, screenCentreX, screenCentreY);

respShrinkPix = [shrinkFactor_pix shrinkFactor_pix -shrinkFactor_pix -shrinkFactor_pix];
goRectResp = goRect+respShrinkPix;


% Instruction screen
Screen('TextSize', ptbWindow, instructionPoints);
Screen('TextFont', ptbWindow, instructionFont);

if ~skipInstructions

    DrawFormattedText(ptbWindow, instructionText1, 'center', 'center', whiteVal, textWrap);
    Screen('Flip', ptbWindow);

    DrawFormattedText(ptbWindow, instructionText2, 'center', 'center', whiteVal, textWrap);
    WaitSecs(2);
    GetClicks([],0);
    startTotalTime = GetSecs;
    Screen('Flip', ptbWindow);

    DrawFormattedText(ptbWindow, instructionText3, 'center', 'center', whiteVal, textWrap);
    WaitSecs(2);
    GetClicks([],0);
    Screen('Flip', ptbWindow);
    
    DrawFormattedText(ptbWindow, instructionText4, 'center', 'center', whiteVal, textWrap);
    WaitSecs(2);
    GetClicks([],0);
    Screen('Flip', ptbWindow);

    DrawFormattedText(ptbWindow, instructionText5, 'center', 'center', whiteVal, textWrap);
    WaitSecs(2);
    GetClicks([],0);
    
    Screen('Flip', ptbWindow);

    WaitSecs(2);
    GetClicks([],0);
    Screen('Flip', ptbWindow);

    Screen('TextSize', ptbWindow, letterPoints);
    Screen('TextFont', ptbWindow, letterFont);
    
    startBlockTime = GetSecs;
    clickTime = startBlockTime;
    HideCursor;
    
    for thisTrial = 1:nPracticeTrials
        
        % Calculate temporal parameters
        % Calculate condition instead of:
        thisCondition = ceil(thisTrial/(nPracticeTrials/nConditions));
        itemBlankDuration = 1.0/itemRate;                                       % Duration in secs of item + blank
        itemDuration = (itemBlankRatio/(itemBlankRatio+1))*itemBlankDuration;   % Duration in secs of item
        blankDuration = itemBlankDuration-itemDuration;                         % Duration in secs of blank
        
        
        Screen('TextSize', ptbWindow, letterPoints);
        Screen('TextFont', ptbWindow, letterFont);
        letterStream = mod([randperm(nItems); randperm(nItems)],nLetters)+1;
        %colourStream = randi(nColours, [2 nItems]);

        if simulTargets
            thisTarget = repmat(randi(nPossTargets)+nLeadTrailItems,1,2);
        else
            thisTarget = randi(nPossTargets,[1 2])+nLeadTrailItems;
        end
        
        thisStream = NaN;
        preCue = 0;
        
        if thisCondition==1
            % Two streams, one target only
            thisStream = randi(2);
            thisTarget(3-thisStream) = NaN;
        end
        

        % Increase priority
        oldPriority = Priority(runPriority);

        % Sync to VBL at start of animation loop
        Screen('Flip', ptbWindow);

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end
        
        if preCue
            if thisStream==1
                % Draw cue on left
                Screen('FrameOval', ptbWindow, cueVal, cueRectL, cueWidth_pix, cueWidth_pix);
            else
                % Draw cue on right
                Screen('FrameOval', ptbWindow, cueVal, cueRectR, cueWidth_pix, cueWidth_pix);
            end
        end 
        
        % Play the tone
        PsychPortAudio('Start', ptbAudioPort, 1, clickTime + ITI, 0);

        % Display fixation with tone
        warnTime = Screen('Flip', ptbWindow, clickTime + ITI);
        
        % Remove cue
        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        Screen('Flip', ptbWindow, warnTime + defaultToneLength - flipProportion*flipInterval);

        % Draw first frame
        Screen('DrawText', ptbWindow, letterArray(letterStream(1,1)), letterXL, letterY, letterVal, greyVal);
        Screen('DrawText', ptbWindow, letterArray(letterStream(2,1)), letterXR, letterY, letterVal, greyVal);

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        % Mark drawing ops as finished
        Screen('DrawingFinished', ptbWindow);

        % Done. Draw item.
        startTime = Screen('Flip', ptbWindow, warnTime + warnWait);

        % Blank screen with fixation

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        Screen('Flip', ptbWindow, startTime + itemDuration - flipProportion*flipInterval);

        % Start remainder of RSVP loop

        for thisItem = (2:nItems)

            % Draw Text
            Screen('DrawText', ptbWindow, letterArray(letterStream(1,thisItem)), letterXL, letterY, letterVal, greyVal);
            Screen('DrawText', ptbWindow, letterArray(letterStream(2,thisItem)), letterXR, letterY, letterVal, greyVal);

            if fixationOn
                Screen('FillOval', ptbWindow, fixVal, fixRect);
            end

            if thisItem == thisTarget(1)
                % Draw cue on left
                Screen('FrameOval', ptbWindow, cueVal, cueRectL, cueWidth_pix, cueWidth_pix);
            end

            if thisItem == thisTarget(2)
                % Draw cue on right
                Screen('FrameOval', ptbWindow, cueVal, cueRectR, cueWidth_pix, cueWidth_pix);
            end

            % Mark drawing ops as finished
            Screen('DrawingFinished', ptbWindow);

            % Done. Draw item.
            Screen('Flip', ptbWindow, startTime + (thisItem-1)*itemBlankDuration - flipProportion*flipInterval);

            % Blank screen with fixation

            if fixationOn&&(thisItem<nItems)
                Screen('FillOval', ptbWindow, fixVal, fixRect);
            end

            Screen('Flip', ptbWindow, startTime + (thisItem-1)*itemBlankDuration + itemDuration - flipProportion*flipInterval);
        end

        % Restore priority
        Priority(oldPriority);

        WaitSecs(stimResponseWait);

        % Draw response screen
        Screen('TextSize', ptbWindow, responsePoints);
        SetMouse(screenCentreX,screenCentreY, screenID);
        ShowCursor('Arrow');

        
        % Randomly determine response order, or put target response first
        % if only one response required;
        
        if thisCondition==2
            responseOrder = randi(2); % 1=L first, 2=R first
        else
            responseOrder = thisStream;
        end
        
        responseTime = NaN(1,2);
        selectedLetters = zeros(nLetters,2);

        rTimeStart = 0;

        for thisResponse = 1:2
            
            if (thisResponse==2)&&(thisCondition==1)
                % Nothing for practice trials
                
            else
            
                if thisCondition==2
                    if ((thisResponse==1)&&(responseOrder==1))||((thisResponse==2)&&(responseOrder==2))
                        % Left
                        responseSide = 1;
                        colValL = whiteVal;
                        colValR = dimVal;
                        validRects = allLetters_L;
                    else
                        % Right
                        responseSide = 2;
                        colValL = dimVal;
                        colValR = whiteVal;
                        validRects = allLetters_R;
                    end
                else
                    if ((thisResponse==1)&&(responseOrder==1))||((thisResponse==2)&&(responseOrder==2))
                        % Left
                        responseSide = 1;
                        colValL = whiteVal;
                        colValR = greyVal;
                        validRects = allLetters_L;
                    else
                        % Right
                        responseSide = 2;
                        colValL = greyVal;
                        colValR = whiteVal;
                        validRects = allLetters_R;
                    end
                end

                goodResponse = 0;

                while goodResponse==0

                    % Check that mouse is still within ptbWindow
                    [xMPos,yMPos] = GetMouse(screenID);
                    if xMPos < 10
                        SetMouse(screenCentreX, yMPos, screenID);
                        ShowCursor('Arrow');
                    end

                    % Draw attribute matrix
                    for thisLetter = 1:nLetters
                        Screen('DrawText', ptbWindow, letterArray(thisLetter), xResponseArray(1), yResponseArrayLetters(thisLetter), colValL, greyVal);
                        Screen('DrawText', ptbWindow, letterArray(thisLetter), xResponseArray(9), yResponseArrayLetters(thisLetter), colValR, greyVal);
                    end

                    % Draw response letters
                    Screen('TextSize', ptbWindow, letterPoints);

                    if sum(selectedLetters(:,1))
                        Screen('DrawText', ptbWindow, letterArray(find(selectedLetters(:,1))), letterXL, letterY, colValL, greyVal); %#ok<FNDSB>
                    else
                        Screen('DrawText', ptbWindow, letterNull, letterXL, letterY, colValL, greyVal);
                    end

                    if sum(selectedLetters(:,2))
                        Screen('DrawText', ptbWindow, letterArray(find(selectedLetters(:,2))), letterXR, letterY, colValR, greyVal); %#ok<FNDSB>
                    else
                        Screen('DrawText', ptbWindow, letterNull, letterXR, letterY, colValR, greyVal);
                    end

                    Screen('TextSize', ptbWindow, responsePoints);

                    if sum(selectedLetters(:,responseSide)) % If attribute is selected
                        Screen('DrawTexture', ptbWindow, colourTex, [], goRect, [], [], [], [], []);
                        Screen('TextSize', ptbWindow, okPoints);
                        DrawFormattedText(ptbWindow, 'OK', 'center', 'center', blackVal);
                        Screen('TextSize', ptbWindow, responsePoints);
                    else
                        % Possibly draw black GO texture
                        % Screen('DrawTexture', ptbWindow, colourTex, [], goRect, [], [], [], [blackVal blackVal blackVal], []);
                        % DrawFormattedText(ptbWindow, 'OK', 'center', 'center', whiteVal);
                    end

                    if ~rTimeStart
                        rTimeStart = Screen('Flip', ptbWindow);
                    else
                        Screen('Flip', ptbWindow);
                    end

                    % Get a click
                    [nClicks,xClick,yClick,buttonClick] = GetClicks(ptbWindow,0);
                    clickTime = GetSecs;

                    % Check whether the click is in a letter rect
                    if IsInRect(xClick,yClick,validRects)
                        % Check for closest position (Y)
                        [minDist,clickedLetter] = min(abs(yResponseArrayLetters_Centre-yClick));
                        selectedLetters(:,responseSide) = zeros(nLetters,1);
                        selectedLetters(clickedLetter,responseSide) = 1;       
                    end

                    if IsInRect(xClick,yClick,goRectResp)
                        if sum(selectedLetters(:,responseSide)) % If all attributes are selected
                            responseTime(thisResponse) = clickTime-rTimeStart;
                            goodResponse=1;
                            Screen('Flip', ptbWindow);
                        else
                            % Possible error tone
                        end
                    end

                end
            end
        end
            
        if thisTrial < nPracticeTrials
            if ceil((thisTrial+1)/(nPracticeTrials/nConditions))>ceil(thisTrial/(nPracticeTrials/nConditions))
                nextSet = ceil((thisTrial+1)/(nPracticeTrials/nConditions));
                
                if nextSet==2
                    % Condition 2
                    nextTrialInfoText = ['In this second set of practice trials, ' ...
                        'there will again be two streams. However, this time there '...
                        'will be a target letter in each stream; ' ...
                        'both will be presented simultaneously. Your task is to report both ' ...
                        'target letters.\n\nPlease report the target letter from the left stream ' ...
                        'using the array on the left, and the target letter from ' ...
                        'the right stream using the array on the right.\n\n' ...
                        'Only one array will be available at a time, with the order of '...
                        'responding selected randomly on each trial.\n\n' ....
                        'Click the mouse button to start the next set of practice trials.\n\n' ...
                        '(Trial ' num2str(thisTrial+1) ' of ' num2str(nPracticeTrials) ')'];
                end
                                
            else
                nextTrialInfoText = ['Click the mouse button to start the next practice trial.\n\n(Trial ' num2str(thisTrial+1) ' of ' num2str(nPracticeTrials) ')'];
            end
            Screen('TextSize', ptbWindow, instructionPoints);
            Screen('TextFont', ptbWindow, instructionFont);
            DrawFormattedText(ptbWindow, nextTrialInfoText, 'center', 'center', whiteVal, textWrap);
            HideCursor;
            Screen('Flip', ptbWindow);

            % Wait for click to continue
            GetClicks(ptbWindow,0);

        else
            nextTrialInfoText = ['Thank you. The practice trials are complete.'...
                '\n\nIf you have any questions, please ask the experimenter now.'...
                '\n\nPlease click the mouse button to begin the experiment.'];
            Screen('TextSize', ptbWindow, instructionPoints);
            Screen('TextFont', ptbWindow, instructionFont);
            DrawFormattedText(ptbWindow, nextTrialInfoText, 'center', 'center', whiteVal);
        end
        
    end

else

    DrawFormattedText(ptbWindow, skippedText, 'center', 'center', whiteVal, textWrap);
    startTotalTime = GetSecs;
end

Screen('Flip', ptbWindow);

WaitSecs(2);
GetClicks([],0);
Screen('Flip', ptbWindow);

Screen('TextSize', ptbWindow, letterPoints);
    

for thisBlock = 1:numel(theseBlocks)


    % --------------------------
    % BEGIN BLOCK
    conditionNum = theseBlocks(thisBlock);
    
    % Calculate temporal parameters
    % Calculate condition instead of:
    % itemRate = itemRates(conditionNum);                 % Item presentation rate (items per second)
    itemBlankDuration = 1.0/itemRate;                                       % Duration in secs of item + blank
    itemDuration = (itemBlankRatio/(itemBlankRatio+1))*itemBlankDuration;   % Duration in secs of item
    blankDuration = itemBlankDuration-itemDuration;                         % Duration in secs of blank
    
    preCue = 0;
    
    % Order of target streams
    if conditionNum==1
        allStreams = NaN(nTrials,1);
    else
        allStreams = mod(randperm(nTrials),2)+1;
        % This will contain 1 more left than right stream for odd numbers
        % of trials per block. So we'll reverse it on alternate sessions
        if mod(nextSession,2)==1
            allStreams = 3-allStreams;
        end
    end
    
    % Set up data structures
    allLetterOrder = NaN(nTrials,2,nItems);
    allTargets = NaN(nTrials,2);
    allRTs = NaN(nTrials,2);
    allResponses = NaN(nTrials,2);
        
    % INSTRUCTIONS
    if conditionNum==1
        blockInstructionText = ['In this block, ' ...
        'there will be two streams. There is a target letter in each stream; ' ...
        'both will be presented simultaneously. Your task is to report both ' ...
        'target letters.\n\n' ...
        'Click the mouse button to begin the first trial.'];
    elseif conditionNum==2
        blockInstructionText = ['In this block, ' ...
        'there will be two streams. There is only one target letter, '...
        'which may appear in either stream. ' ...
        'Your task is to report the target letter.\n\n' ...
        'Click the mouse button to begin the first trial.'];
    end
    Screen('TextSize', ptbWindow, instructionPoints);
    Screen('TextFont', ptbWindow, instructionFont);
    DrawFormattedText(ptbWindow, blockInstructionText, 'center', 'center', whiteVal, textWrap);
    Screen('TextSize', ptbWindow, letterPoints);
    Screen('TextFont', ptbWindow, letterFont);
    Screen('Flip', ptbWindow);
    WaitSecs(2);
    GetClicks([],0);
    
    % --------------------------
    % BEGIN TRIAL

    startBlockTime = GetSecs;
    clickTime = startBlockTime;
    HideCursor;
    

    for thisTrial = 1:nTrials
        Screen('TextSize', ptbWindow, letterPoints);
        Screen('TextFont', ptbWindow, letterFont);
        letterStream = mod([randperm(nItems); randperm(nItems)],nLetters)+1;
        %colourStream = randi(nColours, [2 nItems]);

        if simulTargets
            thisTarget = repmat(randi(nPossTargets)+nLeadTrailItems,1,2);
        else
            thisTarget = randi(nPossTargets,[1 2])+nLeadTrailItems;
        end

        thisStream = allStreams(thisTrial);

        if conditionNum==2
            % Two streams, one target only
            thisStream = randi(2);
            thisTarget(3-thisStream) = NaN;
        end
        
        % Increase priority
        oldPriority = Priority(runPriority);

        % Sync to VBL at start of animation loop
        Screen('Flip', ptbWindow);

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        if preCue
            if thisStream==1
                % Draw cue on left
                Screen('FrameOval', ptbWindow, cueVal, cueRectL, cueWidth_pix, cueWidth_pix);
            else
                % Draw cue on right
                Screen('FrameOval', ptbWindow, cueVal, cueRectR, cueWidth_pix, cueWidth_pix);
            end
        end 
        
        % Play the tone
        PsychPortAudio('Start', ptbAudioPort, 1, clickTime + ITI, 0);
        
        % Display fixation with tone
        warnTime = Screen('Flip', ptbWindow, clickTime + ITI);
        
        % Remove cue
        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        Screen('Flip', ptbWindow, warnTime + defaultToneLength - flipProportion*flipInterval);

        % Draw first frame
        Screen('DrawText', ptbWindow, letterArray(letterStream(1,1)), letterXL, letterY, letterVal, greyVal);
        Screen('DrawText', ptbWindow, letterArray(letterStream(2,1)), letterXR, letterY, letterVal, greyVal);

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        % Mark drawing ops as finished
        Screen('DrawingFinished', ptbWindow);

        % Done. Draw item.
        startTime = Screen('Flip', ptbWindow, warnTime + warnWait);

        % Blank screen with fixation

        if fixationOn
            Screen('FillOval', ptbWindow, fixVal, fixRect);
        end

        Screen('Flip', ptbWindow, startTime + itemDuration - flipProportion*flipInterval);

        % Start remainder of RSVP loop

        for thisItem = (2:nItems)

            % Draw Text
            Screen('DrawText', ptbWindow, letterArray(letterStream(1,thisItem)), letterXL, letterY, letterVal, greyVal);
            Screen('DrawText', ptbWindow, letterArray(letterStream(2,thisItem)), letterXR, letterY, letterVal, greyVal);

            if fixationOn
                Screen('FillOval', ptbWindow, fixVal, fixRect);
            end

            if thisItem == thisTarget(1)
                % Draw cue on left
                Screen('FrameOval', ptbWindow, cueVal, cueRectL, cueWidth_pix, cueWidth_pix);
            end

            if thisItem == thisTarget(2)
                % Draw cue on right
                Screen('FrameOval', ptbWindow, cueVal, cueRectR, cueWidth_pix, cueWidth_pix);
            end

            % Mark drawing ops as finished
            Screen('DrawingFinished', ptbWindow);

            % Done. Draw item.
            Screen('Flip', ptbWindow, startTime + (thisItem-1)*itemBlankDuration - flipProportion*flipInterval);

            % Blank screen with fixation

            if fixationOn&&(thisItem<nItems)
                Screen('FillOval', ptbWindow, fixVal, fixRect);
            end

            Screen('Flip', ptbWindow, startTime + (thisItem-1)*itemBlankDuration + itemDuration - flipProportion*flipInterval);
        end

        % Restore priority
        Priority(oldPriority);

        WaitSecs(stimResponseWait);

        % Draw response screen
        Screen('TextSize', ptbWindow, responsePoints);
        SetMouse(screenCentreX,screenCentreY, screenID);
        ShowCursor('Arrow');

        % Randomly determine response order, or put target response first
        % if only one response required;
        
        if conditionNum==1
            responseOrder = randi(2); % 1=L first, 2=R first
        else
            responseOrder = thisStream;
        end
        
        responseTime = NaN(1,2);
        selectedLetters = zeros(nLetters,2);

        rTimeStart = 0;

        for thisResponse = 1:2
            
            if ~((thisResponse==2)&&(conditionNum~=1))
                if conditionNum==1
                    if ((thisResponse==1)&&(responseOrder==1))||((thisResponse==2)&&(responseOrder==2))
                        % Left
                        responseSide = 1;
                        colValL = whiteVal;
                        colValR = dimVal;
                        validRects = allLetters_L;
                    else
                        % Right
                        responseSide = 2;
                        colValL = dimVal;
                        colValR = whiteVal;
                        validRects = allLetters_R;
                    end
                else
                    if ((thisResponse==1)&&(responseOrder==1))||((thisResponse==2)&&(responseOrder==2))
                        % Left
                        responseSide = 1;
                        colValL = whiteVal;
                        colValR = greyVal;
                        validRects = allLetters_L;
                    else
                        % Right
                        responseSide = 2;
                        colValL = greyVal;
                        colValR = whiteVal;
                        validRects = allLetters_R;
                    end
                end

                goodResponse = 0;

                while goodResponse==0

                    % Check that mouse is still within ptbWindow
                    [xMPos,yMPos] = GetMouse(screenID);
                    if xMPos < 10
                        SetMouse(screenCentreX, yMPos, screenID);
                        ShowCursor('Arrow');
                    end

                    % Draw attribute matrix
                    for thisLetter = 1:nLetters
                        Screen('DrawText', ptbWindow, letterArray(thisLetter), xResponseArray(1), yResponseArrayLetters(thisLetter), colValL, greyVal);
                        Screen('DrawText', ptbWindow, letterArray(thisLetter), xResponseArray(9), yResponseArrayLetters(thisLetter), colValR, greyVal);
                    end

                    % Draw response letters
                    Screen('TextSize', ptbWindow, letterPoints);

                    if sum(selectedLetters(:,1))
                        Screen('DrawText', ptbWindow, letterArray(find(selectedLetters(:,1))), letterXL, letterY, colValL, greyVal); %#ok<FNDSB>
                    else
                        Screen('DrawText', ptbWindow, letterNull, letterXL, letterY, colValL, greyVal);
                    end

                    if sum(selectedLetters(:,2))
                        Screen('DrawText', ptbWindow, letterArray(find(selectedLetters(:,2))), letterXR, letterY, colValR, greyVal); %#ok<FNDSB>
                    else
                        Screen('DrawText', ptbWindow, letterNull, letterXR, letterY, colValR, greyVal);
                    end

                    Screen('TextSize', ptbWindow, responsePoints);

                    if sum(selectedLetters(:,responseSide)) % If attribute is selected
                        Screen('DrawTexture', ptbWindow, colourTex, [], goRect, [], [], [], [], []);
                        Screen('TextSize', ptbWindow, okPoints);
                        DrawFormattedText(ptbWindow, 'OK', 'center', 'center', blackVal);
                        Screen('TextSize', ptbWindow, responsePoints);
                    else
                        % Possibly draw black GO texture
                        % Screen('DrawTexture', ptbWindow, colourTex, [], goRect, [], [], [], [blackVal blackVal blackVal], []);
                        % DrawFormattedText(ptbWindow, 'OK', 'center', 'center', whiteVal);
                    end

                    if ~rTimeStart
                        rTimeStart = Screen('Flip', ptbWindow);
                    else
                        Screen('Flip', ptbWindow);
                    end

                    % Get a click
                    [nClicks,xClick,yClick,buttonClick] = GetClicks(ptbWindow,0);
                    clickTime = GetSecs;

                    % Check whether the click is in a letter rect
                    if IsInRect(xClick,yClick,validRects)
                        % Check for closest position (Y)
                        [minDist,clickedLetter] = min(abs(yResponseArrayLetters_Centre-yClick));
                        selectedLetters(:,responseSide) = zeros(nLetters,1);
                        selectedLetters(clickedLetter,responseSide) = 1;       
                    end

                    if IsInRect(xClick,yClick,goRectResp)
                        if sum(selectedLetters(:,responseSide)) % If all attributes are selected
                            responseTime(thisResponse) = clickTime-rTimeStart;
                            goodResponse=1;
                            Screen('Flip', ptbWindow);
                        else
                            % Possible error tone
                        end
                    end

                end
            end
        end

        % Store data for this trial
        allLetterOrder(thisTrial,:,:) = letterStream;
        allTargets(thisTrial,:) = thisTarget;
        allRTs(thisTrial,:) = responseTime;
        
        if conditionNum==1
            allResponses(thisTrial,:) = [find(selectedLetters(:,1)) find(selectedLetters(:,2))];
        else
            allResponses(thisTrial,thisStream) = find(selectedLetters(:,thisStream));
        end

        if thisTrial < nTrials
            Screen('TextSize', ptbWindow, instructionPoints);
            Screen('TextFont', ptbWindow, instructionFont);
            nextTrialInfoText = ['Click the mouse button to start the next trial.\n\n(Trial ' num2str(thisTrial+1) ' of ' num2str(nTrials) ' in this block)'];
            DrawFormattedText(ptbWindow, nextTrialInfoText, 'center', 'center', whiteVal);
            HideCursor;
            Screen('Flip', ptbWindow);

            % Wait for click to continue
            GetClicks(ptbWindow,0);
        end

    end

    blockDuration = GetSecs-startBlockTime;
    endTime = now;

    % Save block data
    oldDirectory = cd(saveDirectory);

    % Make new filename
    fileNumber = 1;
    newFileName = [participantID '_' num2str(conditionNum) '_' datestr(now,'YY-mm-DD') '_' num2str(fileNumber) '.mat'];

    while exist([saveDirectory newFileName],'file')
        fileNumber = fileNumber+1;
        newFileName = [participantID '_' num2str(conditionNum) '_' datestr(now,'YY-mm-DD') '_' num2str(fileNumber) '.mat'];
    end

    save(newFileName, 'participantID', 'randomSeed', 'conditionNum', 'itemRate', 'letterSeparation', 'letterEccentricity', 'allLetterOrder', 'allTargets', 'allRTs', 'allResponses', 'endTime', 'blockDuration');
  
    cd(oldDirectory);
    
    if thisBlock < numel(theseBlocks)
                
        remainingBlocks = numel(theseBlocks)-thisBlock;
        
        if remainingBlocks == 1
            nextBlockInfoText = ['Thank you. The block is complete.'...
                    '\n\nPlease take a rest, then click the mouse button'...
                    '\n\nwhen you are ready to continue the experiment.'...
                    '\n\n(1 block remaining)'];
        else
            nextBlockInfoText = ['Thank you. The block is complete.'...
                    '\n\nPlease take a rest, then click the mouse button'...
                    '\n\nwhen you are ready to continue the experiment.'...
                    '\n\n(' num2str(remainingBlocks) ' blocks remaining)'];
        end
        
        Screen('TextSize', ptbWindow, instructionPoints);
        Screen('TextFont', ptbWindow, instructionFont);
        DrawFormattedText(ptbWindow, nextBlockInfoText, 'center', 'center', whiteVal);

        Screen('Flip', ptbWindow);

        WaitSecs(2);
        GetClicks([],0);
        Screen('Flip', ptbWindow);

        Screen('TextSize', ptbWindow, letterPoints);
        Screen('TextFont', ptbWindow, letterFont);
    end
    
end

% Update user data
oldDirectory = cd(userDirectory);
nextSession = nextSession+1;
totalTime = [totalTime GetSecs-startTotalTime];
save(userDataFile,'blockOrder','nextSession','totalTime');
cd(oldDirectory);

% Close onscreen window and audio port, release all ressources
WaitSecs(1);
Screen('TextSize', ptbWindow, instructionPoints);
Screen('TextFont', ptbWindow, instructionFont);
DrawFormattedText(ptbWindow, endText, 'center', 'center', whiteVal, textWrap);
Screen('Flip', ptbWindow);
WaitSecs(2);
GetClicks([],0);
Screen('Flip', ptbWindow);
PsychPortAudio('Close', ptbAudioPort);
Screen('CloseAll');