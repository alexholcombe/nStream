% This script takes the individual data files and compiles them in a format
% used by the modeling script. This version is for Experiment 2, where
% there is only a dual-stream RSVP task (no AB task).

clear all; %#ok<CLSCR>

allGroups = {'End6Streams82msSOA','Ex6Streams115msSOA'};
baseDirectory = '~/Google Drive/nStream/';
dataDirectory = [baseDirectory 'wrangledData/'];
saveDirectory = [baseDirectory 'modelOutput/'];



% Specifiy the format of the data in the text file.

dataFormat = {'%s%d%s%s%d%s%s%s%d%d%d%d%s%s%s%s%s%s%d%d%d','%s%d%s%s%d%s%s%s%d%d%d%d%s%s%s%s%s%s%d%d%d'};

% For the RSVP analysis, the variables we need are:
% compiledErrors(thisCondition,thisParticipant,thisSession,thisTrial,thisSide);
% compiledTargets(thisCondition,thisParticipant,thisSession,thisTrial,thisSide);

% Specify the columns containing certain data, in this order:
% 1: Left (or single-stream) Position
% 2: Left (or single-stream) Response Error
% 3: Task
% 4: Response
% 5: Response position
% 6: Subject
% 7: Cued Stream position. 0 is 12 o'clock. Increases clockwise

dataColumns = {[20 12 4 7 21 3 10],[20 12 4 7 21 3 10]};
streams = [6 6];
nStreams = 6;
% Specify the maximum number of trials (per participant, condition, etc).
% We do this so that we can build the
% data matrices to this size. They'll be trimmed afterwards to the actual
% maximum number of trials, but we want to avoid the matrices expanding
% after they're built. That's because we want NaNs in empty cells, not
% zeros. So make sure this is definitely higher than the actual maximum
% number of trials.

nTrialsMaxEstimate = 200;

% Calculate the number of groups.            
nGroups = numel(allGroups);
allParticipantsGlobal = repmat({''},nGroups, 5);

% Cycle through each group
for thisGroup = 1:nGroups
    
    % Open file.
    cd([dataDirectory allGroups{thisGroup}]);
    
    allContents = dir;
    allContents = {allContents.name};
    removeThese = strncmp('.',allContents,1);    % Find invalid entries (beginning with '.')
    allFiles = allContents(~removeThese);
    nTotalFiles = numel(allFiles);

    allParticipants = cell(1,nTotalFiles);

    % Get a list of unique participants.
    for thisFile = 1:nTotalFiles

        thisFileName = allFiles{thisFile};
        participant = strsplit(thisFileName,'.');
        participant = participant{1};
        allParticipants{thisFile} = participant;

    end

    allParticipants = unique(allParticipants);
    nParticipants = numel(allParticipants);
    
    % ---
    % RSVP
    % ---
    
    % Set up data structures. The third, redundant dimension in these
    % structures is there for compatibility with the modeling script, which
    % allows for data to be separated into sessions.

    % compiledErrors(thisParticipant,thisSession,thisTrial);
    % compiledTargets(thisParticipant,thisSession,thisTrial);
    % compiledResponses(thisParticipant,thisSession,thisTrial);
    % compiledResponsePositions(thisParticipant,thisSession,thisTrial);
    
    compiledErrors = NaN(nParticipants,1,nTrialsMaxEstimate);
    compiledTargets = NaN(nParticipants,1,nTrialsMaxEstimate);
    compiledResponsePositions = NaN(nParticipants,1,nTrialsMaxEstimate);
    compiledStreams = NaN(nParticipants,1,nTrialsMaxEstimate);

    nTrialsMaxActual = 0; % Start at zero, then compare each time

    % For each participant...
        
    for thisParticipant = 1:nParticipants
        fprintf('%s\n', allFiles{thisParticipant})
        fileID = fopen(allFiles{thisParticipant});
        allParticipantsGlobal(thisGroup, thisParticipant) = allParticipants(thisParticipant);

        % Read in the data.
        thisRead = textscan(fileID,dataFormat{thisGroup},'Delimiter','\t','HeaderLines',1,'MultipleDelimsAsOne',0);

        theseColumns = dataColumns{thisGroup};

        % Specify the columns containing certain data, in this order:
        % 1: Left (or single-stream) Position
        % 2: Left (or single-stream) Response Error
        % 3: Task
        % 4: Response
        % 5: Response position
        % 6: Subject
        % 7: Cued Stream position. 0 is 12 o'clock. Increases clockwise

        list_T1Position = double(thisRead{theseColumns(1)});
        list_T1Position = list_T1Position +1; % Because Python indexing starts with zero
        list_T1Error = double(thisRead{theseColumns(2)});
        list_T1Response = thisRead{theseColumns(4)};
        list_T1ResponsePosition = thisRead{theseColumns(5)};
        list_T1ResponsePosition = list_T1ResponsePosition +1;
        list_Subject = thisRead{theseColumns(6)};
        list_Stream = thisRead{theseColumns(7)};


        participantID = allParticipants(thisParticipant);
        nTrials = numel(thisRead{theseColumns(1)});
        
        compiledErrors(thisParticipant,1,1:nTrials) = [list_T1Error];
        compiledTargets(thisParticipant,1,1:nTrials) = [list_T1Position];
        compiledResponses(thisParticipant,1,1:nTrials) = [list_T1Response];
        compiledResponsePositions(thisParticipant,1,1:nTrials) = [list_T1ResponsePosition];
        compiledStreams(thisParticipant,1,1:nTrials) = [list_Stream];
        
        % If this is the highest number of trials per participant so far,
        % store that number.
        nTrialsMaxActual = max([nTrialsMaxActual nTrials]);

    end
    
    % Truncate the data matrices
    compiledErrors = compiledErrors(:,:,1:nTrialsMaxActual);
    compiledTargets = compiledTargets(:,:,1:nTrialsMaxActual);
    
    % Save
    cd([saveDirectory 'compiled/']);
    fileName = ['CompiledData_TGRSVP_Exp2_' allGroups{thisGroup}];
    save(fileName,'compiledErrors','compiledTargets', 'allParticipants');
    
end