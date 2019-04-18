
close all;

% SET ---------------------------------------------------------------------

% Add directories

baseDirectory = '~/gitCode/nStream/';
dataDirectory = [baseDirectory 'Data/'];
saveDirectory = [baseDirectory 'modelOutput/'];

cd(dataDirectory);

% Task parameters

letterArray = [char(65:77) char(79:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nConditions = 1;
nStreams = 2;
nTrials = 25;                   % Per participant, per condition, per session
nSessions = 4;
nSamples = 4;

% Participant details

allParticipants = {'AH','CW','EN','FJ','PG','SM'};

% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @pdf_Mixture_Exp1;
pdf_uniformonly = @pdf_Uniform_Exp1;
nReplicates = 100;
pCrit = .001;
smallNonZeroNumber = 10^-5;
fitMaxIter = 10^4;
fitMaxFunEvals = 10^4;

% Categorisation parameters

nCategories = 2;            % Target or Error
categoryCriterion = .5;    % Required probability that trial is a target or error for it to be classified as such

% Figure axes


% CALCULATE ---------------------------------------------------------------

% Task parameters

nLetters = length(letterArray); % Number of possible letters
maxError = nLetters-(nLeadTrailItems+1);
errorValues = -maxError:1:maxError;
offsetFactor = maxError+1;
nErrorValues = numel(errorValues);

% Participant details

nParticipants = numel(allParticipants);


% COMPILE -----------------------------------------------------------------

% Set up data structures

compiledErrors = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
compiledTargets = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
compiledResponses = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
compiledResponsePositions = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);

allRates = NaN(1,nConditions);

% Find relevant files

fileList = what;
fileList = fileList.mat;

for thisSample = 1:nSamples

    for thisCondition = 1:nConditions
    
        for thisParticipant = 1:nParticipants
            
            thisPrefix = [allParticipants{thisParticipant} '_' num2str(thisSample)];
            loadFiles = find(strncmp(thisPrefix,fileList,numel(thisPrefix)))';
            
            % Load and compile

            for thisSession = 1:length(loadFiles)

                load(fileList{loadFiles(thisSession)});

                for thisTrial = 1:nTrials

                    % Calculate
                    
                    targetPosition = allTargets(thisTrial,:);
                    targetLetters = diag(squeeze(allLetterOrder(thisTrial,:,targetPosition)))';
                    responseLetters = allResponses(thisTrial,:);           
                    responsePosition = [find(squeeze(allLetterOrder(thisTrial,1,:))==responseLetters(1)) ...
                        find(squeeze(allLetterOrder(thisTrial,2,:))==responseLetters(2))];
                    positionError = responsePosition-targetPosition;

                    % Store
                    
                    compiledErrors(thisCondition,thisParticipant,thisSession,thisTrial,:) = positionError;
                    compiledTargets(thisCondition,thisParticipant,thisSession,thisTrial,:) = targetLetters;
                    compiledResponses(thisCondition,thisParticipant,thisSession,thisTrial,:) = responseLetters;
                    compiledResponsePositions(thisCondition,thisParticipant,thisSession,thisTrial,:) = responsePosition;

                end

            end

        end
    end

    allRates(thisCondition) = itemRate;

    squeeze(compiledErrors);
    
    group = [num2str(itemRate) '_per_sec'];
    
    fprintf('%s\n', ['CompiledData_TGRSVP_Exp2_' group])
    
    % Save
    cd([saveDirectory 'compiled/']);
    fileName = ['CompiledData_TGRSVP_Exp2_' group];
    save(fileName,'compiledErrors','compiledTargets', 'allParticipants');
    cd(dataDirectory);

end
