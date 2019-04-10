clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP_SONA/Exp1/Data/');

%addpath(genpath('/Users/ptg/Dropbox/MATLAB/'));
%cd('/Users/ptg/Dropbox/MATLAB/DualRSVP_SONA/Exp1/Data/');

% Task parameters

letterArray = [char(65:66) char(68:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nConditions = 2;
nStreams = 2;
nTrials = 25;                   % Per participant, per condition, per session
nSessions = 4;                  % !!!!!!!!!!!!!!

% Participant details

allParticipants = {'JA','JB','JC','JD','JE','JF','JG','JH','JI','JJ','JK','JL','JM','JN','JO','JP','JQ','JR','JS','JT'};
allSex = [0 0 1 1 0 0 0 0 1 1 0 0 0 0 1 0 0 1 1 0];
allAge = [18 25 18 23 24 17 19 20 18 18 19 18 18 19 19 19 18 18 21 22];
EHI = [-200 400 300 400 400 400 400 325 400 -125 400 -75 375 400 325 400 350 300 400 175];

% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @pdf_Mixture_Exp1;
pdf_uniformonly = @pdf_Uniform_Exp1;
nReplicates = 100;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^4;
fitMaxFunEvals = 10^4;


% CALCULATE ---------------------------------------------------------------

% Task parameters
% totalTrials = nTrials*nConditions;

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
  
for thisParticipant = 1:nParticipants
    
    for thisCondition = 1:nConditions

        thisPrefix = [allParticipants{thisParticipant} '_' num2str(thisCondition)];
        loadFiles = find(strncmp(thisPrefix,fileList,numel(thisPrefix)))';

        % Load and compile

        for thisSession = 1:length(loadFiles)

            load(fileList{loadFiles(thisSession)});

            for thisTrial = 1:nTrials

                % Calculate
                targetPosition = allTargets(thisTrial,:);
                responseLetters = allResponses(thisTrial,:);

                targetLetters = NaN(1,2);
                responsePosition = NaN(1,2);

                for thisStream = 1:nStreams

                    if ~isnan(targetPosition(thisStream))

                        targetLetters(thisStream) = allLetterOrder(thisTrial,thisStream,targetPosition(thisStream));

                    end

                    if ~isnan(responseLetters(thisStream))

                        responsePosition(thisStream) = find(squeeze(allLetterOrder(thisTrial,thisStream,:))==responseLetters(thisStream));

                    end

                end

                positionError = responsePosition-targetPosition;

                % Store

                compiledErrors(thisCondition,thisParticipant,thisSession,thisTrial,:) = positionError;
                compiledTargets(thisCondition,thisParticipant,thisSession,thisTrial,:) = targetLetters;
                compiledResponses(thisCondition,thisParticipant,thisSession,thisTrial,:) = responseLetters;
                compiledResponsePositions(thisCondition,thisParticipant,thisSession,thisTrial,:) = responsePosition;

            end

        end

        allRates(thisCondition) = itemRate;
        
    end

end


% MODEL -------------------------------------------------------------------

modelConditions = 4;

% Separated by participant

% Build data structures
allEstimates_byParticipant = NaN(modelConditions,nStreams,nParticipants,nFreeParameters);
allLowerBounds_byParticipant = NaN(modelConditions,nStreams,nParticipants,nFreeParameters);
allUpperBounds_byParticipant = NaN(modelConditions,nStreams,nParticipants,nFreeParameters);

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

for thisCondition = 1:modelConditions
    
    for thisStream = 1:nStreams
        
        for thisParticipant = 1:nParticipants
       
            minNegLogLikelihood = inf;

            if thisCondition==1
                % Dual-stream, all trials
                theseErrors = squeeze(compiledErrors(1,thisParticipant,:,:,thisStream));
                theseErrors = theseErrors(:);
                theseErrors = theseErrors(~isnan(theseErrors));
                
            elseif thisCondition==2
                % Dual-stream, odd trials
                theseErrors = squeeze(compiledErrors(1,thisParticipant,:,:,thisStream));
                theseErrors = theseErrors(:);
                theseErrors = theseErrors(~isnan(theseErrors));
                theseErrors = theseErrors(1:2:numel(theseErrors));
                
            elseif thisCondition==3
                % Dual-stream, even trials
                theseErrors = squeeze(compiledErrors(1,thisParticipant,:,:,thisStream));
                theseErrors = theseErrors(:);
                theseErrors = theseErrors(~isnan(theseErrors));
                theseErrors = theseErrors(2:2:numel(theseErrors));
                
            else
                % Single-stream, all trials
                theseErrors = squeeze(compiledErrors(2,thisParticipant,:,:,thisStream));
                theseErrors = theseErrors(:);
                theseErrors = theseErrors(~isnan(theseErrors));
                
            end
            
             % Compute negative log likelihood for uniform distribution
        
            uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));

            warning('off', 'stats:mlecov:NonPosDefHessian');

            for thisReplicate = 1:nReplicates

                pGuess = rand;
                muGuess = ((3*rand)-1.5);
                sigmaGuess = 2*rand;
                parameterGuess = [pGuess muGuess sigmaGuess];
                parameterLowerBound = [0 -4 smallNonZeroNumber];
                parameterUpperBound = [1 4 5];
                
                [currentEstimates, currentCIs] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);

                % Compute negative log likelihood
                thisNegLogLikelihood = -sum(log(pdf_normmixture(theseErrors,currentEstimates(1),currentEstimates(2),currentEstimates(3))));

                if minNegLogLikelihood > thisNegLogLikelihood
                    minNegLogLikelihood = thisNegLogLikelihood;
                    bestEstimates = currentEstimates;
                    bestEstimateCIs = currentCIs;
                end

            end

            warning('on', 'stats:mlecov:NonPosDefHessian');

            
            % Test for a significant difference in log likelihoods
            [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);
        
            if h==0
            
                % Null model not rejected; use uniform only

                allEstimates_byParticipant(thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allLowerBounds_byParticipant(thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allUpperBounds_byParticipant(thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];

            else

                % Use mixture

                allEstimates_byParticipant(thisCondition,thisStream,thisParticipant,:) = bestEstimates;
                allLowerBounds_byParticipant(thisCondition,thisStream,thisParticipant,:) = bestEstimateCIs(1,:);
                allUpperBounds_byParticipant(thisCondition,thisStream,thisParticipant,:) = bestEstimateCIs(2,:);
                
            end

        end
        
    end
    
end



