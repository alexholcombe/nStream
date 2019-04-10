clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP_SONA/Exp2/Data/');

% Task parameters

letterArray = [char(65:66) char(68:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nConditions = 2;
nStreams = 2;
nTrials = 50;                   % Per participant, per condition, per session
nSessions = 2;

% Participant details

allParticipants = {'KA','KB','KC','KD','KE','KF','KG','KH','KI','KJ','KK','KL','KM','KO','KP','KQ','KR','KS','KT','KU'};
allSex = [0 1 0 0 1 0 1 1 0 0 1 1 0 1 0 1 1 0 1 0];
allAge = [19 18 20 19 20 18 18 18 18 19 20 22 19 22 20 18 18 19 19 24];
EHI = [275 -150 200 350 400 300 0 400 325 0 400 350 175 375 325 400 400 400 400 400];

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
totalTrials = nTrials*nConditions;

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

    % Load and compile
    thisPrefix = allParticipants{thisParticipant};
    loadFiles = find(strncmp(thisPrefix,fileList,numel(thisPrefix)))';
    
    for thisSession = 1:nSessions
    
        load(fileList{loadFiles(thisSession)});

        trialCounter = [0 0];
        
        for thisTrial = 1:totalTrials

            % Calculate
            thisCondition = allPositions(thisTrial);
            
            if thisCondition==3
                thisCondition=1;
            elseif thisCondition==4
                thisCondition=2;
            end
            
            trialCounter(thisCondition) = trialCounter(thisCondition) + 1;
            
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

            compiledErrors(thisCondition,thisParticipant,thisSession,trialCounter(thisCondition),:) = positionError;
            compiledTargets(thisCondition,thisParticipant,thisSession,trialCounter(thisCondition),:) = targetLetters;
            compiledResponses(thisCondition,thisParticipant,thisSession,trialCounter(thisCondition),:) = responseLetters;
            compiledResponsePositions(thisCondition,thisParticipant,thisSession,trialCounter(thisCondition),:) = responsePosition;

        end

    end

    allRates(thisCondition) = itemRate;

end

% MODEL -------------------------------------------------------------------

modelConditions = 2;

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

            theseErrors = squeeze(compiledErrors(thisCondition,thisParticipant,:,:,thisStream));
            theseErrors = theseErrors(:);
            theseErrors = theseErrors(~isnan(theseErrors));
            
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



