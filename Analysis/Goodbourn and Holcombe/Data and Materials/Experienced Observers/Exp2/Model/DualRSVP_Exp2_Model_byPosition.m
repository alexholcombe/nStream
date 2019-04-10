clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP/Exp2/Data/');

% Task parameters

letterArray = [char(65:77) char(79:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nPositions = 12;
nStreams = 2;
nTrials = 25;                   % Per participant, per condition, per session
nSessions = 4;              
thisCondition = 1;

% Participant details

allParticipants = {'EL','FJ','LS','PG','SY','WC'};

% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @pdf_Mixture_Exp1;
pdf_uniformonly = @pdf_Uniform_Exp1;
nReplicates = 100;
pCrit = .05;
smallNonZeroNumber = 10^-10;
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

compiledErrors = NaN(nPositions,nParticipants,nSessions,nStreams,nTrials);
compiledTargets = NaN(nPositions,nParticipants,nSessions,nStreams,nTrials);
compiledResponses = NaN(nPositions,nParticipants,nSessions,nStreams,nTrials);
compiledResponsePositions = NaN(nPositions,nParticipants,nSessions,nStreams,nTrials);

% Find relevant files

fileList = what;
fileList = fileList.mat;
  
for thisParticipant = 1:nParticipants

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
            
            thisPositionNo = targetPosition(1)-6;

            % Store

            compiledErrors(thisPositionNo,thisParticipant,thisSession,:,thisTrial) = positionError';
            compiledTargets(thisPositionNo,thisParticipant,thisSession,:,thisTrial) = targetLetters';
            compiledResponses(thisPositionNo,thisParticipant,thisSession,:,thisTrial) = responseLetters';
            compiledResponsePositions(thisPositionNo,thisParticipant,thisSession,:,thisTrial) = responsePosition';

        end

    end

end


% MODEL -------------------------------------------------------------------

% Combined across participants

% Build data structures

allEstimates_Combined = NaN(nPositions,nStreams,nFreeParameters);
allLowerBounds_Combined = NaN(nPositions,nStreams,nFreeParameters);
allUpperBounds_Combined = NaN(nPositions,nStreams,nFreeParameters);

% Set options

options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

for thisPosition = 1:nPositions
    
    fprintf('\n\nPosition %d / %d...', thisPosition, nPositions);
    
    for thisStream = 1:nStreams
       
        minNegLogLikelihood = inf;
        
        theseErrors = squeeze(compiledErrors(thisPosition,:,:,thisStream,:));
        theseErrors = theseErrors(:);
        theseErrors = theseErrors(~isnan(theseErrors));
        
        % Compute negative log likelihood for uniform distribution
        
        uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));
        
        warning('off', 'stats:mlecov:NonPosDefHessian');
        
        % Get MLE estimates for mixture model
        
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
            
            allEstimates_Combined(thisPosition,thisStream,:) = [0 NaN NaN];
            allLowerBounds_Combined(thisPosition,thisStream,:) = [0 NaN NaN];
            allUpperBounds_Combined(thisPosition,thisStream,:) = [0 NaN NaN];
            
        else
            
            % Use mixture
            
            allEstimates_Combined(thisPosition,thisStream,:) = bestEstimates;
            allLowerBounds_Combined(thisPosition,thisStream,:) = bestEstimateCIs(1,:);
            allUpperBounds_Combined(thisPosition,thisStream,:) = bestEstimateCIs(2,:);
            
        end
        

        
    end
    
end