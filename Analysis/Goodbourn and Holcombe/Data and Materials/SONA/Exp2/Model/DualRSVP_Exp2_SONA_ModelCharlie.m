clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('~/Google Drive/AllRSVP/DualRSVP_SONA/Exp2'));
cd('~/Google Drive/AllRSVP/DualRSVP_SONA/Exp2/Data/');

%addpath(genpath('/Users/ptg/Dropbox/MATLAB/'));
%cd('/Users/ptg/Dropbox/MATLAB/DualRSVP/Exp3/Data/');

% Task parameters

letterArray = [char(65:66) char(68:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nConditions = 2;
nStreams = 2;
nTrials = 50;                   % Per participant, per condition, per session
nSessions = 2;
nModels = 2;

% Participant details

allParticipants = {'KA','KB','KC','KD','KE','KF','KG','KH','KI','KJ','KK','KL','KM','KO','KP','KQ','KR','KS','KT','KU'};
allSex = [0 1 0 0 1 0 1 1 0 0 1 1 0 1 0 1 1 0 1 0];
allAge = [19 18 20 19 20 18 18 18 18 19 20 22 19 22 20 18 18 19 19 24];
EHI = [275 -150 200 350 400 300 0 400 325 0 400 350 175 375 325 400 400 400 400 400];



% Model fitting parameters

nFreeParameters = 3;
pdf_LogNormMixture = @pdf_logNorm_Mixture_Single;
pdf_NormMixture = @pdf_norm_Mixture_Single
pdf_uniformonly = @pdf_Uniform;
nReplicates = 1;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^4;
fitMaxFunEvals = 10^4;

% Declare global variables that need to be accessed by the objective
% function.
global xDomain;
global pseudo_uniform;

% Categorisation parameters

nCategories = 2;            % Target or Error
categoryCriterion = .5;    % Required probability that trial is a target or error for it to be classified as such

% Figure axes


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

    thisPrefix = [allParticipants{thisParticipant} '_'];
    loadFiles = find(strncmp(thisPrefix,fileList,numel(thisPrefix)))';

    % Load and compile

    for thisSession = 1:length(loadFiles)

        load(fileList{loadFiles(thisSession)});
        
        trialCounter = [0 0 0 0];

        for thisTrial = 1:totalTrials

            % Calculate
            
            thisCondition = allPositions(thisTrial);
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

listT1Pos = unique(compiledTargets(:));
listT1Pos(isnan(listT1Pos)) = [];
nT1Pos = numel(listT1Pos);

% Calculate the domain of possible errors (xDomain).
minX_T1 = min(listT1Pos);
maxX_T1 = max(listT1Pos);
minErr = 1-maxX_T1-1;
maxErr = nLetters-minX_T1+1;
xDomain = minErr:maxErr;

% Generate the 'pseudo-uniform' distribution, which is the
% expected distribution of errors if a random guess was
% provided on every trial. This isn't an actual uniform
% distribution because the most extreme errors are only
% possible on trials in which targets appear at their most
% extreme positions.
pseudo_uniform = zeros(size(xDomain));


% Cycle through each possible T1 position.
for thisPosNo = 1:numel(listT1Pos)

    % Identify the actual T1 position corresponding to the
    % position number. For example, the first position number
    % might be the 7th position in the stream.
    thisPos = listT1Pos(thisPosNo);

    % Add to the pseudo-uniform distribution one unit for every
    % possible error given that T1 position.
    pseudo_uniform((1-thisPos-minErr+1):(nLetters-thisPos-minErr+1)) = pseudo_uniform((1-thisPos-minErr+1):(nLetters-thisPos-minErr+1))+ones(1,nLetters);

end

% MODEL -------------------------------------------------------------------

% Combined across participants

% Build data structures

allEstimates_Combined = NaN(nModels,nConditions,nStreams,nFreeParameters);
allLowerBounds_Combined = NaN(nModels,nConditions,nStreams,nFreeParameters);
allUpperBounds_Combined = NaN(nModels,nConditions,nStreams,nFreeParameters);
allBICs_Combined = NaN(nModels, nConditions, nStreams);
% Set options

options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

for thisCondition = 1:nConditions
    
    for thisStream = 1:nStreams
        fprintf('This condition: %d. This stream %d \r', thisCondition, thisStream)
        
        minNegLogLikelihoodNorm = inf;
        minNegLogLikelihoodLogNorm = inf;
        
        
        theseErrors = squeeze(compiledErrors(thisCondition,:,:,:,thisStream));
        theseErrors = theseErrors(:);
        theseErrors = theseErrors(~isnan(theseErrors));
        
        % Compute negative log likelihood for uniform distribution
        
        uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));
        
        warning('off', 'stats:mlecov:NonPosDefHessian');
        
        % Get MLE estimates for mixture model
        
        for thisReplicate = 1:nReplicates;
        
            pGuess = rand;
            muGuessNorm = ((3*rand)-1.5);
            sigmaGuessNorm = 2*rand;
            parameterGuessNorm = [pGuess muGuessNorm sigmaGuessNorm];
            parameterLowerBoundNorm = [0 -4 smallNonZeroNumber];
            parameterUpperBoundNorm = [1 4 5];
            
            [currentEstimatesNorm, currentCIsNorm] = mle(theseErrors, 'pdf', pdf_NormMixture, 'start', parameterGuessNorm, 'lower', parameterLowerBoundNorm, 'upper', parameterUpperBoundNorm, 'options', options);
            
            % Compute negative log likelihood
            
            thisNegLogLikelihoodNorm = -sum(log(pdf_NormMixture(theseErrors,currentEstimatesNorm(1),currentEstimatesNorm(2),currentEstimatesNorm(3))));
            
            if minNegLogLikelihoodNorm > thisNegLogLikelihoodNorm
                
                minNegLogLikelihoodNorm = thisNegLogLikelihoodNorm;
                bestEstimatesNorm = currentEstimatesNorm;
                bestEstimateCIsNorm = currentCIsNorm;
                
            end
            
            pGuess = rand;
            muGuessLogNorm = abs(((3*rand)-1.5));
            sigmaGuessLogNorm = 2*rand;
            while sigmaGuessLogNorm<=(1+1e-2)
                sigmaGuessLogNorm = 2*rand;
            end
            while muGuessLogNorm < 0
                 muGuessLogNorm = abs(((3*rand)-1.5));
            end
            parameterGuessLogNorm = [pGuess muGuessLogNorm sigmaGuessLogNorm];
            parameterLowerBoundLogNorm = [0 0 (1+1e-2)]; %can't have log(SD) = 0
            parameterUpperBoundLogNorm = [1 4 5];
            
            [currentEstimatesLogNorm, currentCIsLogNorm] = mle(theseErrors, 'pdf', pdf_LogNormMixture, 'start', parameterGuessLogNorm, 'lower', parameterLowerBoundLogNorm, 'upper', parameterUpperBoundLogNorm, 'options', options);
            
            thisNegLogLikelihoodLogNorm = -sum(log(pdf_LogNormMixture(theseErrors,currentEstimatesLogNorm(1),currentEstimatesLogNorm(2),currentEstimatesLogNorm(3))));
            
            if minNegLogLikelihoodLogNorm > thisNegLogLikelihoodLogNorm
                
                minNegLogLikelihoodLogNorm = thisNegLogLikelihoodLogNorm;
                bestEstimatesLogNorm = currentEstimatesLogNorm;
                bestEstimateCIsLogNorm = currentCIsLogNorm;
                
            end
            
            
        end
        
        warning('on', 'stats:mlecov:NonPosDefHessian');
        
        % Test for a significant difference in log likelihoods
        [hNorm,pValueNorm,statNorm,cValueNorm] = lratiotest(-minNegLogLikelihoodNorm,-uniformNegLogLikelihood,nFreeParameters,pCrit);
        
        if hNorm==0
            
            % Null model not rejected; use uniform only
            
            allEstimates_Combined(1,thisCondition,thisStream,:) = [0 NaN NaN];
            allLowerBounds_Combined(1,thisCondition,thisStream,:) = [0 NaN NaN];
            allUpperBounds_Combined(1,thisCondition,thisStream,:) = [0 NaN NaN];
            
        else
            
            % Use mixture
            
            allEstimates_Combined(1,thisCondition,thisStream,:) = bestEstimatesNorm;
            allLowerBounds_Combined(1,thisCondition,thisStream,:) = bestEstimateCIsNorm(1,:);
            allUpperBounds_Combined(1,thisCondition,thisStream,:) = bestEstimateCIsNorm(2,:);
            
        end
        
                % Test for a significant difference in log likelihoods
        [hLogNorm,pValueLogNorm,statLogNorm,cValueLogNorm] = lratiotest(-minNegLogLikelihoodLogNorm,-uniformNegLogLikelihood,nFreeParameters,pCrit);
        
        if hLogNorm==0
            fprintf('Uniform not rejected for condition: %d and stream %d', thisCondition, thisStream)
            % Null model not rejected; use uniform only
            
            allEstimates_Combined(2,thisCondition,thisStream,:) = [0 NaN NaN];
            allLowerBounds_Combined(2,thisCondition,thisStream,:) = [0 NaN NaN];
            allUpperBounds_Combined(2,thisCondition,thisStream,:) = [0 NaN NaN];
            
        else
            
            % Use mixture
            
            allEstimates_Combined(2,thisCondition,thisStream,:) = bestEstimatesLogNorm;
            allLowerBounds_Combined(2,thisCondition,thisStream,:) = bestEstimateCIsLogNorm(1,:);
            allUpperBounds_Combined(2,thisCondition,thisStream,:) = bestEstimateCIsLogNorm(2,:);
            
        end
        
        [thisAICNorm thisBICNorm] = aicbic(-minNegLogLikelihoodNorm ,nFreeParameters, numel(theseErrors));
        [thisAICLogNorm thisBICLogNorm] = aicbic(-minNegLogLikelihoodLogNorm ,nFreeParameters, numel(theseErrors));
        
        allBICs_Combined(1,thisCondition,thisStream) = thisBICNorm;
        allBICs_Combined(2,thisCondition,thisStream) = thisBICLogNorm;
        
    end
end
    

BFs = squeeze(exp(-.5*(allBICs_Combined(1,:,:) - allBICs_Combined(2,:,:))));

BFFile = fopen('BayesFactorsGaussianNumerator.csv','w')
csvwrite('BayesFactorsGaussianNumerator.csv', BFs)

% Separated by participant
% Build data structures
allEstimates_byParticipant = NaN(nModels,nConditions,nStreams,nParticipants,nFreeParameters);
allLowerBounds_byParticipant = NaN(nModels,nConditions,nStreams,nParticipants,nFreeParameters);
allUpperBounds_byParticipant = NaN(nModels,nConditions,nStreams,nParticipants,nFreeParameters);
allBICs_byParticipant = NaN(nModels, nConditions,nStreams,nParticipants);

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

for thisCondition = 1:nConditions
    
    for thisStream = 1:nStreams
        
        for thisParticipant = 1:nParticipants
            fprintf('This condition: %d. This stream %d. This Participant %d \r', thisCondition, thisStream, thisParticipant)
       
            minNegLogLikelihoodNorm = inf;
            minNegLogLikelihoodLogNorm = inf;

            theseErrors = squeeze(compiledErrors(thisCondition,thisParticipant,:,:,thisStream));
            theseErrors = theseErrors(:);
            theseErrors = theseErrors(~isnan(theseErrors));
            
             % Compute negative log likelihood for uniform distribution
        
            uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));

            warning('off', 'stats:mlecov:NonPosDefHessian');

            for thisReplicate = 1:nReplicates

                pGuess = rand;
                muGuessNorm = ((3*rand)-1.5);
                sigmaGuessNorm = 2*rand;
                parameterGuessNorm = [pGuess muGuessNorm sigmaGuessNorm];
                parameterLowerBoundNorm = [0 -4 smallNonZeroNumber];
                parameterUpperBoundNorm = [1 4 5];
                
                [currentEstimatesNorm, currentCIsNorm] = mle(theseErrors, 'pdf', pdf_NormMixture, 'start', parameterGuessNorm, 'lower', parameterLowerBoundNorm, 'upper', parameterUpperBoundNorm, 'options', options);
                
                % Compute negative log likelihood
                
                thisNegLogLikelihoodNorm = -sum(log(pdf_NormMixture(theseErrors,currentEstimatesNorm(1),currentEstimatesNorm(2),currentEstimatesNorm(3))));
                
                if minNegLogLikelihoodNorm > thisNegLogLikelihoodNorm
                    
                    minNegLogLikelihoodNorm = thisNegLogLikelihoodNorm;
                    bestEstimatesNorm = currentEstimatesNorm;
                    bestEstimateCIsNorm = currentCIsNorm;
                    
                end
                
                pGuess = rand;
                muGuessLogNorm = abs(((3*rand)-1.5));
                sigmaGuessLogNorm = 2*rand;
                while sigmaGuessLogNorm<=(1+1e-2)
                    sigmaGuessLogNorm = 2*rand;
                end
                while muGuessLogNorm < 0
                     muGuessLogNorm = abs(((3*rand)-1.5));
                end
                parameterGuessLogNorm = [pGuess muGuessLogNorm sigmaGuessLogNorm];
                parameterLowerBoundLogNorm = [0 0 (1+1e-2)]; %can't have log(SD) = 0
                parameterUpperBoundLogNorm = [1 4 5];
                
                [currentEstimatesLogNorm, currentCIsLogNorm] = mle(theseErrors, 'pdf', pdf_LogNormMixture, 'start', parameterGuessLogNorm, 'lower', parameterLowerBoundLogNorm, 'upper', parameterUpperBoundLogNorm, 'options', options);
                
                thisNegLogLikelihoodLogNorm = -sum(log(pdf_LogNormMixture(theseErrors,currentEstimatesLogNorm(1),currentEstimatesLogNorm(2),currentEstimatesLogNorm(3))));
                
                if minNegLogLikelihoodLogNorm > thisNegLogLikelihoodLogNorm
                    
                    minNegLogLikelihoodLogNorm = thisNegLogLikelihoodLogNorm;
                    bestEstimatesLogNorm = currentEstimatesLogNorm;
                    bestEstimateCIsLogNorm = currentCIsLogNorm;
                    
                end
                
                
            end
            
            warning('on', 'stats:mlecov:NonPosDefHessian');
            
            % Test for a significant difference in log likelihoods
            [hNorm,pValueNorm,statNorm,cValueNorm] = lratiotest(-minNegLogLikelihoodNorm,-uniformNegLogLikelihood,nFreeParameters,pCrit);
            
            if hNorm==0
                
                % Null model not rejected; use uniform only
                
                allEstimates_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allLowerBounds_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allUpperBounds_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                
            else
                
                % Use mixture
                
                allEstimates_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = bestEstimatesNorm;
                allLowerBounds_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = bestEstimateCIsNorm(1,:);
                allUpperBounds_byParticipant(1,thisCondition,thisStream,thisParticipant,:) = bestEstimateCIsNorm(2,:);
                
            end
            
                    % Test for a significant difference in log likelihoods
            [hLogNorm,pValueLogNorm,statLogNorm,cValueLogNorm] = lratiotest(-minNegLogLikelihoodLogNorm,-uniformNegLogLikelihood,nFreeParameters,pCrit);
            
            if hLogNorm==0
                fprintf('Uniform not rejected for condition: %d and stream %d', thisCondition, thisStream)
                % Null model not rejected; use uniform only
                
                allEstimates_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allLowerBounds_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                allUpperBounds_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = [0 NaN NaN];
                
            else
                
                % Use mixture
                
                allEstimates_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = bestEstimatesLogNorm;
                allLowerBounds_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = bestEstimateCIsLogNorm(1,:);
                allUpperBounds_byParticipant(2,thisCondition,thisStream,thisParticipant,:) = bestEstimateCIsLogNorm(2,:);
                
            end
            
            [thisAICNorm thisBICNorm] = aicbic(-minNegLogLikelihoodNorm ,nFreeParameters, numel(theseErrors));
            [thisAICLogNorm thisBICLogNorm] = aicbic(-minNegLogLikelihoodLogNorm ,nFreeParameters, numel(theseErrors));
            
            allBICs_byParticipant(1,thisCondition,thisStream,thisParticipant) = thisBICNorm;
            allBICs_byParticipant(2,thisCondition,thisStream,thisParticipant) = thisBICLogNorm;

        end
        
    end
    
end

BFs = squeeze(exp(-.5*(allBICs_byParticipant(1,:,:,:) - allBICs_byParticipant(2,:,:,:))));

BFFile = fopen('BayesFactorsGaussianNumeratorByParticipant.csv','w')
csvwrite('BayesFactorsGaussianNumeratorByParticipantCondition1.csv', BFs(1,:,:))
csvwrite('BayesFactorsGaussianNumeratorByParticipantCondition2.csv', BFs(2,:,:))
% % CATEGORISE --------------------------------------------------------------
% 
% % Set up data structures
% 
% trialIsTarget_Combined = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
% trialIsError_Combined = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
% trialIsTarget_byParticipant = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
% trialIsError_byParticipant = NaN(nConditions,nParticipants,nSessions,nTrials,nStreams);
% 
% % By combined models
% 
% for thisCondition = 1:nConditions
%     
%     for thisStream = 1:nStreams
%         
%         % Get relevant model parameters
%         
%         theseParameters = squeeze(allEstimates_Combined(thisCondition,thisStream,:))';
%         
%         if theseParameters(1) ~= 0
%             
%             % Mixture model
%             
%             thispdf_target = pdf_Normal_Exp1(errorValues,theseParameters(1),theseParameters(2),theseParameters(3));
%             thispdf_error = pdf_Uniform_Exp1(errorValues,1-theseParameters(1));
%             
%             for thisParticipant = 1:nParticipants
%                
%                 for thisSession = 1:nSessions
%                    
%                     for thisTrial = 1:nTrials
%                        
%                         thisError = compiledErrors(thisCondition,thisParticipant,thisSession,thisTrial,thisStream);
%                         
%                         if ~isnan(thisError)
%                             
%                             pTarget = thispdf_target(thisError+offsetFactor)/(thispdf_target(thisError+offsetFactor)+thispdf_error(thisError+offsetFactor));
%                         
%                             if pTarget > categoryCriterion
% 
%                                 trialIsTarget_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 1;
%                                 trialIsError_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
% 
%                             elseif (1-pTarget) > categoryCriterion
% 
%                                 trialIsTarget_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
%                                 trialIsError_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 1;
% 
%                             else
% 
%                                 trialIsTarget_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
%                                 trialIsError_Combined(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
% 
%                             end
%                             
%                         end
%                         
%                     end
%                     
%                 end
%                 
%             end
%             
%         else
%             
%             % Uniform only (thus all errors)
%             
%             trialIsTarget_Combined(thisCondition,:,:,:,thisStream) = zeros(nParticipants,nSessions,nTrials);
%             trialIsError_Combined(thisCondition,:,:,:,thisStream) = ones(nParticipants,nSessions,nTrials);
%             
% %         end
%         
%     end
%     
% end
% 

% 
% % By individual models
% 
% for thisCondition = 1:nConditions
%     
%     for thisStream = 1:nStreams
%         
%         for thisParticipant = 1:nParticipants
%         
%             % Get relevant model parameters
% 
%             theseParameters = squeeze(allEstimates_byParticipant(thisCondition,thisStream,thisParticipant,:))';
% 
%             if theseParameters(1) ~= 0
% 
%                 % Mixture model
% 
%                 thispdf_target = pdf_Normal_Exp1(errorValues,theseParameters(1),theseParameters(2),theseParameters(3));
%                 thispdf_error = pdf_Uniform_Exp1(errorValues,1-theseParameters(1));
% 
%                 for thisSession = 1:nSessions
% 
%                     for thisTrial = 1:nTrials
% 
%                         thisError = compiledErrors(thisCondition,thisParticipant,thisSession,thisTrial,thisStream);
%                         
%                         if ~isnan(thisError)
%                         
%                             pTarget = thispdf_target(thisError+offsetFactor)/(thispdf_target(thisError+offsetFactor)+thispdf_error(thisError+offsetFactor));
% 
%                             if pTarget > categoryCriterion
% 
%                                 trialIsTarget_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 1;
%                                 trialIsError_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
% 
%                             elseif (1-pTarget) > categoryCriterion
% 
%                                 trialIsTarget_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
%                                 trialIsError_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 1;
% 
%                             else
% 
%                                 trialIsTarget_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
%                                 trialIsError_byParticipant(thisCondition,thisParticipant,thisSession,thisTrial,thisStream) = 0;
% 
%                             end
%                             
%                         end
% 
%                     end
% 
%                 end
% 
%             else
% 
%                 % Uniform only (thus all errors)
% 
%                 trialIsTarget_byParticipant(thisCondition,thisParticipant,:,:,thisStream) = zeros(nSessions,nTrials);
%                 trialIsError_byParticipant(thisCondition,thisParticipant,:,:,thisStream) = ones(nSessions,nTrials);
% 
%             end
%         
%         end
%         
%     end
%     
% end
% 
% 
% % CONTINGENT MODEL --------------------------------------------------------
% 
% 
% % MODEL -------------------------------------------------------------------
% 
% % Combined across participants
% 
% % Build data structures
% 
% allEstimates_Contingent_Combined = NaN(nConditions,nStreams,nCategories,nFreeParameters);
% allLowerBounds_Contingent_Combined = NaN(nConditions,nStreams,nCategories,nFreeParameters);
% allUpperBounds_Contingent_Combined = NaN(nConditions,nStreams,nCategories,nFreeParameters);
% 
% % Set options
% 
% options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');
% 
% for thisCondition = 1:nConditions
%     
%     for thisStream = 1:nStreams
%         
%         otherStream = 3-thisStream;
%         
%         for thisCategory = 1:nCategories
%        
%             minNegLogLikelihood = inf;
% 
%             if thisCategory==1
%                 % Target
%                 relevantTrials = squeeze(trialIsTarget_Combined(thisCondition,:,:,:,otherStream));
%             else
%                 % Error
%                 relevantTrials = squeeze(trialIsError_Combined(thisCondition,:,:,:,otherStream));
%             end
%             
%             theseErrors = squeeze(compiledErrors(thisCondition,:,:,:,thisStream));
%             theseErrors = theseErrors(relevantTrials==1);
% 
%             % Compute negative log likelihood for uniform distribution
% 
%             uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));
% 
%             warning('off', 'stats:mlecov:NonPosDefHessian');
% 
%             % Get MLE estimates for mixture model
% 
%             for thisReplicate = 1:nReplicates
% 
%                 pGuess = rand;
%                 muGuess = maxError*((2*rand)-1);
%                 sigmaGuess = rand*std(theseErrors);
%                 parameterGuess = [pGuess muGuess sigmaGuess];
%                 parameterLowerBound = [0 -maxError smallNonZeroNumber];
%                 parameterUpperBound = [1 maxError std(theseErrors)];
%                 [currentEstimates, currentCIs] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
% 
%                 % Compute negative log likelihood
% 
%                 thisNegLogLikelihood = -sum(log(pdf_normmixture(theseErrors,currentEstimates(1),currentEstimates(2),currentEstimates(3))));
% 
%                 if minNegLogLikelihood > thisNegLogLikelihood
% 
%                     minNegLogLikelihood = thisNegLogLikelihood;
%                     bestEstimates = currentEstimates;
%                     bestEstimateCIs = currentCIs;
% 
%                 end
% 
%             end
% 
%             warning('on', 'stats:mlecov:NonPosDefHessian');
% 
%             % Test for a significant difference in log likelihoods
%             [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);
% 
%             if h==0
% 
%                 % Null model not rejected; use uniform only
% 
%                 allEstimates_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = [0 NaN NaN];
%                 allLowerBounds_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = [0 NaN NaN];
%                 allUpperBounds_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = [0 NaN NaN];
% 
%             else
% 
%                 % Use mixture
% 
%                 allEstimates_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = bestEstimates;
%                 allLowerBounds_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = bestEstimateCIs(1,:);
%                 allUpperBounds_Contingent_Combined(thisCondition,thisStream,thisCategory,:) = bestEstimateCIs(2,:);
% 
%             end
%         
%         end
%         
%     end
%     
% end
% % 
% % Separated by participant
% 
% % Build data structures
% allEstimates_Contingent_byParticipant = NaN(nConditions,nStreams,nParticipants,nCategories,nFreeParameters);
% allLowerBounds_Contingent_byParticipant = NaN(nConditions,nStreams,nParticipants,nCategories,nFreeParameters);
% allUpperBounds_Contingent_byParticipant = NaN(nConditions,nStreams,nParticipants,nCategories,nFreeParameters);
% 
% % Set options
% options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');
% 
% for thisCondition = 1:nConditions
%     
%     for thisStream = 1:nStreams
%         
%         otherStream = 3-thisStream;
%         
%         for thisParticipant = 1:nParticipants
%             
%             for thisCategory = 1:nCategories
%        
%                 minNegLogLikelihood = inf;
% 
%                 if thisCategory==1
%                 % Target
%                     relevantTrials = squeeze(trialIsTarget_byParticipant(thisCondition,thisParticipant,:,:,otherStream));
%                 else
%                     % Error
%                     relevantTrials = squeeze(trialIsError_byParticipant(thisCondition,thisParticipant,:,:,otherStream));
%                 end
% 
%                 theseErrors = squeeze(compiledErrors(thisCondition,:,:,:,thisStream));
%                 theseErrors = theseErrors(relevantTrials==1);
% 
%                 if isempty(theseErrors)
%                     
%                         allEstimates_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [NaN NaN NaN];
%                         allLowerBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [NaN NaN NaN];
%                         allUpperBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [NaN NaN NaN];
%                     
%                 else
%                 
%                      % Compute negative log likelihood for uniform distribution
% 
%                     uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));
% 
%                     warning('off', 'stats:mlecov:NonPosDefHessian');
% 
%                     for thisReplicate = 1:nReplicates
% 
%                         pGuess = rand;
%                         muGuess = maxError*((2*rand)-1);
%                         sigmaGuess = rand*std(theseErrors);
%                         parameterGuess = [pGuess muGuess sigmaGuess];
%                         parameterLowerBound = [0 -maxError smallNonZeroNumber];
%                         parameterUpperBound = [1 maxError std(theseErrors)];
%                         [currentEstimates, currentCIs] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
% 
%                         % Compute negative log likelihood
%                         thisNegLogLikelihood = -sum(log(pdf_normmixture(theseErrors,currentEstimates(1),currentEstimates(2),currentEstimates(3))));
% 
%                         if minNegLogLikelihood > thisNegLogLikelihood
%                             minNegLogLikelihood = thisNegLogLikelihood;
%                             bestEstimates = currentEstimates;
%                             bestEstimateCIs = currentCIs;
%                         end
% 
%                     end
% 
%                     warning('on', 'stats:mlecov:NonPosDefHessian');
% 
% 
%                     % Test for a significant difference in log likelihoods
%                     [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);
% 
%                     if h==0
% 
%                         % Null model not rejected; use uniform only
% 
%                         allEstimates_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [0 NaN NaN];
%                         allLowerBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [0 NaN NaN];
%                         allUpperBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = [0 NaN NaN];
% 
%                     else
% 
%                         % Use mixture
% 
%                         allEstimates_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = bestEstimates;
%                         allLowerBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = bestEstimateCIs(1,:);
%                         allUpperBounds_Contingent_byParticipant(thisCondition,thisStream,thisParticipant,thisCategory,:) = bestEstimateCIs(2,:);
% 
%                     end
%                 
%                 end
%                 
%             end
% 
%         end
%         
%     end
%     
% end
% 
% 
% % Save results
% 

