clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP/Exp3/Data/');

%addpath(genpath('/Users/ptg/Dropbox/MATLAB/'));
%cd('/Users/ptg/Dropbox/MATLAB/DualRSVP/Exp3/Data/');

% Task parameters

letterArray = [char(65:66) char(68:85) char(87:90)];      % A to Z
nLeadTrailItems = 6;            % Nuumber of items in stream at beginning and end with no target
nConditions = 2;
nStreams = 2;
nTrials = 50;                   % Per participant, per condition, per session
nSessions = 4;                  % !!!!!!!!!!!!!!

% Participant details

allParticipants = {'AH','FJ','PG','RG','SM','WC'};


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


% % MODEL -------------------------------------------------------------------
% 
% % Combined across participants
% 
% % Build data structures
% 
% allEstimates_Combined = NaN(nConditions,nStreams,nFreeParameters);
% allLowerBounds_Combined = NaN(nConditions,nStreams,nFreeParameters);
% allUpperBounds_Combined = NaN(nConditions,nStreams,nFreeParameters);
% 
% % Set options
% 
% options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');
% 
% for thisCondition = 1:nConditions
%     
%     for thisStream = 1:nStreams
%        
%         minNegLogLikelihood = inf;
%         
%         theseErrors = squeeze(compiledErrors(thisCondition,:,:,:,thisStream));
%         theseErrors = theseErrors(:);
%         theseErrors = theseErrors(~isnan(theseErrors));
%         
%         % Compute negative log likelihood for uniform distribution
%         
%         uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));
%         
%         warning('off', 'stats:mlecov:NonPosDefHessian');
%         
%         % Get MLE estimates for mixture model
%         
%         for thisReplicate = 1:nReplicates
%         
%             pGuess = rand;
%             muGuess = ((3*rand)-1.5);
%             sigmaGuess = 2*rand;
%             parameterGuess = [pGuess muGuess sigmaGuess];
%             parameterLowerBound = [0 -4 smallNonZeroNumber];
%             parameterUpperBound = [1 4 5];
%             
%             [currentEstimates, currentCIs] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
%             
%             % Compute negative log likelihood
%             
%             thisNegLogLikelihood = -sum(log(pdf_normmixture(theseErrors,currentEstimates(1),currentEstimates(2),currentEstimates(3))));
%             
%             if minNegLogLikelihood > thisNegLogLikelihood
%                 
%                 minNegLogLikelihood = thisNegLogLikelihood;
%                 bestEstimates = currentEstimates;
%                 bestEstimateCIs = currentCIs;
%                 
%             end
%             
%         end
%         
%         warning('on', 'stats:mlecov:NonPosDefHessian');
%         
%         % Test for a significant difference in log likelihoods
%         [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);
%         
%         if h==0
%             
%             % Null model not rejected; use uniform only
%             
%             allEstimates_Combined(thisCondition,thisStream,:) = [0 NaN NaN];
%             allLowerBounds_Combined(thisCondition,thisStream,:) = [0 NaN NaN];
%             allUpperBounds_Combined(thisCondition,thisStream,:) = [0 NaN NaN];
%             
%         else
%             
%             % Use mixture
%             
%             allEstimates_Combined(thisCondition,thisStream,:) = bestEstimates;
%             allLowerBounds_Combined(thisCondition,thisStream,:) = bestEstimateCIs(1,:);
%             allUpperBounds_Combined(thisCondition,thisStream,:) = bestEstimateCIs(2,:);
%             
%         end
%         
% 
%         
%     end
%     
% end


% Separated by participant

% Build data structures
allEstimates_byParticipant = NaN(nConditions,nStreams,nParticipants,nFreeParameters);
allLowerBounds_byParticipant = NaN(nConditions,nStreams,nParticipants,nFreeParameters);
allUpperBounds_byParticipant = NaN(nConditions,nStreams,nParticipants,nFreeParameters);

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

for thisCondition = 1:nConditions
    
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
%         end
%         
%     end
%     
% end
% 
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
% 
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


% Save results
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP/Exp3/Model/');
save('DualRSVP_Exp3_Outputs_2Conditions.mat');
