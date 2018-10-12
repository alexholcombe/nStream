% SET ---------------------------------------------------------------------
clear all;

% Add directories
usePath = '~/Google Drive/nStream/';
dataDirectory = [usePath 'modelOutput/compiled/'];

% Task parameters
sampleNames = {'Ex6Streams115msSOA','Ex8Streams82msSOA','End6Streams82msSOA'};

itemRate = 7.5;

letterArray = char(65:90);      % A to Z
nConditions = 1;
nStreams = 1;
nParticipants = 2;
nTrials = 200;
nSessions = 1;
nSamples = numel(sampleNames);

% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @TGAB_pdf_logNorm_Mixture_Single; % We can use the single-episode AB model
pdf_global = @TGAB_pdf_logNorm_Mixture_Single_global_returns;
pdf_uniformonly = @TG_pdf_Uniform;
nReplicates = 200;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^4;
fitMaxFunEvals = 10^4;

% Declare global variables that need to be accessed by the objective
% function.
global xDomain;
global pseudo_uniform;

% Add folders to the MATLAB path.
addpath(genpath(usePath));

% CALCULATE ---------------------------------------------------------------

% Task parameters
nLetters = length(letterArray); % Number of possible letters
rateFactor = 1000./itemRate;

% Build data structures
allAccuracy_byParticipant = NaN(nSamples,nParticipants);
allEstimates_byParticipant = NaN(nSamples,nParticipants,nFreeParameters);
allLowerBounds_byParticipant = NaN(nSamples,nParticipants,nFreeParameters);
allUpperBounds_byParticipant = NaN(nSamples,nParticipants,nFreeParameters);
allMinNegLogLikelihoods_byParticipant = NaN(nSamples,nParticipants);
allNTrials_byParticipant = NaN(nSamples,nParticipants)


for thisSample = 1:nSamples
    
    % Load data
    cd(dataDirectory);
    load(['CompiledData_TGRSVP_Exp2_' sampleNames{thisSample} '.mat']);
    
    
    
    % compiledErrors(thisParticipant,thisSession,thisTrial);
    % compiledTargets(thisParticipant,thisSession,thisTrial);

    nParticipants = size(compiledErrors);
    nParticipants = nParticipants(1);
    
    % MODEL -------------------------------------------------------------------

    % Work out possible positions in the stream for T1.
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

    % Set options
    options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');

    for thisCondition = 1:nConditions

        for thisStream = 1:nStreams

            for thisParticipant = 1:nParticipants
                
                fprintf('Group: %s. Participant: %d \n\r',sampleNames{thisSample}, thisParticipant) 
                minNegLogLikelihood = inf;

                theseErrors = squeeze(compiledErrors(thisParticipant,:,:));
                theseErrors = theseErrors(:);
                theseErrors = theseErrors(~isnan(theseErrors));

                 % Compute negative log likelihood for uniform distribution

                uniformNegLogLikelihood = -sum(log(pdf_uniformonly(theseErrors,1)));

                warning('off', 'stats:mlecov:NonPosDefHessian');

                for thisReplicate = 1:nReplicates

                    pGuess = rand;
                    muGuess = rand;
                    sigmaGuess = 2*rand;
                    while sigmaGuess<=1
                        sigmaGuess = 2*rand;
                    end
                    parameterGuess = [pGuess muGuess sigmaGuess];
                    parameterLowerBound = [0 0 (1+1e-4)]; %might have to replace the mu bound with small nonzero number. Not clear if bounds are open or not. Sigma cannot be 1 or less
                    parameterUpperBound = [1 4 20];

                    [currentEstimates, currentCIs] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
                    %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
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

                    allEstimates_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];
                    allLowerBounds_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];
                    allUpperBounds_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];

                else

                    % Use mixture

                    allEstimates_byParticipant(thisSample,thisParticipant,:) = bestEstimates.*[1 rateFactor rateFactor];
                    allLowerBounds_byParticipant(thisSample,thisParticipant,:) = bestEstimateCIs(1,:).*[1 rateFactor rateFactor];
                    allUpperBounds_byParticipant(thisSample,thisParticipant,:) = bestEstimateCIs(2,:).*[1 rateFactor rateFactor];
                    allMinNegLogLikelihoods_byParticipant(thisSample, thisParticipant) = minNegLogLikelihood;
                    allNTrials_byParticipant(thisSample, thisParticipant) = numel(theseErrors);
                    
                end
                
                thisAccuracy = sum(theseErrors==0)/numel(theseErrors);
                allAccuracy_byParticipant(thisSample,thisParticipant) = thisAccuracy;

            end

        end

    end

end

% Write the data to *.csv files for analysis in JASP
cd([usePath 'modelOutput/CSV/']);

% Accuracy
writeFile = fopen('TGRSVP_Exp2_AccuracyLogNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allAccuracy_byParticipant(thisSample,thisParticipant));
            end
        end
    end
end

% Efficacy
writeFile = fopen('TGRSVP_Exp2_EfficacyLogNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,1));
            end
        end
    end
end

% Latency
writeFile = fopen('TGRSVP_Exp2_LatencyLogNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,2));
            end
        end
    end
end

% Precision
writeFile = fopen('TGRSVP_Exp2_PrecisionLogNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,3));
            end
        end
    end
end

cd('../Likelihood/') %Save the likelihood information for model comparison

save('logNormalModelLikelihood', 'allMinNegLogLikelihoods_byParticipant', 'allNTrials_byParticipant')