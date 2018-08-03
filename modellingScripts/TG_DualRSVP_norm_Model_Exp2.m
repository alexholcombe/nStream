% SET ---------------------------------------------------------------------
clear all;

% Add directories
usePath = '~/gitCode/nStream/';
dataDirectory = [usePath 'modelOutput/compiled/'];



% Task parameters
sampleNames = {'SONA/twoStreams'}; %,'SONA/eightStreams', 'Pilots/End6Strm82msSOA', 'Pilots/Ex6Strm82msSOA'}; %'Ex8Streams82msSOA', 'Ex6Streams115msSOA
itemRates = [12,12];

letterArray = char(65:90);      % A to Z
nConditions = 1;
nStreams = 1;
nParticipants = [10 10 6 4];
nTrials = 360;
nSessions = 1;
nSamples = numel(sampleNames);

% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @TGAB_pdf_Mixture_Single; % We can use the single-episode AB model
%pdf_global = @TGAB_pdf_logNorm_Mixture_Single_global_returns; %This is a
%debuging function that returns all the variables used to calculate the pdf
pdf_uniformonly = @TG_pdf_Uniform;
nReplicates = 200;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^5;
fitMaxFunEvals = 10^5;

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');


% Declare global variables that need to be accessed by the objective
% function.
global xDomain;
global pseudo_uniform;

% Add folders to the MATLAB path.
addpath(genpath(usePath));

% CALCULATE ---------------------------------------------------------------

% Task parameters
nLetters = length(letterArray); % Number of possible letters
rateFactors = 1000./itemRates;

% Build data structures
allAccuracy_byParticipant = NaN(nSamples,max(nParticipants));
allEstimates_byParticipant = NaN(nSamples,max(nParticipants),nFreeParameters);
allLowerBounds_byParticipant = NaN(nSamples,max(nParticipants),nFreeParameters);
allUpperBounds_byParticipant = NaN(nSamples,max(nParticipants),nFreeParameters);
allMinNegLogLikelihoods_byParticipant = NaN(nSamples,max(nParticipants));
allNTrials_byParticipant = NaN(nSamples,max(nParticipants))


allAccuracy_Combined = NaN(nSamples);
allEstimates_Combined = NaN(nSamples,nFreeParameters);
allLowerBounds_Combined = NaN(nSamples,nFreeParameters);
allUpperBounds_Combined = NaN(nSamples,nFreeParameters);
allMinNegLogLikelihoods_Combined = NaN(1,nSamples);
allNTrials_Combined = NaN(1,nSamples);

for thisSample = 1:nSamples
    
    fprintf('MLE by Condition for %s \n', sampleNames{thisSample})
    
    splitName = strsplit(sampleNames{thisSample},'/');
    folder = splitName{1};
    group = splitName{2};
    
    % Load data
    cd(dataDirectory);
    load([folder '/CompiledData_TGRSVP_Exp2_' group '.mat']);
    
    
    
    % compiledErrors(thisParticipant,thisSession,thisTrial);
    % compiledTargets(thisParticipant,thisSession,thisTrial);

    thisNParticipants = size(compiledErrors);
    thisNParticipants = thisNParticipants(1);
    
    rateFactor = rateFactors(thisSample);
    
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
    
    %estimates by condition, averaged over participants
    
    minNegLogLikelihoodCombined = inf;
    
    theseErrorsCombined = squeeze(compiledErrors);
    theseErrorsCombined = theseErrorsCombined(:);
    theseErrorsCombined = theseErrorsCombined(~isnan(theseErrorsCombined));

     % Compute negative log likelihood for uniform distribution

    uniformNegLogLikelihoodCombined = -sum(log(pdf_uniformonly(theseErrorsCombined,1)));

    warning('off', 'stats:mlecov:NonPosDefHessian');

    for thisReplicate = 1:nReplicates
        pGuess = rand;
        muGuess = ((3*rand)-1.5);
        sigmaGuess = 2*rand;
        parameterGuess = [pGuess muGuess sigmaGuess];
        parameterLowerBound = [0 -4 smallNonZeroNumber];
        parameterUpperBound = [1 4 5];

        [currentEstimatesCombined, currentCIsCombined] = mle(theseErrorsCombined, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
        %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
        % Compute negative log likelihood
        thisNegLogLikelihoodCombined = -sum(log(pdf_normmixture(theseErrorsCombined,currentEstimatesCombined(1),currentEstimatesCombined(2),currentEstimatesCombined(3))));

        if minNegLogLikelihoodCombined > thisNegLogLikelihoodCombined
            minNegLogLikelihoodCombined = thisNegLogLikelihoodCombined;
            bestEstimatesCombined = currentEstimatesCombined;
            bestEstimateCIsCombined = currentCIsCombined;
        end

    end
    

    % Test for a significant difference in log likelihoods
    [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihoodCombined,-uniformNegLogLikelihoodCombined,nFreeParameters,pCrit);

    if h==0
        fprintf('Null model not rejected for sample %s', sampleNames{thisSample})
        % Null model not rejected; use uniform only

        allEstimates_Combined(thisSample,:) = [0 NaN NaN];
        allLowerBounds_Combined(thisSample,:) = [0 NaN NaN];
        allUpperBounds_Combined(thisSample,:) = [0 NaN NaN];
        allMinNegLogLikelihoods_Combined(1,thisSample) = minNegLogLikelihoodCombined;
        allNTrials_Combined(1,thisSample) = numel(theseErrorsCombined);

    else

        % Use mixture

        allEstimates_Combined(thisSample,:) = bestEstimatesCombined.*[1 rateFactor rateFactor];
        allLowerBounds_Combined(thisSample,:) = bestEstimateCIsCombined(1,:).*[1 rateFactor rateFactor];
        allUpperBounds_Combined(thisSample,:) = bestEstimateCIsCombined(2,:).*[1 rateFactor rateFactor];
        allMinNegLogLikelihoods_Combined(1,thisSample) = minNegLogLikelihoodCombined;
        allNTrials_Combined(1,thisSample) = numel(theseErrorsCombined);

    end

    
    % Load data
    cd(dataDirectory);
    load( [folder '/CompiledData_TGRSVP_Exp2_' group '.mat']);
   
    
    
    
    % compiledErrors(thisParticipant,thisSession,thisTrial);
    % compiledTargets(thisParticipant,thisSession,thisTrial);

    thisNParticipants = size(compiledErrors);
    thisNParticipants = thisNParticipants(1);
    
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

    
    for thisCondition = 1:nConditions

        for thisStream = 1:nStreams

            for thisParticipant = 1:thisNParticipants
                
                fprintf('Group: %s. Participant: %d \n\r',sampleNames{thisSample}, thisParticipant) 
                minNegLogLikelihoodByParticipant = inf;

                theseErrorsByParticipant = squeeze(compiledErrors(thisParticipant,:,:));
                theseErrorsByParticipant = theseErrorsByParticipant(:);
                theseErrorsByParticipant = theseErrorsByParticipant(~isnan(theseErrorsByParticipant));

                 % Compute negative log likelihood for uniform distribution

                uniformNegLogLikelihoodByParticipant = -sum(log(pdf_uniformonly(theseErrorsByParticipant,1)));

                warning('off', 'stats:mlecov:NonPosDefHessian');

                for thisReplicate = 1:nReplicates
                    %fprintf('Replicate: %d \n',thisReplicate)


                    pGuess = rand;
                    muGuess = ((3*rand)-1.5);
                    sigmaGuess = 2*rand;
                    parameterGuess = [pGuess muGuess sigmaGuess];
                    parameterLowerBound = [0 -4 smallNonZeroNumber];
                    parameterUpperBound = [1 4 5];

                    [currentEstimatesByParticipant, currentCIsByParticipant] = mle(theseErrorsByParticipant, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);
                    %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
                    % Compute negative log likelihood
                    thisNegLogLikelihoodByParticipant = -sum(log(pdf_normmixture(theseErrorsByParticipant,currentEstimatesByParticipant(1),currentEstimatesByParticipant(2),currentEstimatesByParticipant(3))));

                    if minNegLogLikelihoodByParticipant > thisNegLogLikelihoodByParticipant
                        minNegLogLikelihoodByParticipant = thisNegLogLikelihoodByParticipant;
                        bestEstimatesByParticipant = currentEstimatesByParticipant;
                        bestEstimateCIsByParticipant = currentCIsByParticipant;
                    end

                end

                warning('on', 'stats:mlecov:NonPosDefHessian');


                % Test for a significant difference in log likelihoods
                [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihoodByParticipant,-uniformNegLogLikelihoodByParticipant,nFreeParameters,pCrit);

                if h==0

                    % Null model not rejected; use uniform only

                    allEstimates_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];
                    allLowerBounds_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];
                    allUpperBounds_byParticipant(thisSample,thisParticipant,:) = [0 NaN NaN];

                else

                    % Use mixture

                    allEstimates_byParticipant(thisSample,thisParticipant,:) = bestEstimatesByParticipant.*[1 rateFactor rateFactor];
                    allLowerBounds_byParticipant(thisSample,thisParticipant,:) = bestEstimateCIsByParticipant(1,:).*[1 rateFactor rateFactor];
                    allUpperBounds_byParticipant(thisSample,thisParticipant,:) = bestEstimateCIsByParticipant(2,:).*[1 rateFactor rateFactor];
                    allMinNegLogLikelihoods_byParticipant(thisSample, thisParticipant) = minNegLogLikelihoodByParticipant;
                    allNTrials_byParticipant(thisSample, thisParticipant) = numel(theseErrorsByParticipant);
                    
                end
                
                thisAccuracyByParticipant = sum(theseErrorsByParticipant==0)/numel(theseErrorsByParticipant);
                allAccuracy_byParticipant(thisSample,thisParticipant) = thisAccuracyByParticipant;

            end

        end

    end

end

% Write the data to *.csv files for analysis in JASP
cd([usePath 'modelOutput/CSV/']);

% Accuracy
writeFile = fopen('TGRSVP_Exp2_AccuracyNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants(thisSample)
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allAccuracy_byParticipant(thisSample,thisParticipant));
            end
        end
    end
end

% Efficacy
writeFile = fopen('TGRSVP_Exp2_EfficacyNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants(thisSample)
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,1));
            end
        end
    end
end

% Latency
writeFile = fopen('TGRSVP_Exp2_LatencyNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants(thisSample)
        fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,2));
            end
        end
    end
end

% Precision
writeFile = fopen('TGRSVP_Exp2_PrecisionNorm.csv','w');  % Overwrite file
fprintf(writeFile,'Group,SingleLeft,SingleRight,DualLeft,DualRight'); % Header

for thisSample = 1:nSamples
    for thisParticipant = 1:nParticipants(thisSample)
        %fprintf(writeFile,'\n%d',thisSample); % Group
        for thisCondition = 1:nConditions
            for thisStream = 1:nStreams
                fprintf(writeFile,',%.4f', allEstimates_byParticipant(thisSample,thisParticipant,3));
            end
        end
    end
end

cd('../Likelihood/') %Save the likelihood information for model comparison

save('normalModelLikelihoodByParticipant', 'allMinNegLogLikelihoods_byParticipant', 'allNTrials_byParticipant')
save('normalModelLikelihoodCombined', 'allMinNegLogLikelihoods_Combined', 'allNTrials_Combined')