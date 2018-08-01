%Sample from uniform and lognormal and compare the estimated parameters
%from the mixture model fitting code
clear all;

addpath('~/gitcode/nStream/modellingScripts/')
addpath('~/gitcode/nStream/modellingScripts/Model recovery')


allcombs = @allcomb;


% Model fitting parameters

nFreeParameters = 3;
pdf_normmixture = @TGAB_pdf_logNorm_Mixture_Single; % We can use the single-episode AB model
pdf_global = @TGAB_pdf_logNorm_Mixture_Single_global_returns;
pdf_uniformonly = @TG_pdf_Uniform;
nReplicates = 200;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^5;
fitMaxFunEvals = 10^5;

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');


global xDomain;
global pseudo_uniform;

%ground truth parameters
rate = 1000./12;

efficacies = [0 .3 .5 .7];

latencies = [0 1 2];

precisions = [.5 1 3/2];

shifts = [0 1];

participants = 1:10;

trialN = 200; %number of simulated trials for each combination of efficacy latency and precision

cuePos = 15; %No point in varying the cuePos I guess?
nLetters = 22;
%minimum and maximum possible errors
minSPE = -cuePos+1; 
maxSPE = nLetters - cuePos; %24 items in a stream
xDomain = minSPE:maxSPE;

pseudo_uniform = zeros(size(xDomain));
pseudo_uniform((1-cuePos-minSPE+1):(nLetters-cuePos-minSPE+1)) = pseudo_uniform((1-cuePos-minSPE+1):(nLetters-cuePos-minSPE+1))+ones(1,nLetters);


estimatesAndTruth = allcombs(participants, efficacies, latencies, precisions, shifts);

nRows = size(estimatesAndTruth);
nRows = nRows(1);
estimatesAndTruth = [estimatesAndTruth repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1)]; % columns are :groundtruth efficacy, groundtruth latency, groundtruth precision, estimated efficacy, estimated latency, estimated precision
estimatesAndTruthLowerBounds = [estimatesAndTruth repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1)];
estimatesAndTruthUpperBounds = [estimatesAndTruth repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1)];

simulatedData = nan(nRows, 200);

for shift = shifts
    for participant = participants
        for efficacy = efficacies
            for latency = latencies
                for precision = precisions
                    fprintf('participant = %d, efficacy = %d, latency = %d, precision = %d\n', participant, efficacy, latency, precision)
                    theseErrors = NaN(trialN,1);
                    thisRow = find(estimatesAndTruth(:,1) == participant & estimatesAndTruth(:,2)==efficacy & estimatesAndTruth(:,3)==latency & estimatesAndTruth(:,4)==precision & estimatesAndTruth(:,5)==shift);
                    for trial = 1:trialN
                        nonGuess = binornd(1, efficacy);
                        if nonGuess
                            thisObs = lognrnd(latency, precision,1,1);
                            thisObs = floor(thisObs); %Take the floor because if a stream is selected/bound at any point in between two items' onsets the data will be rounded to the last onset
                            while thisObs < minSPE || thisObs > maxSPE
                                thisObs = lognrnd(latency, precision,1,1);
                                thisObs = floor(thisObs);
                            end
                            theseErrors(trial) = thisObs-shift;
                        else %if not nonguess
                            thisObs = (maxSPE-minSPE+1).*rand(1,1)+minSPE;
                            thisObs = floor(thisObs);
                            while thisObs < minSPE || thisObs > maxSPE
                                thisObs = (maxSPE-minSPE+1).*rand(1,1)+minSPE;
                                thisObs = floor(thisObs);
                            end
                            theseErrors(trial) = thisObs;
                        end
                    end
                    simulatedData(thisRow,:) = theseErrors;

                        minNegLogLikelihoodCombined = inf;
                        theseErrors = squeeze(theseErrors);
                        theseErrors = theseErrors(:);
                        theseErrors = theseErrors(~isnan(theseErrors));

                         % Compute negative log likelihood for uniform distribution

                        uniformNegLogLikelihoodCombined = -sum(log(pdf_uniformonly(theseErrors,1)));

                        warning('off', 'stats:mlecov:NonPosDefHessian');

                        for thisReplicate = 1:nReplicates
                            %fprintf('new rep\n\r')
                            pGuess = rand;
                            muGuess = abs(((3*rand)-1.5));
                            sigmaGuess = rand;
                            while sigmaGuess<=0
                                sigmaGuess = rand;
                            end
                            parameterGuess = [pGuess muGuess sigmaGuess];
                            parameterLowerBound = [0 -1.1 10^-8]; %might have to replace the mu bound with small nonzero number. Not clear if bounds are open or not. Sigma cannot be 1 or less
                            parameterUpperBound = [1 5 5];

                            [currentEstimatesCombined, currentCIsCombined] = mle(theseErrors, 'pdf', pdf_normmixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);

                            %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
                            % Compute negative log likelihood
                            thisNegLogLikelihoodCombined = -sum(log(pdf_normmixture(theseErrors,currentEstimatesCombined(1),currentEstimatesCombined(2),currentEstimatesCombined(3))));

                            if minNegLogLikelihoodCombined > thisNegLogLikelihoodCombined
                                minNegLogLikelihoodCombined = thisNegLogLikelihoodCombined;
                                bestEstimatesCombined = currentEstimatesCombined;
                                bestEstimateCIsCombined = currentCIsCombined;
                            end

                        end


                        % Test for a significant difference in log likelihoods
                        [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihoodCombined,-uniformNegLogLikelihoodCombined,nFreeParameters,pCrit);

                        if h==0

                            % Null model not rejected; use uniform only
                            estimatesAndTruth(thisRow,6:8) = [0 NaN NaN];
                            estimatesAndTruthLowerBounds(thisRow,6:8) = [0 NaN NaN];
                            estimatesAndTruthUpperBounds(thisRow,6:8) = [0 NaN NaN];

                        else

                            % Use mixture

                            estimatesAndTruth(thisRow,6:8) = bestEstimatesCombined.*[1 rate rate];
                            estimatesAndTruthLowerBounds(thisRow,6:8) = bestEstimateCIsCombined(1,:);
                            estimatesAndTruthUpperBounds(thisRow,6:8) = bestEstimateCIsCombined(2,:);
                        end
                        estimatesAndTruth(thisRow,6:8)
                end
            end
        end
    end
end


EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/' 'estimatesAndTruth.csv'];
csvwrite(EandTfname, estimatesAndTruth)
EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/' 'estimatesAndTruthLowerBound.csv'];
csvwrite(EandTfname, estimatesAndTruthLowerBounds)
EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/' 'estimatesAndTruthUpperBound.csv'];
csvwrite(EandTfname, estimatesAndTruthUpperBounds)


SimDatafname = ['~/gitcode/nStream/modellingScripts/Model recovery/' 'SimulatedData.csv'];
csvwrite(SimDatafname, simulatedData)
