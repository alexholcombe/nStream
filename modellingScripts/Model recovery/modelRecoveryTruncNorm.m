%Sample from uniform and lognormal and compare the estimated parameters
%from the mixture model fitting code
clear all;

addpath('~/gitcode/nStream/modellingScripts/')
addpath('~/gitcode/nStream/modellingScripts/Model recovery')

truncNormInvCDF = @truncated_normal_a_cdf_inv; %for inverse transform sampling
allcombs = @allcomb;

thisModelName = 'TruncNorm';


% Model fitting parameters

nFreeParameters = 3;
pdf_truncNormMixture = @TruncNormMixtureOneSDPDF; % We can use the single-episode AB model
pdf_normMixture = @NormalMixturePDF;
pdf_uniformonly = @TG_pdf_Uniform;
nReplicates = 100;
pCrit = .05;
smallNonZeroNumber = 10^-10;
fitMaxIter = 10^5;
fitMaxFunEvals = 10^5;

a = 0; %lower bound on truncated norm

% Set options
options = statset('MaxIter', fitMaxIter, 'MaxFunEvals', fitMaxFunEvals, 'Display', 'off');


global xDomain;
global pseudo_uniform;

%ground truth parameters
rate = 1000./12;

efficacies = [0 .5 1];

latencies = [1 1.5];

precisions = [70 90]./rate;

participants = 1:10;

trialN = 200; %number of simulated trials for each combination of efficacy latency and precision

cuePos = 10; %No point in varying the cuePos I guess?
nLetters = 22;
%minimum and maximum possible errors
minSPE = -cuePos+1; 
maxSPE = nLetters - cuePos; %24 items in a stream
xDomain = minSPE:maxSPE; 
pseudo_uniform = zeros(size(xDomain));
pseudo_uniform((1-cuePos-minSPE+1):(nLetters-cuePos-minSPE+1)) = pseudo_uniform((1-cuePos-minSPE+1):(nLetters-cuePos-minSPE+1))+ones(1,nLetters);


estimatesAndTruth = allcombs(participants, efficacies, latencies, precisions);

nRows = size(estimatesAndTruth);
nRows = nRows(1);
estimatesAndTruth = [estimatesAndTruth repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1) repmat(999, nRows,1)]; % columns are :groundtruth efficacy, groundtruth latency, groundtruth precision, estimated efficacy, estimated latency, estimated precision
estimatesAndTruthLowerBounds = estimatesAndTruth;
estimatesAndTruthUpperBounds = estimatesAndTruth;

simulatedData = nan(nRows, 200);


i = 1;
for participant = participants
    for efficacy = efficacies
        for latency = latencies
            for precision = precisions
                fprintf('participant = %d, efficacy = %d, latency = %d, precision = %d\n', participant, efficacy, latency, precision)
                theseErrors = NaN(trialN,1);
                thisRow = find(estimatesAndTruth(:,1) == participant & estimatesAndTruth(:,2)==efficacy & estimatesAndTruth(:,3)==latency & estimatesAndTruth(:,4)==precision);
                norm = makedist('Normal', 'mu',latency, 'sigma',precision);
                truncNorm = truncate(norm, 0, Inf);
                for trial = 1:trialN
                    nonGuess = binornd(1, efficacy);
                    if nonGuess
                        thisObs = random(truncNorm);
                        thisObs = round(thisObs); %Take the floor because if a stream is selected/bound at any point in between two items' onsets the data will be rounded to the last onset
                        while thisObs < minSPE || thisObs > maxSPE
                            thisObs = random(truncNorm);
                            thisObs = round(thisObs);
                        end
                        theseErrors(trial) = thisObs;
                    else %if not nonguess
                        thisObs = (maxSPE-minSPE+1).*rand(1,1)+minSPE;
                        thisObs = round(thisObs);
                        while thisObs < minSPE || thisObs > maxSPE
                            thisObs = (maxSPE-minSPE+1).*rand(1,1)+minSPE;
                            thisObs = round(thisObs);
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
                     
                        parameterLowerBound = [0 -1 .05]; %might have to replace the mu bound with small nonzero number. Not clear if bounds are open or not. Sigma cannot be 1 or less
                        parameterUpperBound = [1 5 5];
                        
                        pGuess = rand;
                        
                        muGuess = parameterUpperBound(2) + (parameterLowerBound(2) - parameterUpperBound(2)) * rand(); %Sample starting value from uniform in the interval defined by the bounds for that value
                        while muGuess < parameterLowerBound(2) || muGuess > parameterUpperBound(2)
                            muGuess = parameterUpperBound(2) + (parameterLowerBound(2) - parameterUpperBound(2)) * rand();
                        end

                        sigmaGuess = parameterUpperBound(3) + (parameterLowerBound(3) - parameterUpperBound(3)) * rand();
                        while sigmaGuess < parameterLowerBound(3) || sigmaGuess > parameterUpperBound(3)
                            sigmaGuess = parameterUpperBound(3) + (parameterLowerBound(3) - parameterUpperBound(3)) * rand();;
                        end
                        parameterGuess = [pGuess muGuess sigmaGuess];

                        [currentEstimatesCombined, currentCIsCombined] = mle(theseErrors, 'pdf', pdf_truncNormMixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);

                        %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
                        % Compute negative log likelihood
                        thisNegLogLikelihoodCombined = -sum(log(pdf_truncNormMixture(theseErrors,currentEstimatesCombined(1),currentEstimatesCombined(2),currentEstimatesCombined(3))));

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
                        estimatesAndTruth(thisRow,5:7) = [0 NaN NaN];
                        estimatesAndTruthLowerBounds(thisRow,5:7) = [0 NaN NaN];
                        estimatesAndTruthUpperBounds(thisRow,5:7) = [0 NaN NaN];

                    else

                        % Use mixture

                        estimatesAndTruth(thisRow,5:7) = bestEstimatesCombined %.*[1 rate rate];
                        estimatesAndTruthLowerBounds(thisRow,5:7) = bestEstimateCIsCombined(1,:);
                        estimatesAndTruthUpperBounds(thisRow,5:7) = bestEstimateCIsCombined(2,:);
                    end

                    minNegLogLikelihoodCombined = inf;
                    theseErrors = squeeze(theseErrors);
                    theseErrors = theseErrors(:);
                    theseErrors = theseErrors(~isnan(theseErrors));

                     % Compute negative log likelihood for uniform distribution

                    uniformNegLogLikelihoodCombined = -sum(log(pdf_uniformonly(theseErrors,1)));

                    warning('off', 'stats:mlecov:NonPosDefHessian');

                    for thisReplicate = 1:nReplicates
                        %fprintf('new rep\n\r')
                        
                        parameterLowerBound = [0 -4 .05]; %might have to replace the mu bound with small nonzero number. Not clear if bounds are open or not. Sigma cannot be 1 or less
                        parameterUpperBound = [1 5 5];
                        
                        pGuess = rand;
                        
                        muGuess = parameterUpperBound(2) + (parameterLowerBound(2) - parameterUpperBound(2)) * rand(); %Sample starting value from uniform in the interval defined by the bounds for that value
                        while muGuess < parameterLowerBound(2) || muGuess > parameterUpperBound(2)
                            muGuess = parameterUpperBound(2) + (parameterLowerBound(2) - parameterUpperBound(2)) * rand();
                        end

                        sigmaGuess = parameterUpperBound(3) + (parameterLowerBound(3) - parameterUpperBound(3)) * rand();
                        while sigmaGuess < parameterLowerBound(3) || sigmaGuess > parameterUpperBound(3)
                            sigmaGuess = parameterUpperBound(3) + (parameterLowerBound(3) - parameterUpperBound(3)) * rand();;
                        end
                        parameterGuess = [pGuess muGuess sigmaGuess];
                        
                        [currentEstimatesCombined, currentCIsCombined] = mle(theseErrors, 'pdf', pdf_normMixture, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options);

                        %[result pseudo_normal normFactor_uniform normFactor_normal uniResultTemp normResultTemp normResult uniResult] = pdf_global(theseErrors, parameterGuess);
                        % Compute negative log likelihood
                        thisNegLogLikelihoodCombined = -sum(log(pdf_normMixture(theseErrors,currentEstimatesCombined(1),currentEstimatesCombined(2),currentEstimatesCombined(3))));

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
                        estimatesAndTruth(thisRow,8:10) = [0 NaN NaN];
                        estimatesAndTruthLowerBounds(thisRow,8:10) = [0 NaN NaN];
                        estimatesAndTruthUpperBounds(thisRow,8:10) = [0 NaN NaN];

                    else

                        % Use mixture

                        estimatesAndTruth(thisRow,8:10) = bestEstimatesCombined %.*[1 rate rate];
                        estimatesAndTruthLowerBounds(thisRow,8:10) = bestEstimateCIsCombined(1,:);
                        estimatesAndTruthUpperBounds(thisRow,8:10) = bestEstimateCIsCombined(2,:);
                    end
                    estimatesAndTruth(thisRow,5:7)
                    [heights, locations] = hist(theseErrors);
                    width = locations(2)-locations(1);
                    heights = heights / (trialN*width);
                    bar(locations,heights,'hist')
                    grid = linspace(min(theseErrors),max(theseErrors));
                    xPos = max(theseErrors)-.25*(max(theseErrors)-min(theseErrors));
                    if h==0
                        text(xPos, max(heights)*.8, 'Efficacy = 0')
                        line(grid, pdf_uniformonly(grid,1))
                    else
                        text(xPos, max(heights)*.8, strcat('Efficacy = ',num2str(bestEstimatesCombined(1))));
                        text(xPos, max(heights)*.7, strcat('Latency = ',num2str(bestEstimatesCombined(2))));
                        text(xPos, max(heights)*.6, strcat('Precision = ',num2str(bestEstimatesCombined(3))));
                        line(grid, pdf_truncNormMixture(grid, bestEstimatesCombined(1), bestEstimatesCombined(2), bestEstimatesCombined(3)));
                    end
                    plotFileName = strcat('plot', num2str(i),'.png');
                    saveas(gcf,['~/gitcode/nStream/modellingScripts/Model recovery/Plots/' thisModelName '/' plotFileName]);
                    i = i + 1;
            end
        end
    end
end


EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/' thisModelName '/estimatesAndTruth' thisModelName '.csv'];
csvwrite(EandTfname, estimatesAndTruth)
EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/' thisModelName '/estimatesAndTruthLowerBound' thisModelName '.csv'];
csvwrite(EandTfname, estimatesAndTruthLowerBounds)
EandTfname = ['~/gitcode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/' thisModelName '/estimatesAndTruthUpperBound' thisModelName '.csv'];
csvwrite(EandTfname, estimatesAndTruthUpperBounds)


SimDatafname = ['~/gitcode/nStream/modellingScripts/Model recovery/SimulatedData/' thisModelName '/SimulatedData' thisModelName '.csv'];
csvwrite(SimDatafname, simulatedData)
