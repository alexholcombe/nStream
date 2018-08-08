%What does applying floor() to samples from a truncated normal do to the
%parameter estimates?
clear all;
addpath('~/gitCode/nStream/modellingScripts');
truncNormPDF = @truncNormPDFXminZero;
nReps = 200;

norm = makedist('Normal', 'mu',1, 'sigma',2);
truncNorm = truncate(norm, -0.1, Inf);

datFloor = NaN(1,2000);
dat = NaN(1,2000);

for i = 1:2000
    thisObs = random(truncNorm);
    datFloor(i) = round(thisObs);
    dat(i) = thisObs;
end

minNegLogLikelihood = Inf;
for rep = 1:nReps
    [phat, phat_ci]  = mle(datFloor , 'pdf', truncNormPDF,'start', [mean(datFloor), std(datFloor)]);
    thisNegLogLikelihood = -sum(log(truncNormPDF(datFloor, phat(1),phat(2))));
    if thisNegLogLikelihood < minNegLogLikelihood
        bestEstimatesFloor = phat;
        minNegLogLikelihood = thisNegLogLikelihood;
    end
end

minNegLogLikelihood = Inf;
for rep = 1:nReps
    [phat, phat_ci]  = mle(dat , 'pdf', truncNormPDF,'start', [0.1+rand*2, 0.3+rand*2], 'lower', [0.1,0.3]);
    thisNegLogLikelihood = -sum(log(truncNormPDF(dat, phat(1),phat(2))));
    if thisNegLogLikelihood < minNegLogLikelihood
        bestEstimates = phat;
        minNegLogLikelihood = thisNegLogLikelihood;
    end
end