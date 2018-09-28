clear all;

% Add directories
usePath = '~/gitCode/nStream/';
likelihoodDirectory = 'modelOutput/Likelihood/';
addpath(usePath)

% Task parameters
sampleNames = {'twoStreams', 'eightStreams'};
modelNames = {'truncNormal','normal'};

nSamples = numel(sampleNames);
nParticipants = [10 10];
nModels = numel(modelNames);
nParams = 3;

% allBIC(:,:,1) = logNormal BIC
% allBIC(:,:,2) = normal BIC
allBICsByParticipant = NaN(nSamples, max(nParticipants), nModels);
allBICsCombined = NaN(nSamples, nModels);


conditionNames ={'twoStreams', 'eightStreams'}; %For writing into CSVs
participants = {'AJ7', 'AN14', 'BB6', 'IK4', 'JA8', 'LH9', 'LS2', 'LT5', 'RN12', 'YZ15'};



cd([usePath likelihoodDirectory])

for thisModel = 1:nModels
    %By-condition analyses
    %fprintf('model: %s combined\n',modelNames{thisModel})
    load([modelNames{thisModel} 'ModelLikelihoodCombined'])
    allNTrials_Combined
    for thisSample = 1:nSamples
        fprintf('This sample: %d. This model %d\n', thisSample, thisModel)
        [thisAICCombined thisBICCombined] = aicbic(-allMinNegLogLikelihoods_Combined(thisSample),nParams, allNTrials_Combined(thisSample));
        allBICsCombined(thisSample, thisModel) = thisBICCombined;
    end
    %by-participant analyses
    fprintf('model: %s by participant\n', modelNames{thisModel})
    load([modelNames{thisModel} 'ModelLikelihoodByParticipant'])
    
    
    
    for thisSample = 1:nSamples
       thisNParticipants = nParticipants(thisSample)
       for thisParticipant = 1:thisNParticipants
           [thisAICByParticipant thisBICByParticipant] = aicbic(-allMinNegLogLikelihoods_byParticipant(thisSample, thisParticipant),nParams, allNTrials_byParticipant(thisSample, thisParticipant))
           allBICsByParticipant(thisSample, thisParticipant, thisModel) = thisBICByParticipant;
       end
    end
end

deltaBICByParticipant = allBICsByParticipant(:,:,1) - allBICsByParticipant(:,:,2);
BFByParticipant = exp(deltaBICByParticipant./2);

deltaBICCombined = allBICsCombined(:,1) - allBICsCombined(:,2);
BFCombined = exp(deltaBICCombined./2);

csvFile = fopen('../BFCombined.csv','w');
fprintf(csvFile, 'twoStreams, eightStreams, End6Strm82msSOA,  Ex6Strm82msSOA \n');
dlmwrite('../BFCombined.csv', BFCombined', '-append', 'precision','%i');

writeFile = fopen('../BF_ByParticipant.csv','w');  % Overwrite file
fprintf(writeFile,'Participant,Group,BF'); % Header

for thisSample = 1:nSamples
  for thisParticipant = 1:nParticipants(thisSample)
      fprintf(writeFile,'\n%s,%s',participants{thisParticipant},conditionNames{thisSample}); % Group
      fprintf(writeFile,',%e', BFByParticipant(thisSample, thisParticipant));
  end
end