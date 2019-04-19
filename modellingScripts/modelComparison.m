clear all;

% Add directories
usePath = '~/gitCode/nStream/';
likelihoodDirectory = 'modelOutput/Likelihood/';
addpath(usePath)

% Task parameters
sampleNames = {'6_per_sec', '8_per_sec' '12_per_sec' '24_per_sec'};
conditionNames ={'6_per_sec', '8_per_sec' '12_per_sec' '24_per_sec'}; %For writing into CSVs
itemRates = [6 8 12 24];

nSamples = numel(sampleNames);
nParticipants = [6 6 6 6];

nStreams = 2;

modelNames = {'normal', 'truncNormal'};
nModels = numel(modelNames);
nParams = 3;

% allBIC(:,:,1) = logNormal BIC
% allBIC(:,:,2) = normal BIC
allBICsByParticipant = NaN(nSamples, nStreams, max(nParticipants), nModels);
allBICsCombined = NaN(nSamples, nModels);


participants = {'AH','CW','EN','FJ','PG','SM'};




cd([usePath likelihoodDirectory])

for thisModel = 1:nModels
    %By-condition analyses
    %fprintf('model: %s combined\n',modelNames{thisModel})
%     load([modelNames{thisModel} 'ModelLikelihoodCombined'])
%     allNTrials_Combined
%     for thisSample = 1:nSamples
%         fprintf('This sample: %d. This model %d\n', thisSample, thisModel)
%         [thisAICCombined thisBICCombined] = aicbic(-allMinNegLogLikelihoods_Combined(thisSample),nParams, allNTrials_Combined(thisSample));
%         allBICsCombined(thisSample, thisModel) = thisBICCombined;
%     end
    %by-participant analyses
    fprintf('model: %s by participant\n', modelNames{thisModel})
    load([modelNames{thisModel} 'ModelLikelihoodByParticipant'])
    
    
    
    for thisSample = 1:nSamples
       thisNParticipants = nParticipants(thisSample);
       for thisParticipant = 1:thisNParticipants
           for thisStream = 1:nStreams
                [thisAICByParticipant thisBICByParticipant] = aicbic(-allMinNegLogLikelihoods_byParticipant(thisSample, thisStream, thisParticipant),nParams, allNTrials_byParticipant(thisSample, thisStream, thisParticipant));
                allBICsByParticipant(thisSample, thisStream, thisParticipant, thisModel) = thisBICByParticipant;
           end
       end
    end
end

deltaBICByParticipant = allBICsByParticipant(:,:,:,2) - allBICsByParticipant(:,:,:,1);
BFByParticipant = exp(deltaBICByParticipant./2);

% deltaBICCombined = allBICsCombined(:,1) - allBICsCombined(:,2);
% BFCombined = exp(deltaBICCombined./2);

% csvFile = fopen('../BFCombined.csv','w');
% fprintf(csvFile, 'twoStreams, eightStreams, End6Strm82msSOA,  Ex6Strm82msSOA \n');
% dlmwrite('../BFCombined.csv', BFCombined', '-append', 'precision','%i');

writeFile = fopen('../BF_ByParticipant.csv','w');  % Overwrite file
fprintf(writeFile,'Participant,Group, Stream, BF'); % Header

for thisSample = 1:nSamples
    for thisStream = 1:nStreams
      for thisParticipant = 1:nParticipants(thisSample)
          fprintf(writeFile,'\n%s,%s,%d',participants{thisParticipant},conditionNames{thisSample}, thisStream); % Group
          fprintf(writeFile,',%e', BFByParticipant(thisSample, thisParticipant));
      end
    end
end