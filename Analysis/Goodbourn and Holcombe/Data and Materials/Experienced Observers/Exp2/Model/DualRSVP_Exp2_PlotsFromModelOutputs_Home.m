clear all;
close all;

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/ptg/Dropbox/MATLAB/'));
cd('/Users/ptg/Dropbox/MATLAB/DualRSVP/Exp2/Model/');

% Image settings

thisColormap = gray(2^8);
thisColormapReverse = flipud(thisColormap);

errorColor = [0 0 0];
targetColor = [1 1 1];
neitherColor = [.5 .5 .5];
modelLine = 'r-';
dashedLine = 'r:';

modelParamIcon_S1 = 'ro';
modelParamIcon_S2 = 'bv';
modelLineColor = [1 0 0; 0 0 1];

% Load data files

load('ModelOutputs.mat');

% Other settings

bootstrapCIs = 0;

asciiArray = [65:77 79:85 87:90];
letterLabels = cell(1,nLetters);
itemRates = [12 12 12 12];
conditionLabels = {'D-D','D-SU','D-SC','S-C'};

itemDurations = 1000./itemRates;

for thisAscii = 1:nLetters
    letterLabels{thisAscii} = char(asciiArray(thisAscii));
end

modelGridPoints = 100;
modelGrid = linspace(-maxError,maxError,modelGridPoints);

errorDistributionAxes = [-(maxError+3) (maxError+3) -0.05 1.05];
categorisedDistributionAxes = [-(maxError+3) (maxError+3) -0.05 1.05];

modelParameterAxes = [0.5 4.5 -0.1 1.1;
                      0.5 4.5 -1.2 1.2;
                      0.5 4.5 -0.2 2.2];
                  
modelParameterAxes_byTime = [0.5 4.5 -0.1 1.1;
                             0.5 4.5 -180 180;
                             0.5 4.5 -30 330];
                         
targetCorrelogramValues = -5:5;
targetCorrelogramValues_byTime = targetCorrelogramValues*itemDurations(1);

positionCorrelogramsAxes = 6*[-1 1 -1 1];
positionCorrelogramsAxes_byTime = 550*[-1 1 -1 1];

positionCorrelogramsAxisTicks = -500:250:500;

contingentModelJitter = [0 -.25 .25]; % Plot x-jitter for standard, target contingent and error contingent models respectively
contingentTargetColor = [0 1 0];
contingentErrorColor = [1 0 0];
contingentModelIcon_S1 = {'ko','go','ro'};
contingentModelIcon_S2 = {'kv','gv','rv'};


% Plot correlogram of responses vs target letters (confusion matrices)

% Axis Labels:  x, 'Target'; y, 'Response';
% Row Labels: (Participants) 'Combined', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Item Rates) '6 i/s', '8 i/s', '12 i/s', '24 i/s';

confusionMatrices = figure('color','white','name','Confusion Matrices');
hold on;

thisPlot = 1;
subplot((nParticipants+1),(nConditions+1),thisPlot);
thisTable = crosstab(reshape(compiledResponses,1,nSessions*nTrials*nStreams*nParticipants*nConditions),reshape(compiledTargets,1,nSessions*nTrials*nStreams*nParticipants*nConditions));
imagesc(thisTable);
colormap(thisColormapReverse);
axis square
set(gca,'TickDir','out','XTick', 1:nLetters,'YTick', 1:nLetters, 'XTickLabel', letterLabels, 'YTickLabel', letterLabels, 'TickLength', [0.025 0.05],'FontSize', 6);


for thisCondition = 1:nConditions

    thisPlot = thisPlot+1;
    subplot((nParticipants+1),(nConditions+1),thisPlot);
    thisTable = crosstab(reshape(compiledResponses(thisCondition,:,:,:,:),1,nSessions*nTrials*nStreams*nParticipants),reshape(compiledTargets(thisCondition,:,:,:,:),1,nSessions*nTrials*nStreams*nParticipants));
    imagesc(thisTable);
    colormap(thisColormapReverse);
    axis square
    set(gca,'TickDir','out','XTick', 1:nLetters,'YTick', 1:nLetters, 'XTickLabel', letterLabels, 'YTickLabel', letterLabels, 'TickLength', [0.025 0.05],'FontSize', 6);

end

for thisParticipant = 1:nParticipants

    % Master
    thisPlot = (thisParticipant*(nConditions+1))+1;
    subplot((nParticipants+1),(nConditions+1),thisPlot);
    thisTable = crosstab(reshape(compiledResponses(:,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams*nConditions),reshape(compiledTargets(:,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams*nConditions));
    imagesc(thisTable);
    colormap(thisColormapReverse);
    axis square
    set(gca,'TickDir','out','XTick', 1:nLetters,'YTick', 1:nLetters, 'XTickLabel', letterLabels, 'YTickLabel', letterLabels, 'TickLength', [0.025 0.05],'FontSize', 6);

    % By condition

    for thisCondition = 1:nConditions

        thisPlot = thisPlot+1;
        subplot((nParticipants+1),(nConditions+1),thisPlot);
        thisTable = crosstab(reshape(compiledResponses(thisCondition,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams),reshape(compiledTargets(thisCondition,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams));
        imagesc(thisTable);
        colormap(thisColormapReverse);
        axis square
        set(gca,'TickDir','out','XTick', 1:nLetters,'YTick', 1:nLetters, 'XTickLabel', letterLabels, 'YTickLabel', letterLabels, 'TickLength', [0.025 0.05],'FontSize', 6);

    end

end
        


% Plot histogram of error responses (response biases)

% Axis Labels: x, 'Letter', y, 'Probability';
% Row Labels: (Participants) 'Combined', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Item Rates) '6 i/s', '8 i/s', '12 i/s', '24 i/s';

responseHistograms = figure('color','white','name','Response Histograms');
rhAxis = [0 25 0 0.25];
hold on;

chanceLevel = 1/nLetters;

thisPlot = 1;
subplot((nParticipants+1),(nConditions+1),thisPlot);
thisData = reshape(compiledResponses,1,nSessions*nTrials*nStreams*nParticipants*nConditions);
thisError = reshape(trialIsError_Combined,1,nSessions*nTrials*nStreams*nParticipants*nConditions);
thisError = thisData(thisError==1);
thisDataHist = histc(thisData,1:nLetters);
totalData = sum(thisDataHist);

thisDataHist = thisDataHist/totalData;
thisErrorHist = histc(thisError,1:nLetters)/totalData;
thisOtherHist = thisDataHist-thisErrorHist;

plot([0 nLetters+1],[chanceLevel chanceLevel],dashedLine);
hold on;
barHandles = bar(1:nLetters,[thisOtherHist;thisErrorHist]','stacked');
set(barHandles(1),'FaceColor',targetColor);
set(barHandles(2),'FaceColor',errorColor);
axis square;
axis(rhAxis);
set(gca,'TickDir','out','XTick', 1:nLetters,'XTickLabel', letterLabels, 'YMinorTick', 'on', 'TickLength', [0.025 0.05],'FontSize', 6);


for thisCondition = 1:nConditions

    thisPlot = thisPlot+1;
    subplot((nParticipants+1),(nConditions+1),thisPlot);

    thisData = reshape(compiledResponses(thisCondition,:,:,:,:),1,nSessions*nTrials*nStreams*nParticipants);
    thisError = reshape(trialIsError_Combined(thisCondition,:,:,:,:),1,nSessions*nTrials*nStreams*nParticipants);
    thisError = thisData(thisError==1);
    thisDataHist = histc(thisData,1:nLetters);
    totalData = sum(thisDataHist);

    thisDataHist = thisDataHist/totalData;
    thisErrorHist = histc(thisError,1:nLetters)/totalData;
    thisOtherHist = thisDataHist-thisErrorHist;

    plot([0 nLetters+1],[chanceLevel chanceLevel],dashedLine);
    hold on;
    barHandles = bar(1:nLetters,[thisOtherHist;thisErrorHist]','stacked');
    set(barHandles(1),'FaceColor',targetColor);
    set(barHandles(2),'FaceColor',errorColor);
    axis square;
    axis(rhAxis);
    set(gca,'TickDir','out','XTick', 1:nLetters,'XTickLabel', letterLabels, 'YMinorTick', 'on', 'TickLength', [0.025 0.05],'FontSize', 6);

end

for thisParticipant = 1:nParticipants

    % Master
    thisPlot = (thisParticipant*(nConditions+1))+1;
    subplot((nParticipants+1),(nConditions+1),thisPlot);

    thisData = reshape(compiledResponses(:,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams*nConditions);
    thisError = reshape(trialIsError_Combined(:,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams*nConditions);
    thisError = thisData(thisError==1);
    thisDataHist = histc(thisData,1:nLetters);
    totalData = sum(thisDataHist);

    thisDataHist = thisDataHist/totalData;
    thisErrorHist = histc(thisError,1:nLetters)/totalData;
    thisOtherHist = thisDataHist-thisErrorHist;

    plot([0 nLetters+1],[chanceLevel chanceLevel],dashedLine);
    hold on;
    barHandles = bar(1:nLetters,[thisOtherHist;thisErrorHist]','stacked');
    set(barHandles(1),'FaceColor',targetColor);
    set(barHandles(2),'FaceColor',errorColor);
    axis square;
    axis(rhAxis);
    set(gca,'TickDir','out','XTick', 1:nLetters,'XTickLabel', letterLabels, 'YMinorTick', 'on', 'TickLength', [0.025 0.05],'FontSize', 6);
    

    % By condition

    for thisCondition = 1:nConditions

        thisPlot = thisPlot+1;
        subplot((nParticipants+1),(nConditions+1),thisPlot);
        
        thisData = reshape(compiledResponses(thisCondition,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams);
        thisError = reshape(trialIsError_Combined(thisCondition,thisParticipant,:,:,:),1,nSessions*nTrials*nStreams);
        thisError = thisData(thisError==1);
        thisDataHist = histc(thisData,1:nLetters);
        totalData = sum(thisDataHist);

        thisDataHist = thisDataHist/totalData;
        thisErrorHist = histc(thisError,1:nLetters)/totalData;
        thisOtherHist = thisDataHist-thisErrorHist;

        plot([0 nLetters+1],[chanceLevel chanceLevel],dashedLine);
        hold on;
        barHandles = bar(1:nLetters,[thisOtherHist;thisErrorHist]','stacked');
        set(barHandles(1),'FaceColor',targetColor);
        set(barHandles(2),'FaceColor',errorColor);
        axis square;
        axis(rhAxis);
        set(gca,'TickDir','out','XTick', 1:nLetters,'XTickLabel', letterLabels, 'YMinorTick', 'on', 'TickLength', [0.025 0.05],'FontSize', 6);

    end

end
    
    
% Plot mixture models with raw data

    % Combined
    
    errorDistributions_Combined = figure('color','white','name','Error Distributions (Combined)');
    thisPlot = 0;
    
    for thisCondition = 1:nConditions
        
        for thisStream = 1:nStreams
            
            thisData = reshape(compiledErrors(thisCondition,:,:,:,thisStream),1,nSessions*nTrials*nParticipants);
            thisDataHist = histc(thisData,errorValues);            
            
            if bootstrapCIs
            
                % Get CIs for data

                for thisBin = 1:numel(thisDataHist)
                    
                    thisDataHist_Proportions = NaN(size(thisDataHist));
                    lCI = NaN(size(thisDataHist));
                    uCI = NaN(size(thisDataHist));

                    theseCIs = prop_ci(thisDataHist(thisBin),sum(thisDataHist),.05);
                    thisDataHist_Proportions(thisBin) = theseCIs(1);
                    lCI(thisBin) = theseCIs(2);
                    uCI(thisBin) = theseCIs(3);

                end
                
            else
               
                % Get proportions only
                
                thisDataHist_Proportions = thisDataHist/sum(thisDataHist);
                
            end
            
            % Get model fit
            
            modelEstimates = squeeze(allEstimates_Combined(thisCondition,thisStream,:));
            
            if modelEstimates(1)==0
                
                % Uniform only
                
                modelOutput = pdf_Uniform_Exp1(modelGrid,1);
                
            else
                
                % Mixture model
                
                modelOutput = pdf_Mixture_Exp1(modelGrid,modelEstimates(1),modelEstimates(2),modelEstimates(3));
                
            end
            
            thisPlot = thisPlot+1;
            subplot(nConditions,nStreams,thisPlot);
            plot(modelGrid,modelOutput,modelLine);
            hold on;
            
            if bootstrapCIs
            
                % Plot errorbars

                for thisError = 1:nErrorValues

                    line(errorValues(thisError)*ones(1,2),[lCI(thisError) uCI(thisError)],'Color',[0 0 0]);

                end
                
            end
                        
            % Data scatterplot
            
            scatter(errorValues,thisDataHist_Proportions,'ko');
                        
            % Format axes
            
            axis(errorDistributionAxes);
            axis square;
            set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');
            
        end
        
    end
            
	% Individual
    
    errorDistributions_Individual = NaN(1,nParticipants);
    
    for thisParticipant = 1:nParticipants
        
        thisName = ['Error Distributions (' allParticipants{thisParticipant} ')'];
        errorDistributions_Individual(thisParticipant) = figure('color','white','name', thisName);
        thisPlot = 0;

        for thisCondition = 1:nConditions

            for thisStream = 1:nStreams

                thisData = reshape(compiledErrors(thisCondition,thisParticipant,:,:,thisStream),1,nSessions*nTrials);
                thisDataHist = histc(thisData,errorValues);

            if bootstrapCIs
            
                % Get CIs for data

                for thisBin = 1:numel(thisDataHist)
                    
                    thisDataHist_Proportions = NaN(size(thisDataHist));
                    lCI = NaN(size(thisDataHist));
                    uCI = NaN(size(thisDataHist));

                    theseCIs = prop_ci(thisDataHist(thisBin),sum(thisDataHist),.05);
                    thisDataHist_Proportions(thisBin) = theseCIs(1);
                    lCI(thisBin) = theseCIs(2);
                    uCI(thisBin) = theseCIs(3);

                end
                
            else
               
                % Get proportions only
                
                thisDataHist_Proportions = thisDataHist/sum(thisDataHist);
                
            end

                % Get model fit

                modelEstimates = squeeze(allEstimates_byParticipant(thisCondition,thisStream,thisParticipant,:));

                if modelEstimates(1)==0

                    % Uniform only

                    modelOutput = pdf_Uniform_Exp1(modelGrid,1);

                else

                    % Mixture model

                    modelOutput = pdf_Mixture_Exp1(modelGrid,modelEstimates(1),modelEstimates(2),modelEstimates(3));

                end

                thisPlot = thisPlot+1;
                subplot(nConditions,nStreams,thisPlot);
                plot(modelGrid,modelOutput,modelLine);
                hold on;

                % Plot errorbars
                
                if bootstrapCIs

                    for thisError = 1:nErrorValues

                        line(errorValues(thisError)*ones(1,2),[lCI(thisError) uCI(thisError)],'Color',[0 0 0]);

                    end
                    
                end

                % Data scatterplot

                scatter(errorValues,thisDataHist_Proportions,'ko');

                % Format axes

                axis(errorDistributionAxes);
                axis square;
                set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');

            end

        end 
        
    end
    
    
% Plot model parameters as a function of presentation rate

% Axis Labels:  x, 'Item Rate'; y, 'Model Parameter';
% Row Labels: (Participants) 'Combined', 'Mean', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Parameter) '(1-Suppression)', 'Delay (Items)', 'Diffusion (Items)';

modelParameters_byItem = figure('color','white','name', 'Model Parameters (By Item)');
thisPlot = 0;

% Combined

for thisParameter = 1:nFreeParameters
    
    thisPlot = thisPlot+1;
    subplot(nParticipants+2,nFreeParameters,thisPlot);
    
    for thisStream = 1:nStreams
        
        if thisStream==1
            modelParamIcon = modelParamIcon_S1;
        else
            modelParamIcon = modelParamIcon_S2;
        end
        
        for thisCondition = 1:nConditions
            
            line([thisCondition thisCondition],[allLowerBounds_Combined(thisCondition,thisStream,thisParameter) allUpperBounds_Combined(thisCondition,thisStream,thisParameter)], 'Color', [0 0 0]);
            hold on;
                        
        end
        
        scatter(1:4,allEstimates_Combined(:,thisStream,thisParameter),modelParamIcon);
        
        box on;
        axis(modelParameterAxes(thisParameter,:));
        axis square;
        set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
    end
        
end


% Mean of individual

for thisParameter = 1:nFreeParameters

    thisPlot = thisPlot+1;
    subplot(nParticipants+2,nFreeParameters,thisPlot);

    for thisStream = 1:nStreams

        if thisStream==1
            modelParamIcon = modelParamIcon_S1;
        else
            modelParamIcon = modelParamIcon_S2;
        end

        theseEstimates = allEstimates_byParticipant(:,thisStream,:,thisParameter);        
        theseMeans = nanmean(theseEstimates,3);
        theseSEMs = nanstd(theseEstimates,[],3)./sqrt(sum(~isnan(theseEstimates),3));

        theseUpperBounds = theseMeans+theseSEMs;
        theseLowerBounds = theseMeans-theseSEMs;
        
        
        for thisCondition = 1:nConditions

            line([thisCondition thisCondition],[theseLowerBounds(thisCondition) theseUpperBounds(thisCondition)], 'Color', [0 0 0]);
            hold on;

        end

        scatter(1:4,theseMeans,modelParamIcon);

        box on;
        axis(modelParameterAxes(thisParameter,:));
        axis square;
        set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
    end

end


% Individual

for thisParticipant = 1:nParticipants

    for thisParameter = 1:nFreeParameters

        thisPlot = thisPlot+1;
        subplot(nParticipants+2,nFreeParameters,thisPlot);

        for thisStream = 1:nStreams

            if thisStream==1
                modelParamIcon = modelParamIcon_S1;
            else
                modelParamIcon = modelParamIcon_S2;
            end

            for thisCondition = 1:nConditions

                line([thisCondition thisCondition],[allLowerBounds_byParticipant(thisCondition,thisStream,thisParticipant,thisParameter) allUpperBounds_byParticipant(thisCondition,thisStream,thisParticipant,thisParameter)], 'Color', [0 0 0]);
                hold on;

            end

            scatter(1:4,allEstimates_byParticipant(:,thisStream,thisParticipant,thisParameter),modelParamIcon);

            box on;
            axis(modelParameterAxes(thisParameter,:));
            axis square;
            set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
        end

    end
    
end



% Plot model parameters as a function of presentation rate

% Axis Labels:  x, 'Item Rate'; y, 'Model Parameter';
% Row Labels: (Participants) 'Combined', 'Mean', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Parameter) '(1-Suppression)', 'Delay (ms)', 'Diffusion (ms)';

modelParameters_byTime = figure('color','white','name', 'Model Parameters (By Time)');
thisPlot = 0;

% Combined

for thisParameter = 1:nFreeParameters
    
    thisPlot = thisPlot+1;
    subplot(nParticipants+2,nFreeParameters,thisPlot);
    
    for thisStream = 1:nStreams
        
        if thisStream==1
            modelParamIcon = modelParamIcon_S1;
        else
            modelParamIcon = modelParamIcon_S2;
        end
        
        for thisCondition = 1:nConditions
            
            theseLowerBounds = allLowerBounds_Combined(thisCondition,thisStream,thisParameter);
            theseUpperBounds = allUpperBounds_Combined(thisCondition,thisStream,thisParameter);
            
            if thisParameter~=1
                
                theseLowerBounds = theseLowerBounds*itemDurations(thisCondition);
                theseUpperBounds = theseUpperBounds*itemDurations(thisCondition);
                
            end
            
            line([thisCondition thisCondition],[theseLowerBounds theseUpperBounds], 'Color', squeeze(modelLineColor(thisStream,:)));
            hold on;
                        
        end
        
        theseEstimates = allEstimates_Combined(:,thisStream,thisParameter);
        
        if thisParameter~=1
            theseEstimates = theseEstimates'.*itemDurations;
        end
        
        scatter(1:4,theseEstimates,modelParamIcon);
        
        box on;
        axis(modelParameterAxes_byTime(thisParameter,:));
        axis square;
        set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
    end
        
end


% Mean of individual

for thisParameter = 1:nFreeParameters

    thisPlot = thisPlot+1;
    subplot(nParticipants+2,nFreeParameters,thisPlot);

    for thisStream = 1:nStreams

        if thisStream==1
            modelParamIcon = modelParamIcon_S1;
        else
            modelParamIcon = modelParamIcon_S2;
        end

        theseEstimates = allEstimates_byParticipant(:,thisStream,:,thisParameter);        
        theseMeans = nanmean(theseEstimates,3);
        theseSEMs = nanstd(theseEstimates,[],3)./sqrt(sum(~isnan(theseEstimates),3));
        
        if thisParameter ~= 1
           
            theseMeans = theseMeans'.*itemDurations;
            theseSEMs = theseSEMs'.*itemDurations;
            
        end

        theseUpperBounds = theseMeans+theseSEMs;
        theseLowerBounds = theseMeans-theseSEMs;
        
        
        for thisCondition = 1:nConditions

            line([thisCondition thisCondition],[theseLowerBounds(thisCondition) theseUpperBounds(thisCondition)], 'Color', squeeze(modelLineColor(thisStream,:)));
            hold on;

        end

        scatter(1:4,theseMeans,modelParamIcon);

        box on;
        axis(modelParameterAxes_byTime(thisParameter,:));
        axis square;
        set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
    end

end
    

% Individual

for thisParticipant = 1:nParticipants

    for thisParameter = 1:nFreeParameters

        thisPlot = thisPlot+1;
        subplot(nParticipants+2,nFreeParameters,thisPlot);

        for thisStream = 1:nStreams

            if thisStream==1
                modelParamIcon = modelParamIcon_S1;
            else
                modelParamIcon = modelParamIcon_S2;
            end

            for thisCondition = 1:nConditions

                theseLowerBounds = allLowerBounds_byParticipant(thisCondition,thisStream,thisParticipant,thisParameter);
                theseUpperBounds = allUpperBounds_byParticipant(thisCondition,thisStream,thisParticipant,thisParameter);
                
                if thisParameter~=1
                
                    theseLowerBounds = theseLowerBounds*itemDurations(thisCondition);
                    theseUpperBounds = theseUpperBounds*itemDurations(thisCondition);

                end
                
                line([thisCondition thisCondition],[theseLowerBounds theseUpperBounds], 'Color', squeeze(modelLineColor(thisStream,:)));
                hold on;

            end

            theseEstimates = allEstimates_byParticipant(:,thisStream,thisParticipant,thisParameter);
        
            if thisParameter~=1
                theseEstimates = theseEstimates'.*itemDurations;
            end

            scatter(1:4,theseEstimates,modelParamIcon);

            box on;
            axis(modelParameterAxes_byTime(thisParameter,:));
            axis square;
            set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTick',1:4,'XTickLabel',conditionLabels);
        end

    end
    
end

% Plot categorised responses

    % Combined
    
    categorisedResponses_Combined = figure('color','white','name','Categorised Responses (Combined)');
    thisPlot = 0;
    
    for thisCondition = 1:nConditions
        
        for thisStream = 1:nStreams
            
            thisData = reshape(compiledErrors(thisCondition,:,:,:,thisStream),1,nSessions*nTrials*nParticipants);
            thisData = thisData(~isnan(thisData));
            
            theseTargets = reshape(trialIsTarget_Combined(thisCondition,:,:,:,thisStream),1,nSessions*nTrials*nParticipants);
            theseTargets = theseTargets(~isnan(theseTargets));
            
            theseErrors = reshape(trialIsError_Combined(thisCondition,:,:,:,thisStream),1,nSessions*nTrials*nParticipants);
            theseErrors = theseErrors(~isnan(theseErrors));
            
            theseNeither = double((~theseTargets)&(~theseErrors));
            
            theseTargets = thisData(theseTargets==1);
            theseErrors = thisData(theseErrors==1);
            theseNeither = thisData(theseNeither==1);
            
            theseTrials = numel(thisData);
            
            targetHist = histc(theseTargets,errorValues)/theseTrials;
            errorHist = histc(theseErrors,errorValues)/theseTrials;
            neitherHist = histc(theseNeither,errorValues)/theseTrials;          
            
            thisPlot = thisPlot+1;
            subplot(nConditions,nStreams,thisPlot);
            hold on;
            
            % Plot data
           
            handleBars = bar(errorValues,[errorHist; neitherHist; targetHist]','stacked');
            set(handleBars(1),'FaceColor',errorColor)
            set(handleBars(2),'FaceColor',neitherColor)
            set(handleBars(3),'FaceColor',targetColor)
                        
            % Format axes
            
            axis(categorisedDistributionAxes);
            axis square;
            set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');
            box on;
            
        end
        
    end
            
	% Individual
    
    categorisedResponses_Individual = NaN(1,nParticipants);
    
    for thisParticipant = 1:nParticipants
        
        thisName = ['Categorised Responses (' allParticipants{thisParticipant} ')'];
        categorisedResponses_Individual(thisParticipant) = figure('color','white','name', thisName);
        thisPlot = 0;

        for thisCondition = 1:nConditions

            for thisStream = 1:nStreams

                thisData = reshape(compiledErrors(thisCondition,thisParticipant,:,:,thisStream),1,nSessions*nTrials);
            
                theseTargets = reshape(trialIsTarget_byParticipant(thisCondition,thisParticipant,:,:,thisStream),1,nSessions*nTrials);
                theseErrors = reshape(trialIsError_byParticipant(thisCondition,thisParticipant,:,:,thisStream),1,nSessions*nTrials);
                
                thisData = thisData(~isnan(thisData));
                theseTargets = theseTargets(~isnan(theseTargets));
                theseErrors = theseErrors(~isnan(theseErrors));
                
                theseNeither = double((~theseTargets)&(~theseErrors));

                theseTargets = thisData(theseTargets==1);
                theseErrors = thisData(theseErrors==1);
                theseNeither = thisData(theseNeither==1);

                theseTrials = numel(thisData);

                targetHist = histc(theseTargets,errorValues)/theseTrials;
                errorHist = histc(theseErrors,errorValues)/theseTrials;
                neitherHist = histc(theseNeither,errorValues)/theseTrials;          

                thisPlot = thisPlot+1;
                subplot(nConditions,nStreams,thisPlot);
                hold on;

                % Plot data

                handleBars = bar(errorValues,[errorHist; neitherHist; targetHist]','stacked');
                set(handleBars(1),'FaceColor',errorColor)
                set(handleBars(2),'FaceColor',neitherColor)
                set(handleBars(3),'FaceColor',targetColor)

                % Format axes

                axis(categorisedDistributionAxes);
                axis square;
                set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');
                box on;

            end

        end 
        
    end
    

    
% Plot correlograms of Left vs Right error (Target only)

% Axis Labels:  x, 'Response Position L (Items)'; y, 'Response Position R (Items)';
% Row Labels: (Participants) 'Combined', 'Pooled Individual', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Item Rates) 'Combined', '6 i/s', '8 i/s', '12 i/s', '24 i/s';

positionCorrelograms_Target_byItem = figure('color','white','name','Position Correlograms by Item (Target)');
thisPlot = 0;

thisCondition = 1;

trialIsTarget_Combined_L = squeeze(trialIsTarget_Combined(thisCondition,:,:,:,1));
trialIsTarget_Combined_R = squeeze(trialIsTarget_Combined(thisCondition,:,:,:,2));
trialIsTarget_Combined_Both = trialIsTarget_Combined_L&trialIsTarget_Combined_R;

trialIsTarget_byParticipant_L = squeeze(trialIsTarget_byParticipant(thisCondition,:,:,:,1));
trialIsTarget_byParticipant_R = squeeze(trialIsTarget_byParticipant(thisCondition,:,:,:,2));
trialIsTarget_byParticipant_Both = trialIsTarget_byParticipant_L&trialIsTarget_byParticipant_R;

thisData_L = squeeze(compiledErrors(thisCondition,:,:,:,1));
thisData_R = squeeze(compiledErrors(thisCondition,:,:,:,2));

% Combined

thisPosition_L = thisData_L(trialIsTarget_Combined_Both==1);
thisPosition_R = thisData_R(trialIsTarget_Combined_Both==1);

thisPlot = thisPlot+1;
subplot((nParticipants+2),1,thisPlot);

[thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

% Expand crosstab to full grid

uniqueL = unique(thisPosition_L);
uniqueR = unique(thisPosition_R);
nUniqueL = numel(uniqueL);
nUniqueR = numel(uniqueR);

nGrid = numel(targetCorrelogramValues);
fullTable = zeros(nGrid,nGrid);


for thisL = 1:nUniqueL

    for thisR = 1:nUniqueR

        posL = uniqueL(thisL);
        posR = uniqueR(thisR);

        fullPosL = find(targetCorrelogramValues==posL);
        fullPosR = find(targetCorrelogramValues==posR);

        fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

    end

end

imagesc(targetCorrelogramValues, targetCorrelogramValues, fullTable);
colormap(thisColormapReverse);
axis square;
axis(positionCorrelogramsAxes);
set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
        'XMinorTick','on','YMinorTick','on','XTick', ...
        [min(targetCorrelogramValues) 0 max(targetCorrelogramValues)], ...
        'YTick',[min(targetCorrelogramValues) 0 max(targetCorrelogramValues)]);
    
    
% Pooled individual
     
thisPosition_L = thisData_L(trialIsTarget_byParticipant_Both==1);
thisPosition_R = thisData_R(trialIsTarget_byParticipant_Both==1);
    
thisPlot = thisPlot+1;
subplot((nParticipants+2),1,thisPlot);

[thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

% Expand crosstab to full grid

uniqueL = unique(thisPosition_L);
uniqueR = unique(thisPosition_R);
nUniqueL = numel(uniqueL);
nUniqueR = numel(uniqueR);

nGrid = numel(targetCorrelogramValues);
fullTable = zeros(nGrid,nGrid);


for thisL = 1:nUniqueL

    for thisR = 1:nUniqueR

        posL = uniqueL(thisL);
        posR = uniqueR(thisR);

        fullPosL = find(targetCorrelogramValues==posL);
        fullPosR = find(targetCorrelogramValues==posR);

        fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

    end

end

imagesc(targetCorrelogramValues, targetCorrelogramValues, fullTable);
colormap(thisColormapReverse);
axis square;
axis(positionCorrelogramsAxes);
set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
    'XMinorTick','on','YMinorTick','on','XTick', ...
    [min(targetCorrelogramValues) 0 max(targetCorrelogramValues)], ...
    'YTick',[min(targetCorrelogramValues) 0 max(targetCorrelogramValues)]); 


    % Individual
    
    for thisParticipant = 1:nParticipants
    
        thisPosition_Lp = squeeze(thisData_L(:,thisParticipant,:,:));
        thisPosition_Rp = squeeze(thisData_R(:,thisParticipant,:,:));
        
        trialIsTarget_Bp = squeeze(trialIsTarget_byParticipant_Both(:,thisParticipant,:,:));
        
        thisPlot = thisPlot+1;

        thisPosition_L = thisPosition_Lp(trialIsTarget_Bp==1);
        thisPosition_R = thisPosition_Rp(trialIsTarget_Bp==1);

        [thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

        % Expand crosstab to full grid

        uniqueL = unique(thisPosition_L);
        uniqueR = unique(thisPosition_R);
        nUniqueL = numel(uniqueL);
        nUniqueR = numel(uniqueR);

        nGrid = numel(targetCorrelogramValues);
        fullTable = zeros(nGrid,nGrid);


        for thisL = 1:nUniqueL

            for thisR = 1:nUniqueR

                posL = uniqueL(thisL);
                posR = uniqueR(thisR);

                fullPosL = find(targetCorrelogramValues==posL);
                fullPosR = find(targetCorrelogramValues==posR);

                fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

            end

        end

        if sum(fullTable(:)>0)
            subplot((nParticipants+2),1,thisPlot);
            imagesc(targetCorrelogramValues, targetCorrelogramValues, fullTable);
            colormap(thisColormapReverse);
            axis square;
            axis(positionCorrelogramsAxes);
            set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
                'XMinorTick','on','YMinorTick','on','XTick', ...
                [min(targetCorrelogramValues) 0 max(targetCorrelogramValues)], ...
                'YTick',[min(targetCorrelogramValues) 0 max(targetCorrelogramValues)]);
        end    
        
    end
    
    
    
    
    
    
% Plot correlograms of Left vs Right error (Target only) by time

% Axis Labels:  x, 'Response Position L (ms)'; y, 'Response Position R (ms)';
% Row Labels: (Participants) 'Combined', 'Pooled Individual', 'AH', 'SM', 'EN', 'PG';
% Column Labels: (Item Rates) 'Combined', '6 i/s', '8 i/s', '12 i/s', '24 i/s';

positionCorrelograms_Target_byTime = figure('color','white','name','Position Correlograms by Time (Target)');
thisPlot = 0;

thisCondition = 1;

trialIsTarget_Combined_L = squeeze(trialIsTarget_Combined(thisCondition,:,:,:,1));
trialIsTarget_Combined_R = squeeze(trialIsTarget_Combined(thisCondition,:,:,:,2));
trialIsTarget_Combined_Both = trialIsTarget_Combined_L&trialIsTarget_Combined_R;

trialIsTarget_byParticipant_L = squeeze(trialIsTarget_byParticipant(thisCondition,:,:,:,1));
trialIsTarget_byParticipant_R = squeeze(trialIsTarget_byParticipant(thisCondition,:,:,:,2));
trialIsTarget_byParticipant_Both = trialIsTarget_byParticipant_L&trialIsTarget_byParticipant_R;

thisData_L = squeeze(compiledErrors(thisCondition,:,:,:,1));
thisData_R = squeeze(compiledErrors(thisCondition,:,:,:,2));

% Combined

thisPosition_L = thisData_L(trialIsTarget_Combined_Both==1);
thisPosition_R = thisData_R(trialIsTarget_Combined_Both==1);

thisPlot = thisPlot+1;
subplot((nParticipants+2),1,thisPlot);

[thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

% Expand crosstab to full grid

uniqueL = unique(thisPosition_L);
uniqueR = unique(thisPosition_R);
nUniqueL = numel(uniqueL);
nUniqueR = numel(uniqueR);

nGrid = numel(targetCorrelogramValues);
fullTable = zeros(nGrid,nGrid);


for thisL = 1:nUniqueL

    for thisR = 1:nUniqueR

        posL = uniqueL(thisL);
        posR = uniqueR(thisR);

        fullPosL = find(targetCorrelogramValues==posL);
        fullPosR = find(targetCorrelogramValues==posR);

        fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

    end

end

imagesc(targetCorrelogramValues_byTime, targetCorrelogramValues_byTime, fullTable);
colormap(thisColormapReverse);
axis square;
axis(positionCorrelogramsAxes_byTime);
set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
        'XMinorTick','on','YMinorTick','on','XTick',positionCorrelogramsAxisTicks, ...
        'YTick',positionCorrelogramsAxisTicks);
    
    
% Pooled individual
     
thisPosition_L = thisData_L(trialIsTarget_byParticipant_Both==1);
thisPosition_R = thisData_R(trialIsTarget_byParticipant_Both==1);
    
thisPlot = thisPlot+1;
subplot((nParticipants+2),1,thisPlot);

[thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

% Expand crosstab to full grid

uniqueL = unique(thisPosition_L);
uniqueR = unique(thisPosition_R);
nUniqueL = numel(uniqueL);
nUniqueR = numel(uniqueR);

nGrid = numel(targetCorrelogramValues);
fullTable = zeros(nGrid,nGrid);


for thisL = 1:nUniqueL

    for thisR = 1:nUniqueR

        posL = uniqueL(thisL);
        posR = uniqueR(thisR);

        fullPosL = find(targetCorrelogramValues==posL);
        fullPosR = find(targetCorrelogramValues==posR);

        fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

    end

end

imagesc(targetCorrelogramValues_byTime, targetCorrelogramValues_byTime, fullTable);
colormap(thisColormapReverse);
axis square;
axis(positionCorrelogramsAxes_byTime);
set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
        'XMinorTick','on','YMinorTick','on','XTick',positionCorrelogramsAxisTicks, ...
        'YTick',positionCorrelogramsAxisTicks);


    % Individual
    
    for thisParticipant = 1:nParticipants
    
        thisPosition_Lp = squeeze(thisData_L(:,thisParticipant,:,:));
        thisPosition_Rp = squeeze(thisData_R(:,thisParticipant,:,:));
        
        trialIsTarget_Bp = squeeze(trialIsTarget_byParticipant_Both(:,thisParticipant,:,:));
        
        thisPlot = thisPlot+1;

        thisPosition_L = thisPosition_Lp(trialIsTarget_Bp==1);
        thisPosition_R = thisPosition_Rp(trialIsTarget_Bp==1);

        [thisTable,thisChi2,thisP,theseLabels] = crosstab(thisPosition_R,thisPosition_L);

        % Expand crosstab to full grid

        uniqueL = unique(thisPosition_L);
        uniqueR = unique(thisPosition_R);
        nUniqueL = numel(uniqueL);
        nUniqueR = numel(uniqueR);

        nGrid = numel(targetCorrelogramValues);
        fullTable = zeros(nGrid,nGrid);


        for thisL = 1:nUniqueL

            for thisR = 1:nUniqueR

                posL = uniqueL(thisL);
                posR = uniqueR(thisR);

                fullPosL = find(targetCorrelogramValues==posL);
                fullPosR = find(targetCorrelogramValues==posR);

                fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

            end

        end

        if sum(fullTable(:)>0)
            subplot((nParticipants+2),1,thisPlot);
            imagesc(targetCorrelogramValues_byTime, targetCorrelogramValues_byTime, fullTable);
            colormap(thisColormapReverse);
            axis square;
            axis(positionCorrelogramsAxes_byTime);
            set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
                'XMinorTick','on','YMinorTick','on','XTick',positionCorrelogramsAxisTicks, ...
                'YTick',positionCorrelogramsAxisTicks);
        end    
        
    end
    