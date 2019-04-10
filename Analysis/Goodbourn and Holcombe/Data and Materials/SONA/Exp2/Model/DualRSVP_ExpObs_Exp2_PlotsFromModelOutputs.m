addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP/Exp3/Model');
load('DualRSVP_Exp3_Outputs_2Conditions.mat');

% Cull participants
% theseParticipants = setdiff(1:20,[1 10 20]);
theseParticipants = 1:6;

allEstimates_byParticipant = allEstimates_byParticipant(:,:,theseParticipants,:);
compiledErrors = compiledErrors(:,theseParticipants,:,:,:);
% EHI = EHI(theseParticipants);
% allAge = allAge(theseParticipants);
% allSex = allSex(theseParticipants);

% Experiment 1 has 2 model conditions, switch around so that Bilateral = 1
% and Unilateral = 2
allEstimates_byParticipant = allEstimates_byParticipant([2 1],:,:,:);
compiledErrors = compiledErrors([2 1],:,:,:,:);

nParticipants = numel(theseParticipants);
nParameters = 3;
nConditions = 2;
nPositions = 2;
itemDuration = 1000/12;
thisColormap = 1-gray(256);
nBoots = 1000; % 100000
alpha = .05;

parameterNames = {'efficacy','latency','precision'};

% Correlogram
thisData_L = compiledErrors(:,:,:,:,1);
thisData_R = compiledErrors(:,:,:,:,2);

combinedTable = zeros(35,35,nConditions);

    for thisCondition = 1:nConditions

        thisPosition_L = squeeze(thisData_L(thisCondition,:,:,:));
        thisPosition_R = squeeze(thisData_R(thisCondition,:,:,:));

        thisPosition_L = reshape(thisPosition_L,[numel(thisPosition_L) 1]);
        thisPosition_R = reshape(thisPosition_R,[numel(thisPosition_R) 1]);

        thisTable = crosstab(thisPosition_R,thisPosition_L);

        % Expand crosstab to full grid

        uniqueL = unique(thisPosition_L);
        uniqueR = unique(thisPosition_R);
        uniqueL = uniqueL(~isnan(uniqueL));
        uniqueR = uniqueR(~isnan(uniqueR));
        nUniqueL = numel(uniqueL);
        nUniqueR = numel(uniqueR);

        theseValues = -17:17;

        fullTable = zeros(35);

        for thisL = 1:nUniqueL

            for thisR = 1:nUniqueR

                posL = uniqueL(thisL);
                posR = uniqueR(thisR);

                fullPosL = find(theseValues==posL);
                fullPosR = find(theseValues==posR);

                fullTable(fullPosR,fullPosL) = thisTable(thisR,thisL);

            end

        end

       combinedTable(:,:,thisCondition) = combinedTable(:,:,thisCondition)+fullTable;
       
       figure('color','white','name', ['Correllogram Condition ' num2str(thisCondition)]);
       theseValues = itemDuration*(-17:17);

    imagesc(theseValues, theseValues, squeeze(combinedTable(:,:,thisCondition)));
    colormap(thisColormap);
    axis square;
    axis(500*[-1 1 -1 1]);
    set(gca,'YDir','normal','TickDir','out','TickLength', [0.025 0.05], ...
        'XMinorTick','off','YMinorTick','off','XTick', ...
        -500:100:500, 'YTick',-500:100:500);
    line([-1000 1000],[-1000 1000],'Color',[0 0 0],'LineStyle','--')

end
    
% T-tests    

fprintf('\nT-TESTS\n');
fprintf('-----------------------');

for thisParameter = 1:nParameters
    
    fprintf('\n\n* %s\n', parameterNames{thisParameter});

    theseData = allEstimates_byParticipant(:,:,:,thisParameter);
    
    C1Data = squeeze(theseData(1,:,:));
    C2Data = squeeze(theseData(2,:,:));

    C1Data_S1 = C1Data(1,:);
    C1Data_S2 = C1Data(2,:);
    C1Data_M = nanmean(C1Data);
    C1Data_D = abs(C1Data(1,:)-C1Data(2,:));

    C2Data_S1 = C2Data(1,:);
    C2Data_S2 = C2Data(2,:);
    C2Data_M = nanmean(C2Data);
    C2Data_D = abs(C2Data(1,:)-C2Data(2,:));
    
    S1Data_D = abs(C1Data(1,:)-C2Data(1,:));
    S2Data_D = abs(C1Data(2,:)-C2Data(2,:));

    [hC1,pC1,ciC1,statsC1] = ttest(C1Data_S1, C1Data_S2);
    [hC2,pC2,ciC2,statsC2] = ttest(C2Data_S1, C2Data_S2);
    [hM,pM,ciM,statsM] = ttest(C1Data_M, C2Data_M);
    [hD,pD,ciD,statsD] = ttest(C1Data_D, C2Data_D);
    
    [hS1,pS1,ciS1,statsS1] = ttest(C1Data_S1, C2Data_S1);
    [hS2,pS2,ciS2,statsS2] = ttest(C1Data_S2, C2Data_S2);
    [hSD,pSD,ciSD,statsSD] = ttest(S1Data_D, S2Data_D);

    effectC1 = nanmean(C1Data_D)/nanstd(C1Data_D);
    effectC2 = nanmean(C2Data_D)/nanstd(C2Data_D);
    effectM = nanmean(C1Data_M-C2Data_M)/nanstd(C1Data_M-C2Data_M);
    effectD = nanmean(C1Data_D-C2Data_D)/nanstd(C1Data_D-C2Data_D);
    
    effectS1 = nanmean(C1Data_S1-C2Data_S1)/nanstd(C1Data_S1-C2Data_S1);
    effectS2 = nanmean(C1Data_S2-C2Data_S2)/nanstd(C1Data_S2-C2Data_S2);
    effectSD = nanmean(S1Data_D-S2Data_D)/nanstd(S1Data_D-S2Data_D);
    
    if thisParameter~=1
        ciC1 = ciC1*itemDuration;
        ciC2 = ciC2*itemDuration;
        ciM = ciM*itemDuration;
        ciD = ciD*itemDuration;
        
        ciS1 = ciS1*itemDuration;
        ciS2 = ciS2*itemDuration;
        ciSD = ciSD*itemDuration;
    end
    
    fprintf('\nCondition 1\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsC1.df, statsC1.tstat, pC1, effectC1, ciC1(1), ciC1(2));
    fprintf('\nCondition 2\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsC2.df, statsC2.tstat, pC2, effectC2, ciC2(1), ciC2(2));
    fprintf('\nMean\t\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsM.df, statsM.tstat, pM, effectM, ciM(1), ciM(2));
    fprintf('\nDifference\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsD.df, statsD.tstat, pD, effectD, ciD(1), ciD(2));
    fprintf('\n\nStream 1\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsS1.df, statsS1.tstat, pS1, effectS1, ciS1(1), ciS1(2));
    fprintf('\nStream 2\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsS2.df, statsS2.tstat, pS2, effectS2, ciS2(1), ciS2(2));
    fprintf('\nStream Diff.\t t(%2d) = %0.2f,\t p = %.4f,\t d = %0.2f,\t CI = [%.3f,\t %.3f].', statsSD.df, statsSD.tstat, pSD, effectSD, ciSD(1), ciSD(2));
end

fprintf('\n\n\n');

% Bootstraps

fprintf('\nBOOTSTRAPS\n');
fprintf('-----------------------');

allBoots_C1 = NaN(nBoots,nParameters);
allBoots_C2 = NaN(nBoots,nParameters);
allBoots_M = NaN(nBoots,nParameters);
allBoots_D = NaN(nBoots,nParameters);

allBoots_S1 = NaN(nBoots,nParameters);
allBoots_S2 = NaN(nBoots,nParameters);
allBoots_SD = NaN(nBoots,nParameters);

for thisBoot = 1:nBoots
   
    sampleVector = randi(nParticipants,[1 nParticipants]);
    thisData = allEstimates_byParticipant(:,:,sampleVector,:);
    
    for thisParameter = 1:nParameters
        theseData = thisData(:,:,:,thisParameter);
    
        C1Data = squeeze(theseData(1,:,:));
        C2Data = squeeze(theseData(2,:,:));

        C1Data_S1 = C1Data(1,:);
        C1Data_S2 = C1Data(2,:);
        C1Data_M = nanmean(C1Data);
        C1Data_D = abs(C1Data(1,:)-C1Data(2,:));

        C2Data_S1 = C2Data(1,:);
        C2Data_S2 = C2Data(2,:);
        C2Data_M = nanmean(C2Data);
        C2Data_D = abs(C2Data(1,:)-C2Data(2,:));
        
        S1Data_D = abs(C1Data(1,:)-C2Data(1,:));
        S2Data_D = abs(C1Data(2,:)-C2Data(2,:));
        
        allBoots_C1(thisBoot,thisParameter) = nanmean(C1Data_S1 - C1Data_S2);
        allBoots_C2(thisBoot,thisParameter) = nanmean(C2Data_S1 - C2Data_S2);
        allBoots_M(thisBoot,thisParameter) = nanmean(C1Data_M - C2Data_M);
        allBoots_D(thisBoot,thisParameter) = nanmean(C1Data_D - C2Data_D);
        
        allBoots_S1(thisBoot,thisParameter) = nanmean(C1Data_S1 - C2Data_S1);
        allBoots_S2(thisBoot,thisParameter) = nanmean(C1Data_S2 - C2Data_S2);
        allBoots_SD(thisBoot,thisParameter) = nanmean(S1Data_D - S2Data_D);

    end
    
end

    for thisParameter = 1:nParameters
        
        fprintf('\n\n* %s\n', parameterNames{thisParameter});
        
        C1_CI = prctile(allBoots_C1(:,thisParameter),100*[alpha (1-alpha)]);
        C2_CI = prctile(allBoots_C2(:,thisParameter),100*[alpha (1-alpha)]);
        M_CI = prctile(allBoots_M(:,thisParameter),100*[alpha (1-alpha)]);
        D_CI = prctile(allBoots_D(:,thisParameter),100*[alpha (1-alpha)]);
        
        S1_CI = prctile(allBoots_S1(:,thisParameter),100*[alpha (1-alpha)]);
        S2_CI = prctile(allBoots_S2(:,thisParameter),100*[alpha (1-alpha)]);
        SD_CI = prctile(allBoots_SD(:,thisParameter),100*[alpha (1-alpha)]);
        
        if thisParameter~=1
            C1_CI = C1_CI*itemDuration;
            C2_CI = C2_CI*itemDuration;
            M_CI = M_CI*itemDuration;
            D_CI = D_CI*itemDuration;
            S1_CI = S1_CI*itemDuration;
            S2_CI = S2_CI*itemDuration;
            SD_CI = SD_CI*itemDuration;
        end
        
        fprintf('\nCondition 1\t CI(boot) = [%.3f,\t %.3f]', C1_CI(1), C1_CI(2));
        fprintf('\nCondition 2\t CI(boot) = [%.3f,\t %.3f]', C2_CI(1), C2_CI(2));
        fprintf('\nMean\t\t CI(boot) = [%.3f,\t %.3f]', M_CI(1), M_CI(2));
        fprintf('\nDifference\t CI(boot) = [%.3f,\t %.3f]', D_CI(1), D_CI(2));
        fprintf('\n\nStream 1\t CI(boot) = [%.3f,\t %.3f]', S1_CI(1), S1_CI(2));
        fprintf('\nStream 2\t CI(boot) = [%.3f,\t %.3f]', S2_CI(1), S2_CI(2));
        fprintf('\nStream Diff.\t CI(boot) = [%.3f,\t %.3f]', SD_CI(1), SD_CI(2));
    end

fprintf('\n\n\n');

% Parameter figures

allMeans = NaN(nParameters,nConditions,nPositions);
allCIs = NaN(nParameters,nConditions,nPositions);

for thisParameter = 1:nParameters
    
    theseData = allEstimates_byParticipant(:,:,:,thisParameter);
    
    for thisCondition = 1:nConditions
        
        conditionData = squeeze(theseData(thisCondition,:,:));
        
        for thisPosition = 1:nPositions
            
            positionData = squeeze(conditionData(thisPosition,:));
            allMeans(thisParameter,thisCondition,thisPosition) = nanmean(positionData);
            allCIs(thisParameter,thisCondition,thisPosition) = 1.96*(nanstd(positionData)/sqrt(sum(~isnan(positionData))));
            
        end
        
    end     

end

allMeans([2 3],:,:) = itemDuration*allMeans([2 3],:,:);
allCIs([2 3],:,:) = itemDuration*allCIs([2 3],:,:);

for thisParameter = 1:nParameters
   
    figure('color','white','name', ['Results ' parameterNames{thisParameter}]);
    barwitherr(squeeze(allCIs(thisParameter,:,:)),squeeze(allMeans(thisParameter,:,:)));
    box on;
    
    if thisParameter==1
    	axis([0 3 -0.1 1.1]);
        set(gca,'YTick',0:.25:1);
    else
        axis([0 3 -15 165]);
        set(gca,'YTick',0:25:150);
    end
    
    axis square;
    set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on','XTickLabel',num2str(1:nConditions));
    
    
end

% Odd-Even Reliability

% for thisParameter = 1:nParameters
%     
%     oddEstimates = squeeze(allReliability_byParticipant(1,:,:,thisParameter));
%     evenEstimates = squeeze(allReliability_byParticipant(2,:,:,thisParameter));
%     
%     if thisParameter~=1
%         oddEstimates = oddEstimates*itemDuration;
%         evenEstimates = evenEstimates*itemDuration;
%     end
%     
%     for thisPosition = 1:nPositions
%         
%         theseOdd = squeeze(oddEstimates(thisPosition,:));
%         theseEven = squeeze(evenEstimates(thisPosition,:));
%         
%         excludeThese = isnan(theseOdd)|isnan(theseEven);
%         
%         theseOdd = theseOdd(~excludeThese);
%         theseEven = theseEven(~excludeThese);
%         
%         figure('color','white','name', ['Reliability ' parameterNames{thisParameter} ' position ' num2str(thisPosition)]);
%         scatter(theseEven,theseOdd,'k.');
%         box on;
%         
%         if thisParameter==1
%             axis([-0.1 1.1 -0.1 1.1]);
%             set(gca,'YTick',0:.25:1,'XTick',0:.25:1);
%         else
%             axis([-15 165 -15 165]);
%             set(gca,'YTick',0:25:150,'XTick',0:25:150);
%         end
%         
%         axis square;
%         set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','off','YMinorTick','on');
%         
%         corr(theseOdd',theseEven')
%         
%     end
%     
% end

% Effects vs Handedness

fprintf('\nHANDEDNESS\n');
fprintf('-----------------------');

for thisParameter = 1:nParameters

    fprintf('\n\n* %s\n', parameterNames{thisParameter});

    theseData = allEstimates_byParticipant(:,:,:,thisParameter);
    
    C1Data = squeeze(theseData(1,:,:));
    C2Data = squeeze(theseData(2,:,:));

    C1Data_S1 = C1Data(1,:);
    C1Data_S2 = C1Data(2,:);
    C1Data_M = nanmean(C1Data);
    C1Data_D = abs(C1Data(1,:)-C1Data(2,:));

    C2Data_S1 = C2Data(1,:);
    C2Data_S2 = C2Data(2,:);
    C2Data_M = nanmean(C2Data);
    C2Data_D = abs(C2Data(1,:)-C2Data(2,:));
    
    S1Data_D = abs(C1Data(1,:)-C2Data(1,:));
    S2Data_D = abs(C1Data(2,:)-C2Data(2,:));

    effectC1 = C1Data_D;
    effectC2 = C2Data_D;
    effectM = C1Data_M-C2Data_M;
    effectD = C1Data_D-C2Data_D;
    
    effectS1 = C1Data_S1-C2Data_S1;
    effectS2 = C1Data_S2-C2Data_S2;
    effectSD = S1Data_D-S2Data_D;
    
    EHI_df_C1 = sum(~isnan(effectC1))-2;
    EHI_df_C2 = sum(~isnan(effectC2))-2;
    EHI_df_M = sum(~isnan(effectM))-2;
    EHI_df_D = sum(~isnan(effectD))-2;
    EHI_df_S1 = sum(~isnan(effectS1))-2;
    EHI_df_S2 = sum(~isnan(effectS2))-2;
    EHI_df_SD = sum(~isnan(effectSD))-2;
    
    [EHI_r_C1, EHI_p_C1] = corr(EHI',effectC1','rows','pairwise');
    [EHI_r_C2, EHI_p_C2] = corr(EHI',effectC2','rows','pairwise');
    [EHI_r_M, EHI_p_M] = corr(EHI',effectM','rows','pairwise');
    [EHI_r_D, EHI_p_D] = corr(EHI',effectD','rows','pairwise');
    
    [EHI_r_S1, EHI_p_S1] = corr(EHI',effectS1','rows','pairwise');
    [EHI_r_S2, EHI_p_S2] = corr(EHI',effectS2','rows','pairwise');
    [EHI_r_SD, EHI_p_SD] = corr(EHI',effectSD','rows','pairwise');
    
    % Create command text file for R polyserial correlations
    ehiData = EHI';
    
    for thisFile = 1:7
        
        if thisFile==1
            effectData = effectC1';
            thisFileName = 'R_Polyserial_C1.txt';
        elseif thisFile==2
            effectData = effectC2';
            thisFileName = 'R_Polyserial_C2.txt';
        elseif thisFile==3
            effectData = effectM';
            thisFileName = 'R_Polyserial_M.txt';
        elseif thisFile==4
            effectData = effectD';
            thisFileName = 'R_Polyserial_D.txt';
        elseif thisFile==5
            effectData = effectS1';
            thisFileName = 'R_Polyserial_S1.txt';
        elseif thisFile==6
            effectData = effectS2';
            thisFileName = 'R_Polyserial_S2.txt';
        elseif thisFile==7
            effectData = effectSD';
            thisFileName = 'R_Polyserial_SD.txt';
        end
        
        numBins = numel(-400:25:400);
        tempfileID = fopen(thisFileName,'w');

        fprintf(tempfileID,'effectData<-c(%f ,\n',effectData(1));
        for thisData = 2:(numel(effectData)-1)
            fprintf(tempfileID,'%f ,\n',effectData(thisData));
        end
        fprintf(tempfileID,'%f )\n',effectData(numel(effectData)));

        fprintf(tempfileID,'\n\n');

        fprintf(tempfileID,'ehiData<-c(%f ,\n',ehiData(1));
        for thisData = 2:(numel(ehiData)-1)
            fprintf(tempfileID,'%f ,\n',ehiData(thisData));
        end
        fprintf(tempfileID,'%f )\n',ehiData(numel(ehiData)));

        fprintf(tempfileID,'\n\n');

        fprintf(tempfileID,'library(polycor)\n');
        fprintf(tempfileID,'polyserial(effectData,ehiData, ML = TRUE, control = list(), std.err = TRUE, maxcor = .9999, bins = %d)\n', numBins);
        fclose(tempfileID);
        
        % Can't seem to run R from MATLAB, maybe a permissions problem.
        % So just saving the files to run later.
        
    end
    
    fprintf('\nCondition 1\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_C1, EHI_r_C1, EHI_p_C1);
    fprintf('\nCondition 2\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_C2, EHI_r_C2, EHI_p_C2);
    fprintf('\nMean\t\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_M, EHI_r_M, EHI_p_M);
    fprintf('\nDifference\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_D, EHI_r_D, EHI_p_D);
    fprintf('\nStream 1\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_S1, EHI_r_S1, EHI_p_S1);
    fprintf('\nStream 2\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_S2, EHI_r_S2, EHI_p_S2);
    fprintf('\nStream Diff.\t r(%2d) = %+.3f,\t p = %.4f.', EHI_df_SD, EHI_r_SD, EHI_p_SD);
     
end

fprintf('\n\n\n');