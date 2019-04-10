clear all;
close all;


% Load data files

load('DualRSVP_Exp2_OutputsByPosition.mat');

% SET ---------------------------------------------------------------------

% Add directories

addpath(genpath('/Users/experimentalmode/Documents/MATLAB/'));
cd('/Users/experimentalmode/Documents/MATLAB/DualRSVP/Exp2/Model/');

targetPositions = 7:18;
targetPositionsMS = targetPositions*(1000/12);
functionX = linspace(min(targetPositionsMS),max(targetPositionsMS),1000);
nTargetPositions = numel(targetPositions);

newFig = figure('color','white','name','Error Distributions (Combined)');

for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,1,1) allUpperBounds_Combined(thisPosition,1,1)],'Color',[0 0 1]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,1,1),'bo');
fit1 = ezfit(targetPositionsMS, allEstimates_Combined(:,1,1)', 'sinphic');
plot(functionX, fit1.m(1)*sin((fit1.m(2)*functionX)+fit1.m(4))+fit1.m(3),'b-');


for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,2,1) allUpperBounds_Combined(thisPosition,2,1)],'Color',[1 0 0]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,2,1),'ro');
fit2 = ezfit(targetPositionsMS, allEstimates_Combined(:,2,1)', 'sinphic');
plot(functionX, fit2.m(1)*sin((fit2.m(2)*functionX)+fit2.m(4))+fit2.m(3),'r-');

% Format axes

axis([500 1750 0 1]);
axis square;
set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');



newFig2 = figure('color','white','name','Error Distributions (Combined)');

for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,1,1) allUpperBounds_Combined(thisPosition,1,1)],'Color',[0 0 1]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,1,2),'bo');
fit1 = ezfit(targetPositionsMS, allEstimates_Combined(:,1,2)', 'sinphic');
plot(functionX, fit1.m(1)*sin((fit1.m(2)*functionX)+fit1.m(4))+fit1.m(3),'b-');


for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,2,1) allUpperBounds_Combined(thisPosition,2,1)],'Color',[1 0 0]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,2,2),'ro');
fit2 = ezfit(targetPositionsMS, allEstimates_Combined(:,2,2)', 'sinphic');
plot(functionX, fit2.m(1)*sin((fit2.m(2)*functionX)+fit2.m(4))+fit2.m(3),'r-');

% Format axes

axis([500 1750 0 1]);
axis square;
set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');






newFig3 = figure('color','white','name','Error Distributions (Combined)');

for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,1,1) allUpperBounds_Combined(thisPosition,1,1)],'Color',[0 0 1]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,1,3),'bo');
fit1 = ezfit(targetPositionsMS, allEstimates_Combined(:,1,3)', 'sinphic');
plot(functionX, fit1.m(1)*sin((fit1.m(2)*functionX)+fit1.m(4))+fit1.m(3),'b-');


for thisPosition = 1:nTargetPositions

    %line(targetPositionsMS(thisPosition)*ones(1,2),[allLowerBounds_Combined(thisPosition,2,1) allUpperBounds_Combined(thisPosition,2,1)],'Color',[1 0 0]);
    hold on;
    
end

scatter(targetPositionsMS, allEstimates_Combined(:,2,3),'ro');
fit2 = ezfit(targetPositionsMS, allEstimates_Combined(:,2,3)', 'sinphic');
plot(functionX, fit2.m(1)*sin((fit2.m(2)*functionX)+fit2.m(4))+fit2.m(3),'r-');

% Format axes

axis([500 1750 0 2]);
axis square;
set(gca,'TickDir','out','TickLength', [0.025 0.05],'XMinorTick','on','YMinorTick','on');