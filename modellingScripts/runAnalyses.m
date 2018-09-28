clear all
addpath('~/gitCode/nStream');
cd('~/gitCode/nStream')
%Compile data
run('modellingScripts/CompileData.m')
%Run trunc normal
cd('~/gitCode/nStream')
run('modellingScripts/TruncNormMixtureFittingOneSD.m')
%Run normal
cd('~/gitCode/nStream')
run('modellingScripts/NormalMixtureFitting.m')
%model comparison
cd('~/gitCode/nStream')
run('modellingScripts/modelComparison.m')