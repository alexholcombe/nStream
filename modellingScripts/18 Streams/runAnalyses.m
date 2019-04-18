clear all
addpath('~/gitCode/nStream');
cd('~/gitCode/nStream')
%Compile data
run('modellingScripts/18 Streams/CompileData.m')
%Run trunc normal
cd('~/gitCode/nStream')
run('modellingScripts/18 Streams/TruncNormMixtureFittingOneSD.m')
%Run normal
cd('~/gitCode/nStream')
run('modellingScripts/18 Streams/NormalMixtureFitting.m')
%model comparison
cd('~/gitCode/nStream')
run('modellingScripts/18 Streams/modelComparison.m')