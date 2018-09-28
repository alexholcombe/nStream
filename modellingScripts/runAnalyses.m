clear all
addpath('~/gitCode/nStream');
cd('~/gitCode/nStream')
%Compile data
run('modellingScripts/TG_compileData_Exp2.m')
%Run trunc normal
cd('~/gitCode/nStream')
run('modellingScripts/TG_DualRSVP_truncNorm_Model_Exp2.m')
%Run normal
cd('~/gitCode/nStream')
run('modellingScripts/TG_DualRSVP_norm_Model_Exp2.m')
%model comparison
cd('~/gitCode/nStream')
run('modellingScripts/modelComparison.m')