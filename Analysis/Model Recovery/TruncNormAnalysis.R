library(ggplot2)
rm(list = ls())
paramEstimates <- read.csv('~/gitCode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/TruncNorm/estimatesAndTruthTruncNorm.csv', header = F)
paramLowerCI <- read.csv('~/gitCode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/TruncNorm/estimatesAndTruthLowerBoundTruncNorm.csv', header = F)
paramUpperCI <- read.csv('~/gitCode/nStream/modellingScripts/Model recovery/EstimatesAndTruth/TruncNorm/estimatesAndTruthUpperBoundTruncNorm.csv', header = F)

colnames(paramEstimates) <-  c(
  'Participant',
  'NonGuessingRate',
  'Mean',
  'SD',
  'Efficacy',
  'Latency',
  'Precision'
)

colnames(paramLowerCI) <-  c(
  'Participant',
  'NonGuessingRate',
  'Mean',
  'SD',
  'EfficacyLower',
  'LatencyLower',
  'PrecisionLower'
)

colnames(paramUpperCI) <-  c(
  'Participant',
  'NonGuessingRate',
  'Mean',
  'SD',
  'EfficacyUpper',
  'LatencyUpper',
  'PrecisionUpper'
)

CIs <- cbind(paramLowerCI[,1:7], paramUpperCI[,5:7])



cor.test(paramEstimates$NonGuessingRate, paramEstimates$Efficacy)

ggplot(paramEstimates, aes(NonGuessingRate,Efficacy))+
  geom_point(colour = '#628093')+
  geom_errorbar(data=CIs, aes(NonGuessingRate, ymin = EfficacyLower, ymax=EfficacyUpper),alpha = .2, width = .15,  inherit.aes = F, colour = '#628093')+
  stat_summary(fun.y = 'mean', geom='point', size = 4, colour = '#dca951')+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = .2, colour = '#dca951')+
  scale_x_continuous(breaks = unique(paramEstimates$NonGuessingRate))+
  scale_y_continuous(breaks = unique(paramEstimates$NonGuessingRate))+
  geom_hline(yintercept = unique(paramEstimates$NonGuessingRate), linetype = 'dashed', alpha = .3)+
  theme(panel.background = element_blank(),axis.line = element_line(size = .5))


cor.test(paramEstimates$Mean, paramEstimates$Latency)

ggplot(paramEstimates, aes(Mean,Latency))+
  geom_point(colour = '#628093')+
  geom_errorbar(data=CIs, aes(Mean, ymin = LatencyLower, ymax=LatencyUpper),alpha = .2, width = .15,  inherit.aes = F, colour = '#628093')+
  stat_summary(fun.y = 'mean', geom='point', size = 4, colour = '#dca951')+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = .2, colour = '#dca951')+
  scale_x_continuous(breaks = unique(paramEstimates$Mean))+
  scale_y_continuous(breaks = c(2:3, unique(paramEstimates$Mean)))+
  geom_hline(yintercept = unique(paramEstimates$Mean), linetype = 'dashed', alpha = .3)+
  theme(panel.background = element_blank(),axis.line = element_line(size = .5))


cor.test(paramEstimates$SD, paramEstimates$Precision)

ggplot(paramEstimates, aes(SD,Precision))+
  geom_point(colour = '#628093')+
  geom_errorbar(data=CIs, aes(SD, ymin = PrecisionLower, ymax=PrecisionUpper),alpha = .2, width = .15,  inherit.aes = F, colour = '#628093')+
  stat_summary(fun.y = 'mean', geom='point', size = 4, colour = '#dca951')+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = .2, colour = '#dca951')+
  scale_x_continuous(breaks = unique(paramEstimates$SD))+
  scale_y_continuous(breaks = c(2:3, unique(paramEstimates$SD)))+
  geom_hline(yintercept = unique(paramEstimates$SD), linetype = 'dashed')+
  theme(panel.background = element_blank(),axis.line = element_line(size = .5))

