########################
###4 rates experiment###
########################
# Thu Apr 18 13:36:56 2019 ------------------------------


rm(list=ls())
library(R.matlab)
library(ggplot2)
library(papaja)
library(reshape2)
library(mixRSVP)
setwd('~/gitCode/nStream/')

skew <- function(x){
  denom <- (sd(x,na.rm=T)^3)*(length(which(!is.na(x)))-1)*(length(which(!is.na(x)))-2)
  deviation <- mean(x,na.rm=T)-x
  numer <- sum(deviation^3)*length(which(!is.na(x)))
  numer/denom
}


if(!dir.exists('Bayes')){
  dir.create('Bayes')
}

##############
##Parameters##
##############

experimentDetails <- list(
  rates = c(6,8,12,24),
  nConditions = 4,
  participants = c('AH','CW','EN','FJ','PG','SM'),
  nParticipants = 6,
  parameterNames = c('Efficacy','Latency','Precision'),
  letters = LETTERS[!LETTERS %in% c('C','V')]
)

efficacy <- read.csv('modelOutput/CSV/TGRSVP_Exp2_EfficacyNorm.csv', stringsAsFactors = F)
latency <- read.csv('modelOutput/CSV/TGRSVP_Exp2_LatencyNorm.csv', stringsAsFactors = F)
precision <- read.csv('modelOutput/CSV/TGRSVP_Exp2_PrecisionNorm.csv', stringsAsFactors = F)

params <- rbind(efficacy, latency, precision)
params %<>% mutate(Group = ordered(Group, levels = c('6_per_sec', '8_per_sec', '12_per_sec', '24_per_sec')))

params %>% filter(Parameter == 'Efficacy') %>%
  ggplot(., aes(x = Group, y = Estimate, fill = factor(Stream)))+
    geom_violin()+
    geom_point(aes(group = factor(Stream)), position = position_dodge(.9), alpha = .5)+
    stat_summary(geom = 'point', aes(shape='Means'), position = position_dodge(.9), fun.y = mean, size = 5)+
    stat_summary(geom = 'errorbar', position = position_dodge(.9), fun.data = mean_se, width = .1)+
    scale_shape_manual(name = NULL,values = c('Means' = 23))+
    scale_fill_manual(name = 'Stream',values = c('1' = '#AA2B3C','2' = '#FB7A48'))+
    labs(x = 'Rate', y = 'Estimate', title = 'Efficacy')+
    theme_apa()+
    theme(plot.title = element_text(hjust=.5))


params %>% filter(Parameter == 'Latency') %>%
  ggplot(., aes(x = Group, y = Estimate, fill = factor(Stream)))+
  geom_violin()+
  geom_point(aes(group = factor(Stream)), position = position_dodge(.9), alpha = .5)+
  stat_summary(geom = 'point', aes(shape='Means'), position = position_dodge(.9), fun.y = mean, size = 5)+
  stat_summary(geom = 'errorbar', position = position_dodge(.9), fun.data = mean_se, width = .1)+
  scale_shape_manual(name = NULL,values = c('Means' = 23))+
  scale_fill_manual(name = 'Stream',values = c('1' = '#AA2B3C','2' = '#FB7A48'))+
  labs(x = 'Rate', y = 'Estimate', title = 'Latency')+
  theme_apa()+
  theme(plot.title = element_text(hjust=.5))


params %>% filter(Parameter == 'Precision') %>%
  ggplot(., aes(x = Group, y = Estimate, fill = factor(Stream)))+
  geom_violin()+
  geom_point(aes(group = factor(Stream)), position = position_dodge(.9), alpha = .5)+
  stat_summary(geom = 'point', aes(shape='Means'), position = position_dodge(.9), fun.y = mean, size = 5)+
  stat_summary(geom = 'errorbar', position = position_dodge(.9), fun.data = mean_se, width = .1)+
  scale_shape_manual(name = NULL,values = c('Means' = 23))+
  scale_fill_manual(name = 'Stream',values = c('1' = '#AA2B3C','2' = '#FB7A48'))+
  labs(x = 'Rate', y = 'Estimate', title = 'Precision')+
  theme_apa()+
  theme(plot.title = element_text(hjust=.5))

theseData <- data.frame(
  participant = character(1e5),
  SPE = numeric(1e5),
  targetSP = numeric(1e5),
  trial = numeric(1e5),
  rate = numeric(1e5),
  stream = numeric(1e5),
  stringsAsFactors = F
  )


startRow <- 1
for(rate in c(6,8,12,24)){
  thisFile <- paste0('modelOutput/compiled/CompiledData_TGRSVP_Exp2_', rate,'_per_sec.mat')
  thisMat <- readMat(thisFile)
  nParticipants <- dim(thisMat$compiledErrors)[2]
  nStreams <- dim(thisMat$compiledErrors)[5]
  nBlocks <- dim(thisMat$compiledErrors)[3]
  nTrialsPerBlock <- dim(thisMat$compiledErrors)[4]
  
  for(thisParticipant in 1:nParticipants){
    
    trial <- 0
    thisParticipantCharacter <- thisMat$allParticipants[[thisParticipant]][[1]][1] #Hmmmmmmmmmmmm
    for(thisBlock in 1:nBlocks){
      for(thisStream in 1:nStreams){
        theseErrors <- thisMat$compiledErrors[1, thisParticipant,thisBlock, ,thisStream]
        theseTargets <- thisMat$compiledTargets[1, thisParticipant,thisBlock, ,thisStream]
        theseTrials <- (max(trial)+1):(max(trial)+25)
        
        temp <- data.frame(
          participant = thisParticipantCharacter,
          SPE = theseErrors,
          targetSP = theseTargets,
          trial = theseTrials,
          rate = rate,
          stream = thisStream,
          stringsAsFactors = F
        )
        
        endRow = startRow + nrow(temp) - 1
        theseData[startRow:endRow,] <- temp
        
        trial <- max(theseTrials)
        startRow <- endRow+1
      }
    }
  }
}

theseData <- theseData[-(startRow:nrow(theseData)),]


theseData %>% 
filter(rate == 6) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~participant+stream)+
  theme_apa()


theseData %>% 
  filter(rate == 8) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~participant+stream)+
  theme_apa()

theseData %>% 
  filter(rate == 12) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~participant+stream)+
  theme_apa()

theseData %>% 
  filter(rate == 24) %>%
  ggplot(aes(x = SPE))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~participant+stream)+
  theme_apa()