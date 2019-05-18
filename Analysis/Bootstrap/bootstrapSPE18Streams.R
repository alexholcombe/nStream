library(ggplot2)
library(dplyr)
library(magrittr)
library(mixRSVP)
library(reshape2)
setwd('~/gitCode/nStream/')

allErrors <- read.csv('Analysis/allErrors18Streams.txt', header = T, stringsAsFactors = F)

runAnyway <- FALSE
plotExample <- FALSE #Generates the components of the bootstrap explainer plot
xDomain <- -4:4

speNs <- function(theseData){
  xDomain = -4:4
  
  outs <- data.frame(xDomain = xDomain, counts = -999)
  for(thisX in xDomain){
    count <- length(which(theseData$SPE == thisX))
    outs$counts[outs$xDomain == thisX] <- count
  }
  
  outs
}



bootstrapPValue <- function(theseData, numItemsInStream, whichSPE, nReps){
  nTrials <- nrow(theseData)

  #############################
  ###Generate pseudo-uniform###
  #############################
  
  thisAccuracy <- length(which(theseData$SPE == 0))/nTrials
  
  thisGuessingRate <- 1 - thisAccuracy
  print(thisAccuracy)
  print(thisGuessingRate)
  
  maxSPETheseData <- theseData %>% pull(targetSP) %>% max
  minSPETheseData <- theseData %>% pull(targetSP) %>% min
  minSPE <- 1 - maxSPETheseData
  maxSPE <- numItemsInStream - minSPETheseData

  guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                             maxSPE = maxSPE,
                                             targetSP = theseData$targetSP,
                                             numItemsInStream = numItemsInStream)

  pseudoUniform <- data.frame(xDomain = minSPE:maxSPE, guessingDist = guessingDist, prob = guessingDist/sum(guessingDist)) #give guessing dist values labels

  

  nWhichSPE <- theseData %>% filter(SPE == whichSPE) %>% nrow() #How many observations at SPE = whichSPE?

  bootstraps <- data.frame(rep = 1:nReps, #store the counts of whichSPE sampled from the pseudouniform here.
                           count = -999)

  for(i in 1:nReps){
    thisSample <- sample(pseudoUniform$xDomain, prob = pseudoUniform$prob, replace = T, size = nTrials*thisGuessingRate) #sample nTrials many trials from the pseudouniform over the range of possible SPEs in this experiment
    nThisSPE<- which(thisSample == whichSPE) %>% length #How many of the trials had an SPE == whichSPE
    bootstraps %<>% mutate(count = replace(count, rep == i, nThisSPE)) #Put the count nThisSPE in the dataframe
  }

  return(length(which(bootstraps$count>=nWhichSPE))/nReps) #a p.value. The proportion of bootstrapped samples that had a count of whichSPE at least as great as the observed count

}


numItemsInStream = 24

pFiles <- list.files(pattern = 'bootstrapPValues18Streams.*\\.csv', 
                     path = 'Analysis/Bootstrap', 
                     full.names = T)


if(length(pFiles)>0 & !runAnyway){
  splits <- pFiles %>% strsplit(x = .,
                                split = '18Streams|\\.csv')
  
  
  dates <- lapply(splits,
                  FUN = function(x){
                    x[2] %>% as.POSIXct(., format = "%d-%m-%Y_%H-%M-%S")
                  }) %>% unlist
  
  whichLatestDate <- which(dates == max(dates))
  
  ps <- read.csv(pFiles[whichLatestDate])
} else{
  ps <- expand.grid(xDomain = xDomain,
                    p = -1,
                    participant = unique(allErrors$ID),
                    condition = unique(allErrors$condition))
  for(thisParticipant in unique(allErrors$ID)){
    print(thisParticipant)
    for(thisCondition in unique(allErrors$condition)){
      print(thisCondition)
      theseData <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
      for(whichSPE in xDomain){
        print(whichSPE)
        thisP <- bootstrapPValue(theseData,numItemsInStream,whichSPE,5000)
        ps %<>% mutate(p = replace(p, xDomain == whichSPE & participant == thisParticipant & condition == thisCondition, thisP))
      }
    }
  }
  write.csv(ps, paste0('Analysis/bootstrapPValues18Streams',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.csv'),row.names = F)
}

ps <- allData %>% group_by(ID, condition) %>% do(speNs(.)) %>% rename(participant = ID) %>% left_join(ps, ., by = c('participant', 'condition', 'xDomain'))
ps <- allData %>% group_by(ID, condition) %>% summarise(ntrials = n()) %>% rename(participant = ID) %>% left_join(ps, ., by = c('participant', 'condition'))


ps %<>% filter(participant != '18TR1')

ps %>% filter(p != -1) %>% ggplot(., aes(x = xDomain, y = p))+
  geom_line(aes(colour = factor(condition)))+
  facet_wrap(~participant,nrow = 3)+
  geom_hline(yintercept = .05, linetype = 'dashed')+
  geom_vline(xintercept = -1, linetype = 'dashed')+
  labs(x = 'SPE', y = 'p', colour = 'nStream')

table <- ps %>% group_by(condition, xDomain) %>% summarise(nSig = length(which(p<.05/12 & p >-1)))

table %<>% mutate(milliseconds = xDomain*(1000/12))

ggplot(table, aes(x = xDomain, y = nSig))+
  geom_line(aes(linetype = condition))

table %>% dcast(.,
                xDomain ~ condition,
                value.var = 'nSig')



bootstrapPlot <- table %>% ggplot(., aes(x=xDomain, y = nSig))+
  geom_line(aes(linetype = factor(condition)))+
  scale_x_continuous(breaks = -9:9)+
  scale_y_continuous(breaks = seq(0,12,3))+
  labs(x = 'SPE', y = 'Deviations from Guessing',linetype = 'nStreams')+
  scale_linetype_manual(values = c('2' = 'solid', '6' = 'dotted', '18' = 'twodash'))

bootstrapPlot

ggsave(filename = 'modelOutput/18Streams/bootstrapPlot.png',
       plot = bootstrapPlot,
       width=30, 
       height=30,
       units='cm')

#######################
###Example Bootstrap###
#######################

if(plotExample){
  randID <- sample(allErrors$ID, size = 1)
  exampleCondition = 2
  whichSPE = 2
  nReps <- 5000
  
  exampleData <- allErrors %>% filter(ID == randID, condition == exampleCondition, !fixationReject)
  exampleNTrials <- nrow(exampleData)
  
  exampleHistogram <- ggplot(exampleData, aes(x = SPE))+
    geom_histogram(binwidth = 1, aes(fill = SPE == whichSPE))+
    geom_vline(xintercept = 0, linetype = 'dashed')+
    scale_x_continuous(breaks = seq(min(exampleData$SPE),max(exampleData$SPE),2))+
    scale_fill_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
    labs(y = 'Count')+
    theme_apa()
  
  exampleHistogram
  
  ggsave(exampleHistogram, 
         file = paste0('modelOutput/18Streams/bootstrapExampleEmpirical', randID,'.png'),
         width = 10,
         height = 10,
         units = 'cm')
  
  exampleAccuracy <- length(which(exampleData$SPE == 0))/exampleNTrials
  
  exampleGuessingRate <- 1 - exampleAccuracy
  print(exampleAccuracy)
  print(exampleGuessingRate)
  
  maxSPETheseData <- exampleData %>% pull(targetSP) %>% max
  minSPETheseData <- exampleData %>% pull(targetSP) %>% min
  minSPE <- 1 - maxSPETheseData
  maxSPE <- numItemsInStream - minSPETheseData
  
  thisGuessingDist <- createGuessingDistribution(minSPE = minSPE,
                                                 maxSPE = maxSPE,
                                                 targetSP = exampleData$targetSP,
                                                 numItemsInStream = numItemsInStream)
  
  pseudoUniform <- data.frame(xDomain = minSPE:maxSPE, guessingDist = thisGuessingDist, prob = (thisGuessingDist/sum(thisGuessingDist))) #give guessing dist values labels
  
  
  exampleGuessingDist <- ggplot(pseudoUniform, aes(x = xDomain, y = prob))+
    geom_bar(stat= 'identity', aes(fill = xDomain == whichSPE))+
    labs(x = 'SPE', y = 'Probability of Response')+
    scale_fill_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
    theme_apa()
  exampleGuessingDist
  
  ggsave(exampleGuessingDist, 
         file = paste0('modelOutput/18Streams/bootstrapExampleGuessing', randID,'.png'),
         width = 10,
         height = 10,
         units = 'cm')
  
  nWhichSPE <- exampleData %>% filter(SPE == whichSPE) %>% nrow() #How many observations at SPE = whichSPE?
  
  bootstraps <- data.frame(rep = 1:nReps, #store the counts of whichSPE sampled from the pseudouniform here.
                           count = -999)
  
  
  theseSamples <- expand.grid(sample = 1:nReps, SPE = numeric(exampleNTrials*exampleGuessingRate))
  
  for(i in 1:nReps){
    thisSample <- sample(pseudoUniform$xDomain, prob = pseudoUniform$prob, replace = T, size = exampleNTrials*exampleGuessingRate) #sample nTrials many trials from the pseudouniform over the range of possible SPEs in this experiment
    theseSamples %<>% mutate(SPE = replace(SPE, sample == i, thisSample))
    nThisSPE<- which(thisSample == whichSPE) %>% length #How many of the trials had an SPE == whichSPE
    bootstraps %<>% mutate(count = replace(count, rep == i, nThisSPE)) #Put the count nThisSPE in the dataframe
  }
  
  randomSamplesForPlotting <- sample(1:nReps, 12, replace = F)
  
  randomSamples <- theseSamples %>% filter(sample %in% randomSamplesForPlotting) %>% 
    ggplot(., aes(x = SPE))+
    geom_histogram(binwidth = 1, aes(fill = SPE == whichSPE))+
    scale_fill_manual(values = c('TRUE' = '#ffa960', 'FALSE' = '#628093'), guide = FALSE)+
    labs(y = 'Count')+
    theme_apa()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )+
    facet_wrap(~sample, nrow = 4)
  
  randomSamples
  
  ggsave(randomSamples, 
         file = paste0('modelOutput/18Streams/randomBootstrapHistograms', randID,'.png'),
         width = 10,
         height = 10,
         units = 'cm')
  
  bootstrapCountDist <- ggplot(bootstraps, aes(x = count))+
    geom_histogram(binwidth = 1, aes(fill = count >= nWhichSPE))+
    scale_fill_manual(values = c('TRUE' = '#ffa951', 'FALSE' = '#628093'), guide = FALSE)+
    labs(x = 'Number of SPEs = 2', y = 'Number of Samples')+
    theme_apa()
  
  ggsave(bootstrapCountDist, 
         file = paste0('modelOutput/18Streams/bootstrapCountDistribution', randID,'.png'),
         width = 10,
         height = 10,
         units = 'cm')
  
  
  
  
  thisP <- length(which(bootstraps$count>=nWhichSPE))/nReps #a p.value. The proportion of bootstrapped samples that had a count of whichSPE at least as great as the observed count
  
  
}
