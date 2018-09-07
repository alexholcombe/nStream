################################################
###Method M from Wilcox and Erceg-Hurn (2014)###
#################### And #######################
#########Shift function analysis################
################################################
rm(list=ls())

library(WRS)

setwd('~/gitCode/nStream/')

eightStreamsFiles <- list.files(path='wrangledData/SONA/eightStreams/', full.names = T, recursive = T)
twoStreamsFiles <- list.files(path='wrangledData/SONA/twoStreams/', full.names = T, recursive = T)

IDs <- unlist(strsplit(eightStreamsFiles, 'wrangledData/SONA/eightStreams//|\\.txt'))
IDs <- IDs[seq(2,length(IDs),2)]

conditions <- c('twoStreams', 'eightStreams')

B

for(thisParticipant in IDs[1]){
  eightStreams <- read.table(eightStreamsFiles[grep(thisParticipant,eightStreamsFiles)], sep='\t', header = T)
  twoStreams <- read.table(twoStreamsFiles[grep(thisParticipant,twoStreamsFiles)], sep='\t', header = T)
  
  allData <- data.frame(condition = as.factor(c(eightStreams$streamsPerRing, twoStreams$streamsPerRing)), error=c(eightStreams$responsePosRelative0, twoStreams$responsePosRelative0))
  
  test <- shiftdhd(data = allData, error~condition)
}