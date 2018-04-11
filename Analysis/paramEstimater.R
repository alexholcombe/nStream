library(mixRSVP)
library(dplyr)
library(magrittr)

data.files <- list.files('../wrangledData/SONA/', full.names = T, recursive = T)

drop.these <- grep(
  pattern = '18Streams',
  x = data.files
)

data.files <- data.files[-drop.these]

IDs <- data.files %>%
  strsplit('Streams/|\\.txt') %>%
  lapply(function(x) x[2]) %>%
  unlist %>%
  .[1:10]

nStreamParameterEst <- expand.grid(
  participant = IDs,
  nStreams = c('twoStreams', 'eightStreams'),
  efficacy = 999,
  latency = 999,
  precision = 999
)

if(!file.exists('../../modelOutput/CSV/2v8mixRSVPParams.csv')){
  for(file in data.files){
    temp <- read.table(file, sep = '\t', header = T)
    ID <- temp$subject[1] %>% as.character() %>% gsub(' ','',.)
    condition <- file %>% strsplit('/SONA//|/[A-Z][A-Z]') %>% lapply(function(x) x[2]) %>% unlist
    temp %<>% rename(targetSP = cuePos0, SPE = responsePosRelative0)
    theseParams <- temp %>% analyzeOneCondition(.,24,parameterBounds(),100)
    
    theseParams$latency %<>% `*`(1000/12)
    theseParams$precision %<>% `*`(1000/12)
    
    nStreamParameterEst %<>%
      mutate(efficacy=replace(efficacy,nStreams == condition & participant == ID, theseParams$efficacy)) %>%
      as.data.frame()
    nStreamParameterEst %<>%
      mutate(latency=replace(latency,nStreams == condition & participant == ID, theseParams$latency)) %>%
      as.data.frame()
    nStreamParameterEst %<>%
      mutate(precision=replace(precision,nStreams == condition & participant == ID, theseParams$precision)) %>%
      as.data.frame()
  }
  write.csv(nStreamParameterEst, file = '../../modelOutput/CSV/2v8mixRSVPParams.csv', col.names = T)
} else {
  nStreamParameterEst <- read.csv('../../modelOutput/CSV/2v8mixRSVPParams.csv', header = T)
}

