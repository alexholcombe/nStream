library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
rm(list=ls())

setwd('~/gitCode/nStream/')

nReps = 100

rate <- 1000/12

GandH <- read.csv('Analysis/Goodbourn and Holcombe/G&HParams.csv', stringsAsFactors = F)
GandHTrials <- read.csv('Analysis/Goodbourn and Holcombe/Data and Materials/allData.csv', stringsAsFactors = F)

###G&H fit models to data from separate streams, but we want to collapse streams and ignore the spatial information.
###So let's use the mixRSVP package to fit the collapsed data. 


twoStreamsOneTarget <- GandHTrials %>% filter(condition == 2, (exp == 'Exp2' & pool == 'Experienced Observers') | (exp == 'Exp1' & pool == 'SONA'))

paramFiles <- list.files(
  path = 'Analysis/Goodbourn and Holcombe',
  pattern = 'ParamsTwoStreamSingleCue2019.csv',
  full.names = T
  )

if(length(paramFiles)>0){
  params <- read.csv(paramFiles[1], stringsAsFactors = F)
} else{
  
  params <- expand.grid(
    participant = unique(twoStreamsOneTarget$ID),
    exp = character(1),
    pool = character(1),
    efficacy = numeric(1),
    latency = numeric(1),
    precision = numeric(1),
    val = numeric(1),
    valGuessing = numeric(1),
    pLRtest = numeric(1),
    stringsAsFactors = F
  )
  
  
  
  for(thisParticipant in unique(twoStreamsOneTarget$ID)){
    thisExp <- twoStreamsOneTarget %>% filter(ID == thisParticipant) %>% pull(exp) %>% unique()
    
    thisPool <- twoStreamsOneTarget %>% filter(ID == thisParticipant) %>% pull(pool) %>% unique()
    
    cat('Participant', thisParticipant, 'in', thisExp, 'from the', thisPool, 'pool.                                          \r')
    
    theseTrials <- twoStreamsOneTarget %>% filter( #Put the SPEs in the same column, because we don't care about the stream location for this analysis
      ID == thisParticipant,
      condition == 2,
      exp == thisExp
    ) %>%
      dcast(ID+trial+block~stream, value.var = 'SPE') %>%
      rename('One' = '1', 'Two' = '2') %>%
      mutate(SPE = ifelse(is.na(One), Two, One))
    
    
    
    theseTargetSPs <- GandHTrials %>% filter(
      ID == thisParticipant,
      condition == 2,
      exp == thisExp
    ) %>% 
      dcast(ID+trial+block~stream, value.var = 'targetSP') %>%
      rename('One' = '1', 'Two' = '2') %>%
      mutate(targetSP = ifelse(is.na(One), Two, One)) %>%
      pull(targetSP)
    
    theseParams <- theseTrials %>% 
      mutate(targetSP = theseTargetSPs) %>% 
      analyzeOneConditionDF(., 24, parameterBounds(), nReps)
    
    params %<>% mutate(
      exp = replace(exp, participant == thisParticipant, thisExp),
      pool = replace(pool, participant == thisParticipant, thisPool),
      efficacy = replace(efficacy, participant == thisParticipant, theseParams$efficacy),
      latency = replace(latency, participant == thisParticipant, theseParams$latency),
      precision = replace(precision, participant == thisParticipant, theseParams$precision),
      val = replace(val, participant == thisParticipant, theseParams$val),
      valGuessing = replace(val, participant == thisParticipant, theseParams$valGuessing),
      pLRtest = replace(pLRtest, participant == thisParticipant, theseParams$pLRtest)
    )
    
  }
  
  params %<>%
    mutate(latency = latency * rate,
           precision = precision * rate,
           pBetween = -999,
           nReps = nReps,
           date = strftime(Sys.time())
    )
  
  for(thisParticipant in params$participant){
    thisLatency <- params %>% filter(participant == thisParticipant) %>% pull(latency)
    thisPrecision <- params %>% filter(participant == thisParticipant) %>% pull(precision)
    
    cdfOne <- pnorm(rate, thisLatency, thisPrecision)
    cdfZero <- pnorm(0, thisLatency, thisPrecision)
    thisPBetween <- cdfOne - cdfZero
    params %<>% mutate(
      pBetween = replace(pBetween, participant == thisParticipant, thisPBetween)
    )
  }
  
  write.csv(
    file = 'Analysis/Goodbourn and Holcombe/ParamsTwoStreamSingleCue2019.csv',
    x = params)
  
}

params %>% summarise(mean = mean(pBetween), 
                     sd = sd(pBetween))

densities <- params %>% #Purrr is new to me
  select(participant,latency, precision)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, precision, participant){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,500,1)/(1000/12)
    latency <- latency/(1000/12)
    precision <- precision/(1000/12)
    data.frame(                                       #pmap_dfr iterates over multiple arguments (cells in each row) simultaneously
      participant = participant,                      #It expects a df from the function and binds the output together by row, hence _dfr (dataframe row). Perfect for ggplot
      SPE = SPE,
      density = dnorm(SPE, latency, precision),
      stringsAsFactors = F
    )
  }
  )


densities %>%
  ggplot(aes(x = SPE, y = density))+
  geom_area(aes(fill = SPE >= 0 & SPE <= 1))+
  facet_wrap(~participant, nrow = 5)+
  scale_fill_manual(values = c('TRUE' = '#ffa951' , 'FALSE' = '#628093'))+
  scale_x_continuous(breaks = -3:3)
