library(ggplot2)
library(mixRSVP)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
library(papaja)
rm(list=ls())

knitr::opts_chunk$set(fig.align = "center",
                      warning = F,
                      echo = F)

nReps = 1

rate <- 1000/12

GandHTrials <- read.csv('Data and Materials/allData.csv', stringsAsFactors = F)

###G&H fit models to data from separate streams, but we want to collapse streams and ignore the spatial information.
###So let's use the mixRSVP package to fit the collapsed data. 


twoStreamsOneTarget <- GandHTrials %>% filter(condition == 2, (exp == 'Exp2' & pool == 'Experienced Observers') | (exp == 'Exp1' & pool == 'SONA'))

paramFiles <- list.files(
  path = '.',
  pattern = 'ParamsTwoStreamSingleCue2019.csv',
  full.names = T
  )

#######################
###Collapse the SPEs###
#######################

twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #
    dcast(ID+trial+block~stream, value.var = 'SPE') %>%
    rename('One' = '1', 'Two' = '2') %>%
    mutate(SPE = ifelse(is.na(One), Two, One)) 

twoStreamsOneTargetWide <- twoStreamsOneTarget %>% #Add targetSP
    dcast(ID+trial+block~stream, value.var = 'targetSP') %>%
    rename('One' = '1', 'Two' = '2') %>%
    mutate(targetSP = ifelse(is.na(One), Two, One)) %>%
    select(ID,trial, block, targetSP) %>%
    inner_join(twoStreamsOneTargetWide, by = c('ID','trial', 'block'))


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
    
    theseTrials <- twoStreamsOneTargetWide %>% filter( #Put the SPEs in the same column, because we don't care about the stream location for this analysis
      ID == thisParticipant
    )
    
    theseParams <- theseTrials %>%
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
  # 
  # write.csv(
  #   file = 'ParamsTwoStreamSingleCue2019.csv',
  #   x = params,
  #   row.names = F)
  # 
}

#####################
###Parameter Plots###
#####################

params %>% 
  select(efficacy)%>%
  ggplot(aes(y = efficacy))+
  geom_violin(aes(x = 1))+
  geom_point(aes(x = 1), position = 'dodge')+
  scale_x_continuous(breaks = NULL)+
  labs(x = NULL, y = 'Efficacy')+
  theme_apa()

params %>% 
  select(latency)%>%
  ggplot(aes(y = latency))+
  geom_violin(aes(x = 1))+
  geom_point(aes(x = 1), position = 'dodge')+
  scale_x_continuous(breaks = NULL)+
  labs(x = NULL, y = 'Latency')+
  theme_apa()

params %>% 
  select(precision)%>%
  ggplot(aes(y = precision))+
  geom_violin(aes(x = 1))+
  geom_point(aes(x = 1), position = 'dodge')+
  scale_x_continuous(breaks = NULL)+
  labs(x = NULL, y = 'Precision')+
  theme_apa()

#################################
##Param stats and p(0<=SPE<=1)###
#################################

params %>%
  melt(
    measure.vars = c('efficacy','latency','precision'),
    id.vars = c('participant'),
    variable.name = 'Parameter',
    value.name = 'Estimate'
  ) %>% 
  group_by(Parameter) %>%
  summarise(Mean = mean(Estimate),
            Sd = sd(Estimate)) %>%
  knitr::kable(digits = 2)
  
paramsForTable <- params %>% 
  select(participant, pBetween) %>%
  rename(Participant = participant, SPEZero = pBetween)

cbind(paramsForTable[1:13,], paramsForTable[14:26,]) %>%
  knitr::kable(digits = 2)
t.test(x = params$pBetween,
       mu = 1, 
       alternative = 'less'
       )

params %>% 
  summarise(mean = mean(pBetween),
            sd = sd(pBetween),
            min = min(pBetween),
            max = max(pBetween))

###################
###Density plots###
###################

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
  facet_wrap(~participant, ncol = 3)+
  scale_fill_manual(labels = c('TRUE' = '0<=SPE<=1', 'FALSE' = 'SPE < 0 or SPE >1'), values = c('TRUE' = '#ffa951' , 'FALSE' = '#628093'), name = NULL)+
  scale_x_continuous(breaks = -3:3)

#############################
###Observed counts at zero###
#############################

params <- twoStreamsOneTargetWide %>%
    group_by(ID) %>%
    summarise(propZero = sum(SPE == 0)/n()) %>%
    rename(participant = ID)%>%
    inner_join(params, by ='participant')

###################################
###Proportion of guesses at zero###
###################################

guessingDistSummarise <- function(targetSP){ #get proportion of guessing at 0

  maxSPTheseData <-targetSP %>% max
  minSPTheseData <- targetSP %>% min
  minSPE <- 1 - maxSPTheseData
  maxSPE <- 24 - minSPTheseData

  xDomain <- minSPE:maxSPE
  
  guessingDist <- createGuessingDistribution(minSPE = minSPE,
                                             maxSPE = maxSPE,
                                             targetSP = targetSP,
                                             numItemsInStream = 24)
  
  return(guessingDist[xDomain==0]/sum(guessingDist))
} 

#Add the guessing proportions to params
params <- twoStreamsOneTargetWide %>%
  group_by(ID) %>%
  summarise(guessingPropZero = guessingDistSummarise(targetSP)) %>%
  rename(participant= ID) %>%
  inner_join(params, by = 'participant')


#Predicted counts at zero
params %<>%
  mutate(predictedZero = (pBetween * efficacy) + (guessingPropZero * (1-efficacy)))

params %<>% mutate(
  greaterThanEfficacy = ifelse(predictedZero > efficacy, TRUE, FALSE)
)

params %>%
  select(efficacy, pBetween, guessingPropZero, predictedZero) %>%
  round(2)%>%
  View

lm(propZero~predictedZero, data = params) %>% summary()


ggplot(params, aes(x = predictedZero, y = propZero))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  geom_smooth(method = 'lm')

# params %<>% 
#   mutate(
#     predictedZero <- 
#   )
