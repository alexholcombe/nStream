rm(list=ls())
library(ggplot2)
library(dplyr)
library(magrittr)
setwd('~/Google Drive/AllRSVP/')

allRSVP <- read.csv('parameterMasterDF.csv', stringsAsFactors = F)

propBeforeCue <- function(theseParams)
  theseProportions <- expand.grid(participant = theseParticipants,
                                  condition = theseConditions,
                                  stream = 1:2,
                                  proportion = 999)
  for(thisCondition in theseConditions){
    for(thisParticipant in theseParticipants){
      for(thisStream in 1:2){
        latency <- theseParams %>% filter(participant == thisParticipant, condition == thisCondition, parameter == 'Latency', stream == thisStream) %>% pull(estimate)
        precision <- theseParams %>% filter(participant == thisParticipant, condition == thisCondition, parameter == 'Precision', stream == thisStream) %>% pull(estimate)
        thisProp <- pnorm(0, latency, precision)
        theseProportions %<>% mutate(proportion = replace(proportion, participant == thisParticipant & condition == thisCondition & stream == thisStream, thisProp))
      }
    }
  }
  return(theseProportions)
}

GHOne <- allRSVP %>% filter(experiment == 'DualRSVP_SONA_Exp1' | experiment == 'DualRSVP_Exp2')
GHTwo <- allRSVP %>% filter(experiment == 'DualRSVP_SONA_Exp2' | experiment == 'DualRSVP_Exp3')
GHThree <- allRSVP %>% filter(experiment == 'DualRSVP_SONA_Exp3' | experiment == 'DualRSVP_Exp4')

GHOne %<>% mutate(participant_kind = ifelse(experiment == 'DualRSVP_SONA_Exp1', 'Naive', 'Experienced'))
GHTwo %<>% mutate(participant_kind = ifelse(experiment == 'DualRSVP_SONA_Exp2', 'Naive', 'Experienced'))
GHThree %<>% mutate(participant_kind = ifelse(experiment == 'DualRSVP_SONA_Exp3', 'Naive', 'Experienced'))

GHOneProp <- propBeforeCue(GHOne)
GHTwoProp <- propBeforeCue(GHTwo)
GHThreeProp <- propBeforeCue(GHThree)
