library(progress)

longData <- melt(rawData,id.vars = c('participant', 'model', 'latency', 'efficacy', 'precision'),measure.vars = list(SPE = paste0('SPE', 1:200), targetSP = paste0('targetSP', 1:200) ))

paramOtherModel <- params %>%
  mutate(model = ifelse(model == 'Normal', 'Gamma', 'Normal'),
         efficacyEstimate = -999,
         latencyEstimate = -999,
         precisionEstimate = -999, 
         val = -999,
         pLRTest = -999)

bar <- progress_bar$new(format = "Fitting models [:bar] :percent eta: :eta", ,total = 800)

for(thisEfficacy in efficacies){
  for(thisLatency in latencies){
    for(thisPrecision in precisions){
      for(thisParticipant in participants){
        for(thisModel in models){
          bar$tick()
          
          
          otherModel <- models[models != thisModel] #Fit the model that didn't generate the data
          
          #cat(paste0('Model: ', otherModel, ', Participant: ', thisParticipant,', Latency: ', thisLatency, ', Precision: ', thisPrecision, ', Efficacy: ', thisEfficacy, rep(' ', times = 50), '\r'))
          
          theseData <- longData[
            efficacy == thisEfficacy &
              latency == thisLatency &
              precision == thisPrecision &
              model == thisModel &
              participant == thisParticipant,
          ]
          
          thisRow <- (
            paramOtherModel$model == otherModel &
              paramOtherModel$latency == thisLatency & 
              paramOtherModel$efficacy == thisEfficacy &
              paramOtherModel$precision == thisPrecision &
              paramOtherModel$participant == thisParticipant
          )
          
          theseData <- as.data.frame(theseData)
          theseParams <- analyzeOneConditionDF(theseData, 24, parameterBounds(otherModel), 20, modelKind = otherModel)
          paramOtherModel$latencyEstimate[thisRow] <- theseParams$latency
          paramOtherModel$efficacyEstimate[thisRow] <- theseParams$efficacy
          paramOtherModel$precisionEstimate[thisRow] <- theseParams$precision
          paramOtherModel$val[thisRow] <- theseParams$val
          paramOtherModel$pLRTest[thisRow] <- theseParams$pLRtest
        }
      }
    }
  }
}

paramFileName <- paste0('modelRecoveryMixRSVPParamsOtherModel_', timeStamp,'.csv')

write.csv(x = paramOtherModel, #write the params and raw data
          file = paramFileName)


