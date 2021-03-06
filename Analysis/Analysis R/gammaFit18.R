###############################
###Date: 2019-04-25 14:18:37###
###Author: Charlie Ludowici####
###############################
#Eighteen Streams Gamma fitting
library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
library(BayesFactor)
library(papaja)
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.csv('Analysis/allErrors18Streams.txt', stringsAsFactors = F)

plots = F

IDs <- allData %>% pull(ID) %>% unique()
conditions <-  allData %>% pull(condition) %>% unique()

nReplications = 20
numLettersInStream <- 24

paramFiles <- list.files('Analysis/Gamma Fits/', pattern = 'params.*18', full.names = T)


analyses <- function(params, modelKind = NULL, bestFitting = FALSE, nIterations = 10000){
  if(bestFitting & is.null(modelKind)){
    params %<>% filter(model == favouredModel)
    modelKind = 'Best Fitting'
  } else if(bestFitting & !is.null(modelKind)) {
    paramsFavoured <- params %>% filter(model == favouredModel)
    paramsModelKind <- params %>% filter(model == modelKind)
    
    params <- paramsFavoured
    for(thisCondition in unique(params$condition)){ #We're going to have instances where the unfavoured model is modelKind, drop them
      thisModelKind <- paramsModelKind %>% filter(condition == thisCondition)
      thisModelKind %<>% filter(!participant %in% params$participant[params$condition == thisCondition])
      params <- rbind(params,thisModelKind)
    }
  } else if(!bestFitting & !is.null(modelKind)){
    params %<>% filter(model == modelKind)
  } else {
    stop('What are you doing? You need to specify modelKind or set bestFitting to True')
  }
  
  results <- list()
  

  params %<>%
    mutate(
      condition = factor(condition),
      participant = factor(participant)
    )
  
  #######################
  ###Efficacy Analyses###
  #######################
  
  efficacyFullAgainstNull <- anovaBF(efficacy ~ condition + participant,
                                     data=params,
                                     whichRandom = 'participant',
                                     progress = F
  )
  

  BayesFactorLabel <- efficacyFullAgainstNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  #Only evparticipantence for an effect of ring
  
  efficacyPlot = ggplot(params, aes(x=condition, y = efficacy))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 6)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9),  size = 7, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    labs(x = "Number of Streams", y = "Efficacy", title = paste0(modelKind, ': Efficacy'))+
    lims(y = c(0,1))+
    theme_apa(base_size = 30)

  
  results[['Efficacy']] <- list(
    'BF' = efficacyFullAgainstNull,
    'Plot' = efficacyPlot
  )
  
  #######################
  ###Latency Analyses###
  ####################### 
  latencyBFFullVSNull <- anovaBF(latency ~ condition + participant,
                                 data=params,
                                 whichRandom = 'participant', 
                                 progress = F
  )
  
  samples <- posterior(latencyBFFullVSNull, iterations=nIterations, progress = F) #http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
  consistent <- (samples[,'condition-2']<samples[,'condition-6'] & samples[,'condition-6']<samples[,'condition-18'])
  latencyBFOrderedVSFull <- (sum(consistent)/nIterations)/(1/6)
  
  latencyBFOrderedVSNull <- as.vector(latencyBFFullVSNull)*latencyBFOrderedVSFull
  
  BayesFactorLabelOne <- latencyBFOrderedVSNull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~Null]==', .)
  BayesFactorLabelTwo <- latencyBFOrderedVSFull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~No-Order]==', .)
  
  limits <- c(min(params$latency)-10, max(params$latency)+20)
  
  latencyPlot <- ggplot(params, aes(x=condition, y = latency))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 6)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9),  size = 7, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    lims(y = limits)+
    labs(x = 'Number of Streams', y = 'Latency (ms)', title = paste0(modelKind, ': Latency'))+
    theme_apa(base_size = 30)

  results[['Latency']] <- list(
    'BF' = list(
      'fullVNull' = latencyBFFullVSNull,
      'orderedVFull' = latencyBFOrderedVSFull,
      'orderedVNull' = latencyBFOrderedVSNull
    ),
    'Plot' = latencyPlot 
  )
  
  ########################
  ###Precision Analyses###
  ########################
  precisionBFFullVSNull <- anovaBF(precision ~ condition + participant,
                                   data=params,
                                   whichRandom = 'participant',
                                   progress = F
  )
  
  samples <- posterior(precisionBFFullVSNull, iterations=nIterations, progress = F) #http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
  consistent <- (samples[,'condition-2']>samples[,'condition-6'] & samples[,'condition-6']>samples[,'condition-18'])
  precisionBFOrderedVSFull <- (sum(consistent)/nIterations)/(1/6)
  
  precisionBFOrderedVSNull <- as.vector(precisionBFFullVSNull)*precisionBFOrderedVSFull
  
  BayesFactorLabelOne <- precisionBFOrderedVSNull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~Null]==', .)
  BayesFactorLabelTwo <- precisionBFOrderedVSFull %>% as.vector %>% round(2) %>% paste0('BF[Ordered~vs~No-Order]==', .)
  
  
  limits <- c(min(params$precision)-10, max(params$precision)+20)
  
  precisionPlot <- ggplot(params, aes(x=condition, y = precision))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 6)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9),  size = 7, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    lims(y = limits)+
    labs(x = 'Number of Streams', y = 'Precision (ms)', title = paste0(modelKind, ': Precision'))+
    theme_apa(base_size = 30)

  results[['Precision']] <- list(
    'BF' = list(
      'fullVNull' = precisionBFFullVSNull,
      'orderedVFull' = precisionBFOrderedVSFull,
      'orderedVNull' = precisionBFOrderedVSNull
    ),
    'Plot' = precisionPlot 
  )
  
  results
}

itemRate <- 80

if(length(paramFiles)>0){
  paramTimes <- paramFiles %>% gsub('.*paramsDF18_|\\.csv','', .) %>% as.POSIXct(format = '%d-%m-%Y_%H-%M')
  paramsDF <- read.csv(paramFiles[which(paramTimes == max(paramTimes))], stringsAsFactors = F)
  noticedTheFrameIssue <- strptime("01-08-2019_00-01", format = '%d-%m-%Y_%H-%M') #When I noticed the framerate thing
  
  if(max(paramTimes)<noticedTheFrameIssue){ #if the time the parameters were estimated was before I noticed the framerate thing, update the temporal estimates
    print('updated times to account for framerate')
    paramsDF %<>% mutate(
      shape = ifelse(model == 'Gamma', latency, NA), #Create gamma-specific params
      scale = ifelse(model == 'Gamma', precision, NA),
      latency = ifelse(model == 'Gamma', shape*scale, latency), #convert gamma params to temporal (SPE)
      precision = ifelse(model == 'Gamma', sqrt(shape)*scale, precision),
      latency = latency*itemRate, #Convert SPE temporal params to milliseconds
      precision = precision*itemRate,
      scale = scale*itemRate #So that the scale is in MS
    )
  }
} else {
  paramsDF <- expand.grid(
    participant = IDs,
    condition = conditions,
    efficacy = numeric(1),
    latency = numeric(1),
    precision = numeric(1),
    val = numeric(1),
    valGuessing = numeric(1),
    pLRtest = 999,
    model = c('Gamma','Normal')
  )
  
  parameterBoundsGamma <- data.frame(
    lower = c(0,0.1,0.1),
    upper = c(1,8,5)
  )
  
  parameterBoundsNormal <- data.frame(
    lower = c(0, -4, .001),
    upper = c(1, 4, 3)
  )
  
  nObs <- allData %>%
    filter(!fixationReject) %>%
    group_by(ID, condition) %>%
    summarise(n = n()) %>%
    rename(participant = ID)
  
  nParams <- c('Gamma' = 3, 'Normal' = 3)
  
  for(thisParticipant in IDs){
    for(thisCondition in conditions){
      theseData <- allData %>% filter(ID == thisParticipant, condition == thisCondition)
      
      cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting gamma model                                  \r', sep = '')
      paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
      cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting normal model                                  \r', sep = '')
      paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
      
      paramsDF %<>% mutate(
        efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$efficacy),
        latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$latency),
        precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$precision),
        val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$val),
        valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition  & model == 'Normal', paramsN$valGuessing),
        pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$pLRtest)
      )
      
      paramsDF %<>% mutate(
        efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$efficacy),
        latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$latency),
        precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$precision),
        val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$val),
        valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$valGuessing),
        pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$pLRtest)
      )
    }
  }
  
  
  BICs <- paramsDF %>%
    compareMixtureModels(params = .,
                         nParam = nParams,
                         nObs = nObs)
  
  
  paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
    mutate(favouredModel = replace(favouredModel, BF >2, 'Normal')) %>%
    mutate(favouredModel = replace(favouredModel, BF<2^-1, 'Gamma')) %>%
    select(participant, condition, BF, favouredModel) %>%
    left_join(paramsDF, ., by = c('participant', 'condition')) #Add all this information to the paramDF 
  
  paramsFile <- paste0('Analysis/Gamma Fits/paramsDF_', timeStamp, '.csv')
  
  write.csv(file = paramsFile,
            x = paramsDF, 
            row.names = F)
  
  paramsDF %<>% mutate(
    shape = ifelse(model == 'Gamma', latency, NA), #Create gamma-specific params
    scale = ifelse(model == 'Gamma', precision, NA),
    latency = ifelse(model == 'Gamma', shape*scale, latency), #convert gamma params to temporal (SPE)
    precision = ifelse(model == 'Gamma', sqrt(shape)*scale, precision),
    latency = latency*itemRate, #Convert SPE temporal params to milliseconds
    precision = precision*itemRate,
    scale = scale*itemRate 
  )
  
}

paramsDF %<>% filter(participant != '18TR1')

densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(latency, condition, precision, shape, scale, participant, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-500,1000,1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(favouredModel == 'Gamma'){
      if(model == 'Gamma'){
        density =dgamma(SPE, shape = shape, scale = scale)
        data.frame(ID = participant,
                   condition = condition,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } 
    } else if(favouredModel == 'Normal'){
      if(model == 'Normal'){
        density = dnorm(SPE, latency, precision)
        data.frame(ID = participant,
                   condition = condition,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    } else {
      if(model == 'Gamma'){
        density =dgamma(SPE, shape = shape, scale = scale)
        data.frame(ID = participant,
                   condition = condition,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      } else if(model == 'Normal'){
        density = dnorm(SPE, latency, precision)
        data.frame(ID = participant,
                   condition = condition,
                   SPE = SPE,
                   density = density,
                   model = model,
                   favouredModel = favouredModel,
                   stringsAsFactors = F)
      }
    }
  }
  )

densities %<>% mutate(SPE = SPE/80) #It was in ms, like the parameter estimates

if(plots){
  for(thisID in IDs){
    for(thisCondition in conditions){
      theseObservations <- allData %>% filter(ID == thisID, condition == thisCondition)
      
      theseDensities <- densities %>% filter(ID == thisID, condition == thisCondition)
      
      thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
      
      thisPlot <- ggplot(theseObservations, aes(x = SPE))+
        geom_histogram(binwidth = 1)+
        geom_line(data = theseDensities, aes(x = SPE, y = density*50, colour = model)) +
        labs(title = paste0('Participant: ', thisIDNoSpace,'. Condition: ', thisCondition))
      
      ggsave(filename = paste0('Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'_',thisCondition,'.png'),
             plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
      )
    } 
  }
  
  allPlot <- allData %>% 
    ggplot(aes(x = SPE))+
    geom_histogram(binwidth = 1)+
    geom_line(data = densities, aes(x = SPE, y = density*50, colour = model)) +
    geom_vline(xintercept = 0, linetype = 'dashed')+
    facet_wrap(~ID+condition)
  
  ggsave(filename = 'allData.png',
         plot = allPlot,
         height = 18,
         width = 32,
         units = 'cm')
}

gammaanalysis <- analyses(
  params = paramsDF,
  modelKind = 'Gamma'
  )

normalAnalysis <- analyses(
  params = paramsDF,
  modelKind = 'Normal'
)

plotHeight <-  6.90045
plotWidth <- 27.67/2

ggsave('~/latencyEighteenStream.png', plot = normalAnalysis$Latency$Plot,height = plotHeight, width = plotWidth, units = 'in')
ggsave('~/precisionEighteenStream.png', plot = normalAnalysis$Precision$Plot,height = plotHeight, width = plotWidth, units = 'in')
ggsave('~/efficacyEighteenStream.png', plot = normalAnalysis$Efficacy$Plot,height = plotHeight, width = plotWidth, units = 'in')


bestFittingResults <- analyses(
  params = paramsDF,
  bestFitting = T,
  modelKind = 'Normal'
)


randPart <- allData %>% pull(ID) %>% unique() %>% sample(size = 3)

randPartData <- allData %>% filter(ID %in% randPart)
randPartDensities <- densities %>% filter(ID %in% randPart)

randomParticipantPlot <- ggplot()+
  geom_histogram(data = randPartData, aes(x = SPE), binwidth = 1)+
  geom_line(data = randPartDensities, aes(x = SPE, y = density*50*80, colour = model), size = 1)+
  facet_grid(cols = vars(condition), rows = vars(ID))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  labs(y = 'Count')+
  theme_apa(base_size = 30)

randomParticipantPlot

ggsave('~/gitCode/nStream/manuscripts_etc/Manuscript Figures/randomParticipantPlot18.png', randomParticipantPlot, width = 16, height = 9, units = 'in')

dummy <- expand.grid(condition = ordered(c(2,6,18), levels = c('2', '6', '18'), labels = c('2', '6', '18')),
                     variable = factor(c('efficacy'), levels = c('efficacy', 'latency', 'precision'), labels = c('Efficacy', 'Latency', 'Precision')),
                     value = c(0,.5,1))

paramFacet <- paramsDF %>% filter(model == 'Normal')  %>% 
  melt(measure.vars = c('efficacy', 'latency', 'precision')) %>% 
  mutate(condition = ordered(condition, levels = c('2', '6', '18'))) %>%
  mutate(variable = factor(variable, levels = c('efficacy', 'latency', 'precision'), labels = c('Efficacy', 'Latency', 'Precision'))) %>%
  ggplot(aes(x = condition, y= value))+
  geom_point(alpha=.5, colour = '#dca951', size = 2)+
  geom_blank(data = dummy)+
  stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), size = 5, colour = '#23375f')+
  stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
  facet_wrap(~variable, scales = 'free') + 
  geom_line(aes(group = participant),alpha = .3)+
  labs(x = 'Number of Streams', y = 'Estimate')+
  theme_apa(base_size = 20)

ggsave('~/paramFacet18.png', paramFacet, width = 16, height = 9, units = 'in')
