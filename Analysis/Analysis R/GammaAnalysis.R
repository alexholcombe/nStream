analyses <- function(params, modelKind = NULL, bestFitting = FALSE, nIterations = 10000){
  params %<>% mutate(condition = ordered(condition, levels = c('Exogenous', 'Endogenous')))
  if(bestFitting){
    params %<>% filter(model == favouredModel)
    modelKind = 'Best Fitting'
  } else {
    params %<>% filter(model == modelKind)
    x = params %>% filter(condition == 'Endogenous') #For t-tests. If bestFitting is true then we get different Ns, so must use a linear model instead
    y = params %>% filter(condition == 'Exogenous')
  }
  
  results <- list()
  
  #######################
  ###Efficacy Analyses###
  #######################
  if(bestFitting){
    efficacyBFFullVSNull <- anovaBF(efficacy ~ condition + participant,
                                    data=params,
                                    whichRandom = 'participant', 
                                    progress = FALSE
    )
  } else {
    xEfficacy <- x %>% pull(efficacy)
    yEfficacy <- y %>% pull(efficacy)
    efficacyBFFullVSNull <- ttestBF(x = xEfficacy, y = yEfficacy, paired = T)
  }
  
  
  BayesFactorLabel <- efficacyBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  #Only evparticipantence for an effect of ring
  
  efficacyPlot = ggplot(params, aes(x=condition, y = efficacy))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), size = 4, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    labs(x = "Cue Type", y = "Efficacy")+
    scale_x_discrete(breaks = c('Exogenous', 'Endogenous'))+
    lims(y = c(0,1))+
    theme_apa()
  
  
  results[['Efficacy']] <- list(
    'BF' = efficacyBFFullVSNull,
    'Plot' = efficacyPlot
  )
  
  #######################
  ###Latency Analyses###
  ####################### 
  if(bestFitting){
    latencyBFFullVSNull <- anovaBF(latency ~ condition + participant,
                                   data=params,
                                   whichRandom = 'participant', 
                                   progress = FALSE
    )
  } else {
    xLatency <- x %>% pull(latency)
    yLatency <- y %>% pull(latency)
    latencyBFFullVSNull <- ttestBF(x = xLatency, y = yLatency, paired = T)
  }
  
  
  BayesFactorLabelOne <- latencyBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  
  latencyPlot <- ggplot(params, aes(x=condition, y = latency))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9),size = 4, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    labs(x = 'Number of Streams', y = 'Latency (ms)')+
    scale_x_discrete(breaks = c('Exogenous', 'Endogenous'))+
    lims(y = c(0, 300))+
    theme_apa()
  
  
  results[['Latency']] <- list(
    'BF' = latencyBFFullVSNull,
    'Plot' = latencyPlot 
  )
  
  ########################
  ###Precision Analyses###
  ########################
  if(bestFitting){
    precisionBFFullVSNull <- anovaBF(precision ~ condition + participant,
                                     data=params,
                                     whichRandom = 'participant', 
                                     progress = FALSE
    )
  } else {
    xPrecision <- x %>% pull(precision)
    yPrecision <- y %>% pull(precision)
    precisionBFFullVSNull <- ttestBF(x = xPrecision, y = yPrecision, paired = T)
  }
  
  BayesFactorLabelOne <- precisionBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  
  precisionPlot <- ggplot(params, aes(x=condition, y = precision))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#dca951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9),  size = 4, colour = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    labs(x = 'Number of Streams', y = 'Precision (ms)')+
    scale_x_discrete(breaks = c('Exogenous', 'Endogenous'))+
    theme_apa()
  
  results[['Precision']] <- list(
    'BF' =  precisionBFFullVSNull,
    'Plot' = precisionPlot 
  )
  
  results
}