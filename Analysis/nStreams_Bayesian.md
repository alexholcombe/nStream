nStreams Analysis
================
Charlie Ludowici
5/18/2017

``` r
library(ggplot2)
library(reshape2)
library(papaja)
library(BayesFactor)
```

    ## Loading required package: coda

    ## Loading required package: Matrix

    ## ************
    ## Welcome to BayesFactor 0.9.12-4.2. If you have questions, please contact Richard Morey (richarddmorey@gmail.com).
    ## 
    ## Type BFManual() to open the manual.
    ## ************

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)
library(truncnorm)
theme_set(theme_apa(base_size = 15) ) 

nParticipants = 13

inclusionBF <- function(model, variable){
  
  ###https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp###

  
  priorProbs <- model %>% newPriorOdds() %>% `*`(model) %>% as.BFprobability() %>% as.vector() 
  
  theseNames <- names(priorProbs)
  nProbs <- 1:length(priorProbs)
  variableMatches <- grep(variable, theseNames)
  
  if(grepl(':', variable)){
    subordinateVariables <- variable %>% strsplit(':') %>% unlist()
    
    thisRegex <- paste0(subordinateVariables,collapse = '.*\\+.*')
    
    subordinateEffects <- grep(thisRegex, theseNames, perl = T)
    subordinateEffects <- subordinateEffects[!subordinateEffects %in% variableMatches]
    
    
    sum(priorProbs[variableMatches])/sum(priorProbs[subordinateEffects])
  } else {
    interactionMatches <- grep(paste0(variable,'(?=:)|(?<=:)',variable), theseNames, perl = T)
    
    variableMainEffects <- variableMatches[!variableMatches %in% interactionMatches]
    
    
    otherMainEffects <- nProbs[!nProbs %in% c(variableMainEffects,interactionMatches)]
    
    
    sum(priorProbs[variableMainEffects])/sum(priorProbs[otherMainEffects])
  }
}

savePlots <- T

posterior <- function(t, N1, N2=NULL, delta, lo=-Inf, hi = Inf,
                      priorMean=0,priorSD=1) {
        N = ifelse(is.null(N2), N1, N1*N2/(N1+N2))
        df  = ifelse(is.null(N2), N1 - 1, N1 + N2 - 2)
        
        #prior and likelihood
        #prior <- function(delta) dnorm(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        prior <- function(delta) dcauchy(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        K=1/integrate(prior,lower=lo,upper=hi)[[1]]
        f=function(delta) K*prior(delta)
        
        #(The as.integer bits above just provide bounds for the prior if you want them)
      
        likelihood <- function(delta) dt(t, df, delta*sqrt(N))
        
        #marginal likelihood
        marginal <- integrate(function(x) f(x)*likelihood(x), lo, hi)[[1]]
        
        #posterior
        post <- function(x) f(x)*likelihood(x) / marginal
        return(post(delta))
}
```

``` r
null = 0

allErrors <- read.table('allErrors.txt', sep = '\t', header = T)

LatencyNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_LatencyNorm.csv')
LatencyTNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_LatencyTruncNorm.csv')

PrecisionNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_PrecisionNorm.csv')
PrecisionTNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_PrecisionTruncNorm.csv')

EfficacyNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_EfficacyNorm.csv')
EfficacyTNorm <- read.csv('../modelOutput/CSV/TGRSVP_Exp2_EfficacyTruncNorm.csv')
```

Model fits
==========

``` r
BFs <- read.csv('../modelOutput/BF_ByParticipant.csv', header = T)
BFs %>% filter(Group == 'twoStreams') %>% pull(BF) %>% sort()
```

    ##  [1] 6.920463e-01 8.866223e-01 1.851372e+00 2.025142e+00 2.545393e+00
    ##  [6] 9.376330e+00 2.340434e+01 2.975602e+01 7.944392e+01 5.499396e+02
    ## [11] 2.483506e+04 6.808310e+04 7.016859e+04

``` r
BFs %<>% mutate(preferredModel = '') %>% mutate(preferredModel = replace(preferredModel, BF>3, 'Buffering')) %>% mutate(preferredModel = replace(preferredModel,BF<(1/3), 'Attention')) %>% mutate(preferredModel = replace(preferredModel, BF<3 & BF>(1/3), 'Neither'))


allEfficacy <- rbind(EfficacyNorm,EfficacyTNorm)
allEfficacy$participantN <- factor(rep(1:13, times =4))

efficacyModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allEfficacy, whichRandom = 'participantN')

efficacyModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in efficacyModelInclusionBF$factor){
  efficacyModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(efficacyModelBF, thisFactor)))
  print(inclusionBF(efficacyModelBF, thisFactor))
}
```

    ## [1] 0.6943305
    ## [1] 6.942121
    ## [1] 0.4810378

``` r
knitr::kable(efficacyModelInclusionBF)
```

| factor      |         BF|
|:------------|----------:|
| Group       |  0.6943305|
| Model       |  6.9421206|
| Group:Model |  0.4810378|

``` r
allLatency <- rbind(LatencyNorm,LatencyTNorm)
allLatency$participantN <- factor(rep(1:nParticipants, times =4))

latencyModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allLatency, whichRandom = 'participantN')

latencyModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in latencyModelInclusionBF$factor){
  latencyModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(latencyModelBF, thisFactor)))
  print(inclusionBF(latencyModelBF, thisFactor))
}
```

    ## [1] 24056.9
    ## [1] 3.127051
    ## [1] 0.4885946

``` r
knitr::kable(latencyModelInclusionBF)
```

| factor      |            BF|
|:------------|-------------:|
| Group       |  2.405690e+04|
| Model       |  3.127051e+00|
| Group:Model |  4.885946e-01|

``` r
allPrecision <- rbind(PrecisionNorm,PrecisionTNorm)
allPrecision$participantN <- factor(rep(1:nParticipants, times =4))

precisionModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allPrecision, whichRandom = 'participantN')

precisionModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in precisionModelInclusionBF$factor){
  precisionModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(precisionModelBF, thisFactor)))
  print(inclusionBF(precisionModelBF, thisFactor))
}
```

    ## [1] 0.2770782
    ## [1] 0.4891999
    ## [1] 0.5431911

``` r
knitr::kable(precisionModelInclusionBF)
```

| factor      |         BF|
|:------------|----------:|
| Group       |  0.2770782|
| Model       |  0.4891999|
| Group:Model |  0.5431911|

Latency Analyses
================

``` r
latencyTwo <- LatencyNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
latencyEight <- LatencyNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

latencyTFreq  <- t.test(x = latencyTwo,
                        y = latencyEight, 
                        paired = T)

tLatency <- latencyTFreq$statistic[[1]]

N1<- nParticipants
N2<- nParticipants

priorMean = null
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(null,priorMean,priorSD) / posterior(tLatency, N1, delta=null,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(null,priorMean,priorSD) ) / posterior(tLatency, N1, delta=null, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 24.05721

``` r
BFplus
```

    ## [1] 0.08369593

``` r
delta  <- seq(-2, 4, .01)

posteriorAndPriorDF <- data.frame(
  delta = delta, 
  posterior = posterior(tLatency,N1,delta=delta, priorMean=priorMean,priorSD=priorSD), 
  prior = dcauchy(delta, priorMean,priorSD))

posteriorModeLatency <- optimize(function(delta) posterior(tLatency, N1, delta=delta, priorMean=priorMean, priorSD=priorSD), 
                                 interval=c(-4,4),
                                 maximum = T)[[1]]

#This would only work for normal, we use Cauchy!
#credibleIntervalDensityLower <- mean(posteriorAndPriorDF$posterior)-sd(posteriorAndPriorDF$posterior)*1.96
#credibleIntervalDensityUpper <- mean(posteriorAndPriorDF$posterior)+sd(posteriorAndPriorDF$posterior)*1.96


LatencyNormPlot <- ggplot(LatencyNorm,aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour=Participant), size = 3)+
  scale_colour_brewer(palette = 'Spectral')+
  labs(x='Condition',y='Estimate (ms)',title='Latency')+
  theme(plot.title = element_text(hjust=.5))

show(LatencyNormPlot)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
wideFormatLatency <- dcast(data=LatencyNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
LatencyNormScatter <- ggplot(wideFormatLatency, aes(x=twoStreams, y=eightStreams))+
  geom_point(size = 4, aes(colour=ordered(1:nParticipants)))+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(20,120), y=c(20,120))+
  labs(title='Latency Estimates (ms)', x = 'Two Streams', y='Eight Streams')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=100, y=45, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x =1:100, y=37, label = paste0('Effect size = ', round(posteriorModeLatency,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(LatencyNormScatter)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
LatencyNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Latency Effect Size')

show(LatencyNormBayesPlot)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-4-3.png)

Precision Analysis
==================

``` r
precisionTwo <- PrecisionNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
precisionEight <- PrecisionNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

precisionTFreq  <- t.test(x = precisionTwo,
                        y = precisionEight, 
                        paired = T)

tPrecision <- precisionTFreq$statistic[[1]]

t  <- tPrecision
N1<- nParticipants
N2<- nParticipants

priorMean =0
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(0,priorMean,priorSD) / posterior(tPrecision, N1, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(0,priorMean,priorSD) ) / posterior(tPrecision, N1, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 0.3467801

``` r
BFplus
```

    ## [1] 0.5143989

``` r
delta  <- seq(-4, 2, .01)

posteriorModePrecision <- optimize(function(delta) posterior(tPrecision, N1, delta=delta,priorMean=priorMean,priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]

posteriorAndPriorDF <- data.frame(delta = delta, 
                                  posterior = posterior(tPrecision,N1,delta=delta,
                                                    priorMean=priorMean,priorSD=priorSD), 
                                  prior = dcauchy(delta, priorMean,priorSD))

posteriorModePrecision <- optimize(function(delta) posterior(tPrecision, N1, delta=delta, priorMean=priorMean, priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]


PrecisionNormPlot <- ggplot(PrecisionNorm,aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour=Participant),alpha=.8, size = 3)+
  scale_colour_brewer(palette = 'Spectral')+
  labs(x='Condition',y='Estimate (ms)',title='Precision')+
  theme(plot.title = element_text(hjust=.5))

show(PrecisionNormPlot)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
wideFormatPrecision <- dcast(data=PrecisionNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
PrecisionNormScatter <- ggplot(wideFormatPrecision, aes(x=twoStreams, y=eightStreams, colour=ordered(1:nParticipants)))+
  geom_point(size = 4)+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(40,100), y=c(40,100))+
  labs(title='Precision Estimates (ms)')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=90, y=70, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x = 90, y=66, label = paste0('Effect size = ', round(posteriorModePrecision,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(PrecisionNormScatter)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
PrecisionNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Precision Effect Size')
```

Efficacy Analysis
=================

``` r
efficacyTwo <- EfficacyNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
efficacyEight <- EfficacyNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

efficacyTFreq  <- t.test(x = efficacyTwo,
                        y = efficacyEight, 
                        paired = T)

tEfficacy <- efficacyTFreq$statistic[[1]]
N1<- nParticipants
N2<- nParticipants

priorMean =0
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(0,priorMean,priorSD) / posterior(tEfficacy, N1, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(0,priorMean,priorSD) ) / posterior(tEfficacy, N1, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 0.7606536

``` r
BFplus
```

    ## [1] 1.395237

``` r
delta  <- seq(-2, 4, .01)

posteriorAndPriorDF <- data.frame(delta = delta, posterior = posterior(tEfficacy ,N1,delta=delta,
                                                                       priorMean=priorMean,priorSD=priorSD), prior = dcauchy(delta, priorMean,priorSD))

posteriorModeEfficacy <- optimize(function(delta) posterior(tEfficacy, N1, delta=delta,priorMean=priorMean,priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]


EfficacyNormPlot <- ggplot(EfficacyNorm, aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour = Participant), size = 3)+
  labs(x='Condition',y='Estimate',title='Efficacy')+
  theme(plot.title = element_text(hjust=.5))+
  scale_colour_brewer(palette = 'Spectral')

show(EfficacyNormPlot)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
wideFormatEfficacy <- dcast(data=EfficacyNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
EfficacyNormScatter <- ggplot(wideFormatEfficacy, aes(x=twoStreams, y=eightStreams, colour=ordered(1:nParticipants)))+
  geom_point(size = 4)+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(0,1), y=c(0,1))+
  labs(title='Efficacy Estimates [1 - P(Guess)]', y = 'Eight Streams', y='Eight Streams')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=.8, y=.45, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x = .8, y=.37, label = paste0('Effect size = ', round(posteriorModeEfficacy,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(EfficacyNormScatter)
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
EfficacyNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Efficacy Effect Size')
```

``` r
propBeforeCue <- expand.grid(
  Participant = unique(LatencyNorm$Participant),
  Group = unique(LatencyNorm$Group),
  Proportion = -999,
  nTrialsBeforeCue = -999
)

for(thisParticipant in unique(LatencyNorm$Participant)){
  for(thisCondition in unique(LatencyNorm$Group)){
    
    thisNormLatency <- LatencyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    thisNormPrecision <- PrecisionNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    
    thisNormEfficacy <- EfficacyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    
    thisNTrials <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject) %>% nrow 
    
    thisProportionBeforeCue <- pnorm(0, thisNormLatency, thisNormPrecision)
    thisNEfficaciousBeforeCue <- thisNTrials * thisNormEfficacy * thisProportionBeforeCue
    
    propBeforeCue %<>% mutate(Proportion = replace(Proportion, Participant==thisParticipant & Group == thisCondition, thisProportionBeforeCue))
   
    propBeforeCue %<>% mutate(nTrialsBeforeCue = replace(nTrialsBeforeCue, Participant==thisParticipant & Group == thisCondition, thisNEfficaciousBeforeCue)) 
  }
}

propBeforeCue %>% group_by(Group) %>% summarise(proportion = mean(Proportion))
```

    ## # A tibble: 2 x 2
    ##   Group        proportion
    ##   <fct>             <dbl>
    ## 1 eightStreams      0.124
    ## 2 twoStreams        0.201

``` r
knitr::kable(propBeforeCue)
```

| Participant | Group        |  Proportion|  nTrialsBeforeCue|
|:------------|:-------------|-----------:|-----------------:|
| AB14        | twoStreams   |   0.3460529|         31.801296|
| BN12        | twoStreams   |   0.1666919|         14.784519|
| EF6         | twoStreams   |   0.0642611|          5.051830|
| FC21        | twoStreams   |   0.2505643|         16.345941|
| GR13        | twoStreams   |   0.1552371|         11.357329|
| HT5         | twoStreams   |   0.1071650|          7.386060|
| JW8         | twoStreams   |   0.2965125|         21.637261|
| KH18        | twoStreams   |   0.0483285|          2.980437|
| MC10        | twoStreams   |   0.3242443|         15.030472|
| NC21        | twoStreams   |   0.1243903|          8.272451|
| RL11        | twoStreams   |   0.3584030|         27.728997|
| SH20        | twoStreams   |   0.1414313|         12.250004|
| ZZ9         | twoStreams   |   0.2358338|         16.266732|
| AB14        | eightStreams |   0.2378881|         21.243649|
| BN12        | eightStreams |   0.1587756|         14.318316|
| EF6         | eightStreams |   0.0481324|          2.960645|
| FC21        | eightStreams |   0.1606586|          9.159051|
| GR13        | eightStreams |   0.2029877|         17.811321|
| HT5         | eightStreams |   0.1026912|          6.176671|
| JW8         | eightStreams |   0.0636077|          4.512844|
| KH18        | eightStreams |   0.0600685|          4.159071|
| MC10        | eightStreams |   0.0503799|          2.194186|
| NC21        | eightStreams |   0.0965501|          6.175869|
| RL11        | eightStreams |   0.1893389|         16.756153|
| SH20        | eightStreams |   0.1188641|         11.228001|
| ZZ9         | eightStreams |   0.1280490|          7.057357|

``` r
ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='twoStreams'],
        y = propBeforeCue$Proportion[propBeforeCue$Group=='eightStreams'],
        data = propBeforeCue,
        paired = T)
```

    ## Bayes factor analysis
    ## --------------
    ## [1] Alt., r=0.707 : 4.081142 ±0%
    ## 
    ## Against denominator:
    ##   Null, mu = 0 
    ## ---
    ## Bayes factor type: BFoneSample, JZS

``` r
x <- ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='eightStreams'],
        data = propBeforeCue,
        mu = 0) 

ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='twoStreams'],
        data = propBeforeCue, mu = 0)
```

    ## Bayes factor analysis
    ## --------------
    ## [1] Alt., r=0.707 : 1222.487 ±0%
    ## 
    ## Against denominator:
    ##   Null, mu = 0 
    ## ---
    ## Bayes factor type: BFoneSample, JZS

``` r
# 
# EfficacyNorm$Parameter <- as.character('Efficacy')
# LatencyNorm$Parameter <- as.character('Latency')
# PrecisionNorm$Parameter <- as.character('Precision')
# 
# allParams <- rbind(EfficacyNorm,LatencyNorm,PrecisionNorm)
# 
# allParams <- melt(allParams, measure.vars = c('twoStreams','eightStreams'), variable.name = 'Condition',value.name = 'Estimate')
# 
# paramBar <- ggplot(allParams[!allParams$Parameter=='Efficacy',], aes(x=Parameter,y=Estimate, fill = Condition))+
#   stat_summary(geom='bar', fun.y = mean, position = position_dodge(.9))+
#   stat_summary(geom='errorbar', fun.data=mean_se, position = position_dodge(.9), width = .3)+
#   scale_fill_brewer(palette = 'Spectral')
# 
# paramBar
# 
# predictions <- data.frame(twoStreams = rnorm(1000, LatencyNorm$twoStreams[7], PrecisionNorm$twoStreams[7]), eightStreams = rnorm(1000, LatencyNorm$eightStreams[7], PrecisionNorm$eightStreams[7]))
# 
# predictions <- melt(predictions, measure.vars = c('twoStreams', 'eightStreams'), variable.name = 'Condition', value.name = 'response')
# 
# meanLatencyTwo <- mean(LatencyNorm$twoStreams)
# meanLatencyEight <- mean(LatencyNorm$eightStreams)
# 
# meanPrecisionTwo <- mean(PrecisionNorm$twoStreams)
# meanPrecisionEight <- mean(PrecisionNorm$eightStreams)
# 
# 
# predictionPlot <- ggplot()+
#   stat_function(data=data.frame(x=c(-400:400)/83.33), aes(x, fill = 'Eight Streams'), fun = dnorm, args = list(mean = meanLatencyEight/83.33, sd = meanPrecisionEight/83.33), geom='area', alpha = .9)+
#   stat_function(data=data.frame(x=c(-400:400)/83.33), aes(x, fill = 'Two Streams'), fun = dnorm, args = list(mean = meanLatencyTwo/83.33, sd = meanPrecisionTwo/83.33), geom='area', alpha = .9)+
#   scale_fill_manual(values=c('Two Streams' = '#628093', 'Eight Streams' = '#dca951'))+
#   scale_x_continuous(breaks = -3:4,limits = c(-3,4))+
#   labs(x='SPE', y=NULL, fill = 'Condition')
# 
# predictionPlot
# 
# 
# if(savePlots){
#   ggsave(PrecisionNormPlot, file = 'PrecisionNormViolin.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormPlot, file = 'LatencyNormViolin.png', height=15, width=20,units='cm')
#   ggsave(EfficacyNormPlot, file = 'EfficacyNormViolin.png', height=15, width=20,units='cm')
#   
#   ggsave(PrecisionNormScatter, file = 'PrecisionNormScatter.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormScatter, file = 'LatencyNormScatter.png', height=15, width=20,units='cm')
#   ggsave(EfficacyNormScatter, file = 'EfficacyNormScatter.png', height=15, width=20,units='cm')
#   
#   
#   ggsave(EfficacyNormBayesPlot, file = 'EfficacyNormEffectSize.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormBayesPlot, file = 'LatencyNormEffectSize.png', height=15, width=20,units='cm')
#     ggsave(PrecisionNormBayesPlot, file = 'PrecisionNormEffectSize.png', height=15, width=20,units='cm')
# }
```

``` r
for(thisParticipant in unique(allErrors$ID)){
  densities <- data.frame(SPE = numeric(0), condition = character(0), normDensity = numeric(0), tNormDensity = numeric(0))
  for(thisCondition in unique(allErrors$condition)){
    
    thisPreferredModel <- BFs %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(preferredModel)
    
    if(thisPreferredModel == 'Buffering'){
      colours <- c('green','red')
    } else if(thisPreferredModel=='Attention'){
      colours <- c('red','green')
    } else if(thisPreferredModel == 'Neither'){
      colours <- c('red','red')
    }
    
    thisNormLatency <- LatencyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    thisNormPrecision <- PrecisionNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    
    thisTNormLatency <- LatencyTNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    thisTNormPrecision <- PrecisionTNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    
    theseErrors <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
    
    #print(paste0('Participant: ', thisParticipant, '. Condition: ', thisCondition,'. N = ', nrow(theseErrors)))
    minError <- theseErrors %>% pull(error) %>% min
    maxError <- theseErrors %>% pull(error) %>% max
    thisRange <- seq(minError,maxError,.1)
    
    theseDensities <- data.frame(SPE = thisRange, 
                                 condition = rep(thisCondition, times = length(thisRange)),
                                 normDensity = dnorm(thisRange, thisNormLatency, thisNormPrecision), 
                                 tNormDensity = dtruncnorm(thisRange, a = thisTNormLatency-thisTNormPrecision, thisTNormLatency, thisTNormPrecision, b = Inf))
    
    densities <- rbind(densities, theseDensities)
    
    if(any(is.nan(theseDensities$density))){
      print(theseDensities)
    }
    
  
    # thisFileName <- paste0('modelOutput/Plots/',thisCondition,'/',thisRing,'/',thisParticipant,'-',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.png')
    # ggsave(filename = thisFileName, thisPlot,width = 16, height = 9)
  }
  errors <- allErrors %>% filter(ID == thisParticipant)
  thisPlot <- ggplot(errors, aes(x=error))+
      geom_histogram(binwidth = 1)+
      geom_line(data = densities, aes(x = SPE, y=tNormDensity*150), colour = colours[1])+ #scale density to histogram with density * N * binwidth
      geom_line(data = densities, aes(x = SPE, y=normDensity*150), colour = colours[2])+
      scale_y_continuous(sec.axis = sec_axis(~./nrow(theseErrors), name = 'Density'))+
      labs(y = 'Frequency', title = thisParticipant)+
    facet_wrap(~condition)
    
    show(thisPlot)
}
```

![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-1.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-2.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-3.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-4.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-5.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-6.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-7.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-8.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-9.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-10.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-11.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-12.png)![](nStreams_Bayesian_files/figure-markdown_github/unnamed-chunk-9-13.png)
