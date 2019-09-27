rm(list=ls())
library(ggplot2)
devtools::load_all('~/gitCode/mixRSVP/')

###Guessing Distribution bounds###
minSP <- 7
maxSP <- 11
targetSP <- rep(minSP:maxSP, each = 20)

minSPE <- 1 - maxSP
maxSPE <- 24 - minSP


SPE <- seq(minSPE, maxSPE, .1)

###Guessing Probs###
guessingDist <- createGuessingDistribution(minSPE, maxSPE, targetSP,24)
guessingDist <- guessingDist/sum(guessingDist)

guessingDist <- data.frame(SPE=minSPE:maxSPE, prob = guessingDist)


##Bin the SPEs because guessing is the same within a bin##
guessingSPE <- c((min(SPE)):max(SPE))[cut(SPE,breaks = (min(SPE)):max(SPE) , include.lowest = T, labels = F)]

##assign a probability to an SPE based on its bin##
guessingFreqs <- sapply(guessingSPE,FUN = function(x){
if(x %in% guessingDist$SPE){
  guessingDist$prob[guessingDist$SPE == x]
  } else {
      0
  }
}
)

shape = 3
scale = .6

binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,shape,scale, 'Gamma')
binProb <- binAreasEfficacy/sum(binAreasEfficacy)

mixtureFreq = binProb*.75 + guessingFreqs * .25

data <- data.frame(SPE = SPE,
                 mixture = mixtureFreq,
                 guessing = guessingFreqs*.25,
                 efficacious = binProb*.75,
                 model = 'Gamma',
                 stringsAsFactors = F)


binAreasEfficacy<- sapply(SPE, areaOfGaussianBin,   .1,.8,.8,'Normal')
binProb <- binAreasEfficacy/sum(binAreasEfficacy)

mixtureFreq = binProb*.75 + guessingFreqs * .25

dataNorm <- data.frame(SPE = SPE,
                   mixture = mixtureFreq,
                   guessing = guessingFreqs*.25,
                   efficacious = binProb*.75,
                   model = 'Normal',
                   stringsAsFactors = F)

data = rbind(dataNorm, data)

SPESamples <- data.frame(SPE = numeric(300), model = rep(c('Normal', 'Gamma'), each = 150))


mixturePlots <- ggplot(data, aes(x = SPE))+
  geom_line(aes(y = mixture), linetype = 2)+
  geom_area(aes(y = guessing), alpha = .5)+
  geom_area(aes(y = efficacious), alpha = .7)+
  theme_apa(base_size = 15)+
  facet_wrap(~model)+
  labs(y = 'Density')

ggsave(plot = mixturePlots, filename = '~/gitCode/nStream/manuscripts_etc/Manuscript Figures/ExampleMixtures.png', width = 8, height = 4.5, units = "in")

