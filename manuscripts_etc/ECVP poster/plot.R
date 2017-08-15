rm(list=ls())
library(ggplot2)
theme_set(papaja::theme_apa(base_size = 30) )

setwd('~/Dropbox/ECVP poster/')
nTrials <- 1000
efficacy = .65
mean = 40
sd = 72
nonGuessing <- round(rnorm(efficacy*nTrials, mean,sd)/83.25)
guessing <- round(runif((1-efficacy)*nTrials, -12, 11))
nonGuessingDensity <- dnorm(nonGuessing, mean,sd)/83.25
guessingDensity <- dunif(guessing, -12, 11)

logSPEs <- rlnorm(efficacy*nTrials, 150,50)
nonGuessingLogDensity <- dlnorm(rlnorm(efficacy*nTrials, 150,50), 150,50)
guessingLogDensity <- dunif(guessing, -12, 11)



SPE <- c(nonGuessing, guessing)
densities <- c(nonGuessingDensity, guessingDensity)
logdensities <- c(nonGuessingLogDensity, guessingLogDensity)
trial <- c(rep('nonGuessing', times=nTrials*efficacy), rep('Guessing', times=nTrials*(1-efficacy)))


data <- data.frame(SPEs=SPE, trial = trial, densities = densities, logdensities = logdensities)


theseLetters <- c(rep(NA, times=(24-2)/2),"P","R","T",rep(NA, times=(24-3)/2))
theseLetters[c(1:11,15:24)] <- sample(LETTERS[!LETTERS %in% c(theseLetters,"C","W")], 24-3)

cuePos <- which(theseLetters == 'R')

Idxs <- (1:24)-cuePos

xLabels <- paste(-12:11, theseLetters, sep='\n')

labelFace <- c()
labelSize <- c()

for(i in -12:11){
  labelFace <- c(labelFace, ifelse(i>=(-1) & i<=1, 'bold','plain' ))
  labelSize <- c(labelSize, ifelse(i>=(-1) & i<=1, 28,25 ))
}

plot1 <- ggplot(data, aes(x=SPEs))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks=-12:11, labels=xLabels)+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  labs(x = 'Temporal Error', y='Count', title = NULL)+
  theme(
    axis.text.x = 
      element_text(face = labelFace, size = labelSize),
    title = element_text(family = 'Arial'),
    plot.title = element_text(hjust = .5))
  

plot1

plot2 <- ggplot(data, aes(x=SPEs))+
  stat_function(fun=dnorm,
                aes(colour = 'Non-Guesses', 
                fill = 'Non-Guesses'),
                geom = 'density', 
                n=10000, 
                args = list(mean = mean/82.35, sd=sd/82.35))+
  stat_function(fun = dunif,
                geom='density',
                n=10000, 
                alpha = .6, 
                aes(colour = 'Guesses', 
                fill = 'Guesses'),
                args = list(min = -12, max = 11))+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks=-12:11, labels=xLabels)+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  labs(y = NULL)+
  scale_fill_manual(name = 'Component', values = c("Guesses" = '#0D7526', "Non-Guesses"= '#F8AD3F'))+
  scale_colour_manual(name = 'Component',values = c("Guesses" = '#0D7526', "Non-Guesses"= '#F8AD3F'))+
  theme(
    axis.text.x = 
      element_text(face = labelFace, size = labelSize),
    title = element_text(family = 'Arial'),
    plot.title = element_text(hjust = .5))

plot2


plot3 <- ggplot(data, aes(x=SPEs))+
  stat_function(fun=dlnorm,
                aes(colour = 'Non-Guesses', 
                fill = 'Non-Guesses'),
                geom = 'density', 
                n=10000, 
                args = list(meanlog = log(1.5), sdlog = log(1.5)))+
  stat_function(fun = dunif,
                geom='density',
                n=10000, 
                alpha = .6, 
                aes(colour = 'Guesses', 
                fill = 'Guesses'),
                args = list(min = -12, max = 11))+
  scale_fill_manual(name = 'Component', values = c("Guesses" = '#0D7526', "Non-Guesses"= '#F8AD3F'))+
  scale_colour_manual(name = 'Component',values = c("Guesses" = '#0D7526', "Non-Guesses"= '#F8AD3F'))+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks=-12:11, labels=xLabels)+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  labs(y = NULL)+
  theme(
    axis.text.x = 
      element_text(face = labelFace, size = labelSize),
    title = element_text(family = 'Arial'),
    plot.title = element_text(hjust = .5))

plot3

ggsave(plot1, filename = 'simulatedError.png',  height = 9, width = 16)
ggsave(plot2,filename = 'normDensity.png', height = 9, width = 16)
ggsave(plot3,filename = 'logNormDensity.png', height = 9, width = 16)

