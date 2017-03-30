setwd('~/gitcode/nStream/rawData/')

CB <- read.table('CBTest_24Mar2017_13-37.txt', header =T, sep='\t')

modelProbabilities <- data.frame(SPE = sort(unique(CB$responsePosRelative0)), empiricalProbs = NA, symmetricProbs = NA)

for(SPEValue in unique(CB$responsePosRelative0)){ #For each unique SPE. Calculate the emprical model's probabilty for that value, which is just the proportion of responses equal to it
  modelRow <- modelProbabilities$SPE==SPEValue
  empiricalProb <- length(which(CB$responsePosRelative0==SPEValue))/nrow(CB)
  modelProbabilities$empiricalProbs[modelRow] <- empiricalProb
}

#average both sides of the distribution of empirical probabilities to get a symmetric distribution
#Do I include the cue in both sides of the distribution?
preCueRows <- modelProbabilities$SPE<0
postCueRows <- modelProbabilities$SPE>0

preCueSPE <- modelProbabilities$empiricalProbs[preCueRows]
postCueSPE <- modelProbabilities$empiricalProbs[postCueRows]

oneSideOfTheSymmetricDistribution <- (preCueSPE + postCueSPE)/2 #This fails because the lengths of the pre-cue and post-cue vectors are not equal
