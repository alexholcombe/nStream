ExpOneDemo <- read.csv("~/gitCode/nStream/Analysis/anonDemographic8Stream.csv")

ExpOne <- read.table('~/gitCode/nStream/Analysis/allErrors.txt', sep = '\t', header = T)

IDs <- as.character(unique(ExpOne$ID))
charIDs <- gsub('[0-9]*', '', IDs)

calc_age <- function(birthDate, refDate) { #Taken from https://gist.github.com/mmparker/7254445
  
  birthDate <- strptime(birthDate, format = '%d/%m/%y')
  refDate <- strptime(refDate, format = '%d/%m/%Y')
  
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}

ExpOneDemo$Age <- sapply(ExpOneDemo$DOB, function(x) calc_age(x, '1/05/2017'))
ExpOneDemo$charID <- substr(ExpOneDemo$UniKey, start = 1, stop = 2)
ExpOneDemo$charID <- toupper(ExpOneDemo$charID)

mean_age <- mean(ExpOneDemo$Age)
sd_age <- sd(ExpOneDemo$Age)

nFemale <- sum(ExpOneDemo$Gender=='F')
