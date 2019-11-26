ExpTwoDemoSONA <- read.csv('~/gitCode/nStream/Analysis/anonDemographic18StreamSONA.csv')
ExpTwo <- read.csv('~/gitCode/nStream/Analysis/allErrors18Streams.txt')

IDs <- gsub('[0-9]*', '', unique(ExpTwo$ID))

calc_age <- function(birthDate, refDate) { #Taken from https://gist.github.com/mmparker/7254445
  
  birthDate <- strptime(birthDate, format = '%d/%m/%y')
  refDate <- strptime(refDate, format = '%d/%m/%Y')
  
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}



ExpTwoDemoSONA$charID <- toupper(substr(ExpTwoDemoSONA$UniKey, 1,2))
ExpTwoDemoSONA <- ExpTwoDemoSONA[,3:5]

ExpTwoDemoSONA$Age <- sapply(ExpTwoDemoSONA$DOB,  function(x) calc_age(x, '1/10/2018'))
ExpTwoDemoSONA[!ExpTwoDemoSONA$charID %in% IDs,]


#AH is AW

IDs[IDs=='AH'] <- 'AW'

ExpTwoDemoSONA <- ExpTwoDemoSONA[ExpTwoDemoSONA$charID %in% IDs,]

ExpTwoDemoGrad <- data.frame(
  Gender = 'X',
  charID = c('JG', 'JC', 'WN', 'RN'),
  Age = -999
)

ExpTwoDemoGrad$Gender <- c('F','F','M','M')
ExpTwoDemoGrad$Age <- c(32, 26, 26,30)

ExpTwoDemo <- rbind(ExpTwoDemoGrad, ExpTwoDemoSONA[,-2])

