# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
source('~/R/geneticAlgorithm/R/algorithmGeneticSources.R')

fitnessFunction <- function(x) 2*(x^2+1)

nBits <- 7
minNumber <- 1
maxNumber <- 127
maxPopulation <- 100
numberOfGenerations <- 200

## zbadać kiedy zbieżność, 200 pokoleń. PK od 0.5, 0.6, 0.75, 0.9, 1
## PM od 0, 0.05, 0.10, 0.15, 0.20
## Po którym pokoleniu zbieżnosć
PK <- c(0.5, 0.6, 0.75, 0.9, 1)
PM <- c(0, 0.05, 0.10, 0.15, 0.20)

generationsResultMatrix <- matrix(nrow = 0, ncol = 5)

for (combinationProbability in PK) {
  resultVector <- c()
  for (mutationProbability in PM) {
    result <- basicGeneticAlgorithm(nBits, minNumber, maxNumber, maxPopulation, combinationProbability, mutationProbability, numberOfGenerations)
    resultVector <- cbind(resultVector, result, deparse.level = 0)
  }
  generationsResultMatrix <- rbind(generationsResultMatrix, resultVector, deparse.level = 0)
}

print(generationsResultMatrix)
for (index in 1:5) {
  plotTitle <- sprintf("plotFor%f.png", PK[index])
  png(plotTitle)
  plot(PM, generationsResultMatrix[index,])
  dev.off()
}

