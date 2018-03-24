# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
library(GA)

fitnessFunction <- function(x) 2*(x^2+1)

randStartupPopulationSize <- function() {
  numbersToRand <- 1
  population <- round(runif(numbersToRand, min = 1, max = maxPopulation))
  return (population)
}

createStartupPopulation <- function(populationSize, nBits) {
  numbersToRand <- 1
  populationMatrix <- matrix(nrow = 0, ncol = nBits)
  for (index in 1:populationSize) {
    number <- round(runif(numbersToRand, min = minNumber, max = maxNumber))
    binaryNumber <- decimal2binary(number, nBits)
    populationMatrix <- rbind(populationMatrix, binaryNumber, deparse.level = 0)
  }
  return(populationMatrix)
}

fitnessEvaluation <- function(chromosomsMatrix) {
  fitnessRates <- matrix(nrow = 0, ncol = 1)
  for (index in 1:nrow(chromosomsMatrix)) {
    number <- binary2decimal(chromosomsMatrix[index,])
    fitnessRates <- rbind(fitnessRates, fitnessFunction(number), deparse.level = 0)
  }
  return (fitnessRates)
}

findNextGenerationParentsByChromosomsSelection <- function(chromosomsMatrix, fitnessRates) {
  probabilityMatrix <- matrix(nrow = 0, ncol = 1)
  sumAllProbabilities <- 0
  nextGenerationParents <- matrix(nrow = 0, ncol = nBits)

  for (index in 1:nrow(fitnessRates)) {
    sumAllProbabilities <- sumAllProbabilities + fitnessRates[index]
  }

  for (index in 1:nrow(fitnessRates)) {
    probability <- fitnessRates[index] / sumAllProbabilities
    probabilityMatrix <- rbind(probabilityMatrix, probability, deparse.level = 0)
  }

  for (indexRule in 1:nrow(fitnessRates)) {
    randValueToCompare = round(runif(1, min = 0, max = sumAllProbabilities))
    index <- 1
    chromosomToCompareFitnessRate <- fitnessRates[index]
    while (chromosomToCompareFitnessRate < randValueToCompare && index < nrow(fitnessRates)) {
      index <- index + 1
      chromosomToCompareFitnessRate <- chromosomToCompareFitnessRate + fitnessRates[index]
    }
    nextGenerationParents <- rbind(nextGenerationParents, chromosomsMatrix[index,], deparse.level = 0)
  }

  return(nextGenerationParents)
}

nBits <- 8
minNumber <- 1
maxNumber <- 127
maxPopulation <- 100
startupPopulationSize <- randStartupPopulationSize()
print(startupPopulationSize)
chromosomsMatrix1 <- createStartupPopulation(startupPopulationSize, nBits)
print(chromosomsMatrix1)
fintessRatesMatrix1 <- fitnessEvaluation(chromosomsMatrix1)
print(fintessRatesMatrix1)
nextGenerationParents <- findNextGenerationParentsByChromosomsSelection(chromosomsMatrix1, fintessRatesMatrix1)
print(nextGenerationParents)
