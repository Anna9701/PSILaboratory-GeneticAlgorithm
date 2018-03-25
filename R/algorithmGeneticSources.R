library(GA)

randStartupPopulationSize <- function() {
  numbersToRand <- 1
  population <- round(runif(numbersToRand, min = 1, max = maxPopulation))
  if (population %% 2 == 1) {
    population <- population + 1
  }
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

crossoverPopulation <- function(nextGenerationParents) {
  nextGeneration <- nextGenerationParents[sample(1:startupPopulationSize),]
  numberOfPairs <- nrow(nextGeneration) / 2
  index <- 1
  for (iterator in 1:numberOfPairs) {
    pair1 <- nextGeneration[index,]
    pair2 <- nextGeneration[index+1,]
    locus <- round(runif(1, min = 1, max = 8))
    print(locus)
    temp1 <- c(8)
    temp2 <- c(8)
    temp1[1:locus] <- pair1[1:locus]
    temp2[1:locus] <- pair2[1:locus]
    temp1[(locus+1):8] <- pair2[(locus+1):8]
    temp2[(locus+1):8] <- pair1[(locus+1):8]
    nextGeneration[index + 1,] <- temp2
    nextGeneration[index,] <- temp1
    index <- index + 2
  }
  return(nextGeneration)
}

