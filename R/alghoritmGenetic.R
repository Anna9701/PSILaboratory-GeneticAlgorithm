# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
library(GA)

fitnessFunction <- function(x) 2*(x^2+1)

randStartupPopulationSize <- function() {
  maxPopulation <- 100
  population <- round(runif(1) * maxPopulation) + 1
  return (population)
}

createStartupPopulation <- function(populationSize, nBits) {
  populationMatrix <- matrix(nrow = 0, ncol = nBits)
  for (index in 1:populationSize) {
    number <- round(runif(1) * 126) + 1
    binaryNumber <- decimal2binary(number, 8)
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

nBits <- 8
startupPopulationSize <- randStartupPopulationSize()
print(startupPopulationSize)
chromosomsMatrix1 <- createStartupPopulation(startupPopulationSize, nBits)
print(chromosomsMatrix1)
fintessRatesMatrix1 <- fitnessEvaluation(chromosomsMatrix1)
print(fintessRatesMatrix1)
