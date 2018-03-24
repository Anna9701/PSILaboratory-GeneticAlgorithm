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
  for (index in 0:populationSize) {
    number <- round(runif(1) * 126) + 1
    binaryNumber <- decimal2binary(number, 8)
    populationMatrix <- rbind(populationMatrix, binaryNumber, deparse.level = 0)
  }
  return(populationMatrix)
}


startupPopulationSize <- randStartupPopulationSize()
print(startupPopulationSize)
nBits <- 8
chromosomsMatrix <- createStartupPopulation(startupPopulationSize, nBits)
print(chromosomsMatrix)

