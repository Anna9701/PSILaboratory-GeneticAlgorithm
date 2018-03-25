# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
library(GA)
source('~/R/geneticAlgorithm/R/algorithmGeneticSources.R')

fitnessFunction <- function(x) 2*(x^2+1)

nBits <- 8
minNumber <- 1
maxNumber <- 127
maxPopulation <- 10

startupPopulationSize <- randStartupPopulationSize()
#print(startupPopulationSize)

chromosomsMatrix1 <- createStartupPopulation(startupPopulationSize, nBits)
#print(chromosomsMatrix1)

fintessRatesMatrix1 <- fitnessEvaluation(chromosomsMatrix1)
#print(fintessRatesMatrix1)

nextGenerationParents <- findNextGenerationParentsByChromosomsSelection(chromosomsMatrix1, fintessRatesMatrix1)
print(nextGenerationParents)

nextGeneration <- crossoverPopulation(nextGenerationParents)
print(nextGeneration)
