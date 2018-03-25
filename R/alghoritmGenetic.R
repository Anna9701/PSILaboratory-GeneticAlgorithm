# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
library(GA)
source('~/R/geneticAlgorithm/R/algorithmGeneticSources.R')

fitnessFunction <- function(x) 2*(x^2+1)

nBits <- 8
minNumber <- 1
maxNumber <- 127
maxPopulation <- 10
PK <- 0.75
PM <- 1

startupPopulationSize <- randStartupPopulationSize()
chromosomsMatrix1 <- createStartupPopulation(startupPopulationSize, nBits)
fintessRatesMatrix1 <- fitnessEvaluation(chromosomsMatrix1)
nextGenerationParents <- findNextGenerationParentsByChromosomsSelection(chromosomsMatrix1, fintessRatesMatrix1)
nextGeneration <- crossoverPopulation(nextGenerationParents, nBits, PK)
nextGeneration <- processMutationInPopulation(nextGeneration, nBits, PM)
fintessRatesMatrix2 <- fitnessEvaluation(nextGeneration)
print(fintessRatesMatrix1)
print(fintessRatesMatrix2)
