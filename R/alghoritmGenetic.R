# Script to find the max value of function 2(x^2+1) using basic genetic algotithm
library(GA)
source('~/R/geneticAlgorithm/R/algorithmGeneticSources.R')

fitnessFunction <- function(x) 2*(x^2+1)

nBits <- 8
minNumber <- 1
maxNumber <- 127
maxPopulation <- 100
PK <- 0.75
PM <- 0.10
numberOfGenerations <- 10

result <- basicGeneticAlgorithm(nBits, minNumber, maxNumber, maxPopulation, PK, PM, numberOfGenerations)
print(result)
print(binary2decimal(result))
