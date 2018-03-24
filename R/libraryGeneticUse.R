# Hello, world!
#
# This is an example function named 'hello'
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
library(GA)

findBestResultByGeneticAlgorithm <- function(population, pmutation, pcrossing) {
  f <- function(x) as.integer(2*(x^2+1))
  min <- 1
  max <- 127
  pmutation <- as.double(pmutation)
  pcrossing <- as.double(pcrossing)
  print(pmutation)
  print(pcrossing)
  curve(f, min, max, n = 1000)
  GA <- ga(type = "real-value", min = min, max = max, pmutation = pmutation, pcrossover = pcrossing, fitness = f, maxiter = 100, monitor = FALSE, popSize = population)
  print(summary(GA))
  plot(GA)
}

readParametersAndRunAnalysis <- function() {
  p <- readline(prompt="Enter a population number: ")
  p <- as.integer(p)
  pc <- readline(prompt="Enter a crossover probability: ")
  pm <- readline(prompt="Enter a mutation probability: ")
  findBestResultByGeneticAlgorithm(p, pm, pc)
}

