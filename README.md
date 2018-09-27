# Bayesian models of diversity effects 


## Toy example

- *toy.Rmd* the R markdown file for Bayesian reasoning in the toy example
- *toy.html* the output

## Analysis

- The four CSV in the data subdirectory are the raw anonymised data from each condition
- The three R scripts handle preprocessing, computing the CDFs and computing the Bayes factors
- Output directory has the reshaped data, the CDFs and the results of the Bayes factor analysis 

## Modelling

- *diversitySampling.R* implements the Bayesian generalisation model under strong and weak sampling assumptions, generates random hypothesis spaces and then computes the relationship between premise diversity and the strength of evidence for a superordinate category.
- *diversityData.Rdata* saves the results of one such simulation
- *empiricalData.csv* is a set of averaged ratings from the experiments used in drawing the figures
- *figure.pdf* is the output