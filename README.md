## Comprehensive monitoring in North American bats reveals low body mass and reproductive status as predictors of coronavirus shedding 


### General Information

__FOR PEER REVIEW ONLY__

This repo contains instructions and source code for reproducing the statistical analyses in the manuscript.
Comprehensive monitoring in North American bats reveals low body mass and reproductive status as predictors of coronavirus shedding 


### Repo Contents

- Analysis2023.R: master analysis file for 2023 data analysis.
- stan scripts: contains the source .stan files to reproduce the anaysis. 
- NABOH_database.RData: an .RData file with a set of data frames used for the analysis.

### 1. System Requirements

#### Hardware Requirements

Our source code requires only a standard computer. Much of the Markov chain Monte Carlo code is run in parallel so a computer with ample memory and multiple cores can be advantageous. The runtimes below are generated using a macbook with the recommended specs (64 GB RAM, 8 cores at 2.7 GHz). The code will also work on linux or windows computer.

#### Software Requirements

Reproducing the statistical analyses requires a current version of R and stan. We use version 4.5.0 of R and version 2.32.7 of stan.

#### Package dependencies and versions

Users will need the following packages install the following packages to execute the code. Our versions are effective March 27, 2026

```
tidyverse
knitr
rstan
loo
scales
rstanarm
patchwork
viridis
```


### 2. Installation Guide

Running the analysis requires:

- installing R. Depending on wifi speeds, installing R usually takes a few minutes.
- installing stan. Depending on wifi speeds, installing stan usually takes a few minutes.
- installing the necessary R packages (listed above). Depending on wifi speeds, installing packages usually takes about 30 seconds per package.

### 3. Running R code

The script Analysis2023.R (and associated stan files) contain the code to run the analysis for sites with repeat visits in 2023 and will generate figures from the manuscript.

### 3. Instructions for Use

