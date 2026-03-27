## Comprehensive monitoring in North American bats reveals low body mass and reproductive status as predictors of coronavirus shedding 


### General Information

This repo contains instructions and source code for reproducing the statistical analyses in the manuscript.
Comprehensive monitoring in North American bats reveals low body mass and reproductive status as predictors of coronavirus shedding 


### Repo Contents

- scripts: contains the source .R and .stan files to reproduce the anaysis. Each file is detailed below in the specific sections corresponding to the statistical analyses.
- data: contains the raw source data and model generated output.
- figures: contains the final output figures from the manuscript. 

### 1. System Requirements

#### Hardware Requirements

Our source code requires only a standard computer. Much of the Markov chain Monte Carlo code is run in parallel so a computer with ample memory and multiple cores can be advantageous. The runtimes below are generated using a macbook with the recommended specs (64 GB RAM, 8 cores at 2.7 GHz). The code will also work on linux or windows computer.

#### Software Requirements

Reproducing the statistical analyses requires a current version of R and stan. We use version 4.5.0 of R and version 2.32.7 of stan.

#### Package dependencies and versions

Users will need the following packages install the following packages to execute the code. Our versions are effective March 27, 2026

```
tidyverse 2.0.0
lubridate 1.9.3
stringr 1.5.1
rstan 2.32.6
cowplot 1.1.3
ggtext 0.1.2
jpeg 0.1-10
scales 1.3.0
tictoc 1.2.1
```


### 2. Installation Guide

Running the analysis requires:

- installing R. Depending on wifi speeds, installing R usually takes a few minutes.
- installing stan. Depending on wifi speeds, installing stan usually takes a few minutes.
- installing the necessary R packages (listed above). Depending on wifi speeds, installing packages usually takes about 30 seconds per package.


### 3. Instructions for Use

