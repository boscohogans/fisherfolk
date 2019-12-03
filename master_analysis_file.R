library(tidyverse)
library(lme4)
library(sjPlot)
library(data.table)
library(naniar)
library(MatchIt)
library(scales)


#Building dataset
source("building_dataset.R")

#Make indicator variables
source("indicator_variables.R")

#Generate propenstiy score
source("propensity_score_match.R")

#Inequality curves
#source("inequality_curves.R")

#Regressions
#source("regressions.R")