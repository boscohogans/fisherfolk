library(tidyverse)
library(lme4)
library(sjPlot)
library(data.table)
library(naniar)
library(MatchIt)
library(scales)
library(broom.mixed)
library(PerformanceAnalytics)
library(ggpubr)




#Building dataset
source("building_dataset.R")

#Make indicator variables
source("indicator_variables.R")

#Generate propenstiy score
source("propensity_score_match.R")

#Inequality curves
source("inequality_curves.R")

#Regressions
source("regressions.R")

#Regression tables and graphs
source("regression_graphs.R")