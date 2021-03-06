library(utils)
library(rgdal)
library(sf)
library(leaflet)
library(dplyr)
library(rgeos)
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
library(statar)
library(ggplot2)

#Finding fishing villages
source("buffer_creation.R")

#Sensitivity analysis
#source("sensitivity_analysis.R")

#Kenya sensitivity
#source("kenya_sensitivty.R")

#Building dataset
source("building_dataset.R")

#Make indicator variables
source("indicator_variables.R")

#Tabels of indicator variables
#source("indicator_tables.R")

#Generate propenstiy score
source("propensity_score_match.R")

#Tables
source("descriptive_tables.R")

#Inequality curves
#source("inequality_curves.R")

#Regressions
source("regressions.R")

#Regression tables and graphs
source("tables_graphs.R")

