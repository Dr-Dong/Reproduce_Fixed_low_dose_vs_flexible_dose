#install.packages("meta")
#install.packages("metafor")
#install.packages("readxl") 
#install.packages("summarytools")
#install.packages("dplyr")

Sys.setenv(LANG = "en")
options(warn=-1)# there are several non-important warnings, like omiting NAs in meta-regression. If you feel like seeing them, just delete the options(warn=-1)

library(meta)
library(metafor)
library(readxl)
library(summarytools)
library(dplyr)

#DESCRIPTIVES
source("Descriptives.R")

#PRIMARY ANALYSES
source("20vs20-40-80.R")
source("20vs20-40-80tolerability.R")
source("20vs20-40-80acceptability.R")

#ADJUSTED FOR POSSIBLE CONFOUNDER


#SECONDARY ANALYSES
source("20vs20-40.R")   ###to see if just a little bit of increase could be better
source("20vs10-20-40-80.R")   ###allowing for downward titration
source("20vs20-40-80adjusted for year.R")   ###adjusted for study year



