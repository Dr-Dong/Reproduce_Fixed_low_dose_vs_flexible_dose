install.packages("meta")
install.packages("metafor")
install.packages("readxl") 
install.packages("summarytools")
install.packages("dplyr")

options(warnings=-1)# there are several non-important warnings, like omiting NAs in meta-regression. If you fill like seeing them, just delete the options(warn=-1)
library(meta)
library(metafor)
library(readxl)
library(summarytools)
library(dplyr)


options(warn = -1)
#DESCRIPTIVES
source("Descriptives.R")

#PRIMARY ANALYSES
source("20vs20-40-80.R")
source("20vs20-40-80tolerability.R")
source("20vs20-40-80acceptability.R")

#ADJUSTED FOR POSSIBLE CONFOUNDER
source("20vs20-40-80adjusted.R")

#secondary analyses to see if just a little bit of increase could be better
source("20vs20-40.R")
source("20vs20-40tolerability.R")
source("20vs20-40acceptability.R")
options(warn = 1)




