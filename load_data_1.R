# Script to load the SSI irrigation data
# Belfast data dive 21/22 Jan 2020

# Required data
# ns is the number of studies
# na[] is a vector of number of arms for each study
# t[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the treatment in that arm
# r[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the number of events in that arm
# n[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the number of patients in that arm
# Ideally need a mapping from numeric t[, ] to the actual treatment names

# Library to read in Excel files
library(xlsx)

#baseline_directory<-"~/Bristol/R for CEA/R data dive Belfast 2020/NMA data project"
#setwd(baseline_directory)

# A bonus would be draw a network plot
source("mtm_networkplot_fun.R")

# Load ICL data
icl_data<-read.xlsx("SSI ICL data 20Jan2020.xlsx",sheetIndex=2)

# Here will be exported the formatted data
bugs_data <- list()

# Save it for use in the modelling script
save(bugs_data)

# t1 is a vector of interventions studied
# t2 is a vector of the corresponding studies
# nameoftreatments is some vector of treatment names
mtm_networkplot_fun(t1 = t1, t2 = t2, percomparison = FALSE, 
                    nameoftreatments = vector_of_treatment_names)
