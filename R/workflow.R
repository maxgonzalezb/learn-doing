source('C:\\repos\\learn-doing\\R\\functions.R')
df=readRDS(file = 'C:\\repos\\learn-doing\\data\\contractData_new.rds')
df.ranked=df
result.robustness.ranks=readRDS(file = 'C:\\repos\\learn-doing\\data\\robustness_ranks.rds')

#Checked to run correctly
source('C:\\repos\\learn-doing\\R\\main_results.R')

#
source('C:\\repos\\learn-doing\\R\\Mechanisms.R')
source('C:\\repos\\learn-doing\\R\\Robustness_FULL.R')


#library(Rcpp)
#library(estimatr)

