source('C:\\repos\\learn-doing\\R\\functions.R')
df=readRDS(file = 'C:\\repos\\learn-doing\\data\\contractData_new.rds')
df.ranked=df

#Checked to run correctly
source('C:\\repos\\learn-doing\\R\\Mechanisms.R')
source('C:\\repos\\learn-doing\\R\\ranking.R')
source('C:\\repos\\learn-doing\\R\\main_results.R')


# Load robustness analysis
result.robustness.ranks=readRDS(file = 'C:\\repos\\learn-doing\\data\\robustness_ranks_new.rds')
table_robustness_period=readRDS(file = 'C:\\repos\\learn-doing\\data\\table_robustness_period_new.rds')
table_robustness_weightprice=readRDS(file = 'C:\\repos\\learn-doing\\data\\table_robustness_weight_new.rds')
robustness_close_wins.lin=readRDS(file='C:\\repos\\learn-doing\\data\\robustness_close_wins.lin_new.rds')
robustness_close_wins.bin=readRDS(file='C:\\repos\\learn-doing\\data\\robustness_close_wins.bin_new.rds')



source('C:\\repos\\learn-doing\\R\\results_output.R')
#library(Rcpp)
#library(estimatr)
#source('C:\\repos\\learn-doing\\R\\Robustness_FULL.R')
