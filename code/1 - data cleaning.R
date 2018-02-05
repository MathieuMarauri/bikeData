
# Import and audit data. Cleaning if necessary (coercing variables to date o
# factor format).

# Inputs: the raw data

# Outputs: audit results and cleaned data.

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('auditdata') # data quality check
library('ggplot2') # plots


# Data cleaning -----------------------------------------------------------

# import data
bikes <- fread(file = 'data/raw/data.csv')

# first audit
qualityCheck(data = bikes,
             file = 'output/audit/bikes_raw.xlsx')

# coerce datetime variable to date format
bikes[, 'datetime' := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = 'CET')]

# coerce categorical variables to factor
factor_cols <- c('season', 'holiday', 'workingday', 'weather')
bikes[, (factor_cols) := lapply(X = .SD, FUN = as.factor), 
      .SDcols = factor_cols]

# second audit with properly formatted data
qualityCheck(data = bikes, 
             file = 'output/audit/bikes_clean.xlsx')

# save results and clean session
saveRDS(bikes, 'data/clean/bikes.rds')
rm(factor_cols, bikes)
