
# description -------------------------------------------------------------

# this code runs the calculations for coefficient estimation, in/out of sample predictions and predcition validation 


# read ---------------------------------------------------------------
# source auxiliary functions
source('code/auxiliary.R')

# load packages
pacman::p_load(tidyverse, stargazer)

# read processed data
data <- as.data.frame(readRDS('output/data.RDS'))

# transform data
dat <- data_transformation(data, pb = F, predict = 2020)


# est - cointains the results for tabel 1 ---------------------------------------------------------------------

# methods to be used
methods <- c('OLS_small', 'OLS_large', 'OLS_large_cFE', 'OLS_large_interacted_cFE', 'RW', 'mean', 'TS')
e <- list()

# iterate over methods
for (i in 1:length(methods)) {
  
  # print names
  print(methods[[i]])
  
  # estimate
  e[[i]] <- estimate(data = dat$dat_est, methods[[i]])
  
  # name elements
  names(e)[i] <- methods[[i]]
  
}


# predict  --------------------------------------------------------------------


# methods to be used
methods <- c('OLS_small', 'OLS_large', 'OLS_large_cFE', 'OLS_large_interacted_cFE', 'RW', 'mean', 'TS')
p_in_sample <- list()
p_out_of_sample <- list()

# iterate over methods and predict in sample (2015)
for (i in 1:length(methods)) {
  
  # print names
  print(methods[[i]])
  
  # predict (set pb = T to predict pseudo Bayesian flows)
  p_in_sample[[i]] <- pred(data = data, pb = F, methode = methods[[i]], predict = 2015)
  
  # name elements
  names(p_in_sample)[i] <- methods[[i]]
  
}

# iterate over methods and predict out of sample (2020)
for (i in 1:length(methods)) {
  
  # print names
  print(methods[[i]])
  
  # predict
  p_out_of_sample[[i]] <- pred(data = data, pb = F, methode = methods[[i]], predict = 2020)
  
  # name elements
  names(p_out_of_sample)[i] <- methods[[i]]
  
}





# validate ----------------------------------------------------------------

# Table 2 - log level
val_ll <- validation(p_in_sample, c = F)

# Table 2 - changes
p_change <- pred_change(p_in_sample)
val_c <- validation(p_change, c = T)

# plot --------------------------------------------------------------------

plot_pred(p_in_sample)

# output for paper ------------------------------------------------------------------

# T1
stargazer(e$OLS_small, e$OLS_large, e$OLS_large_cFE, e$OLS_large_interacted_cFE, omit = 25:376)

# T2
stargazer(val_ll)
stargazer(val_c)

# plots
plot_pred(p_in_sample)




# tmp ---------------------------------------------------------------------

a <- dat$dat_est
bc <- paste(a$orig,a$dest)
for (y in unique(a$year0)) {
  
  tmp <- a[a$year0 == y,]
  bc <- intersect(unique(bc), unique(paste(tmp$orig,tmp$dest)))
  
}

d <- data_transformation(data, pb = F, 2010)



# methods to be used
methods <- c('OLS_small', 'OLS_large', 'OLS_large_cFE', 'OLS_large_interacted_cFE', 'RW', 'mean', 'TS')
p_in_sample_change <- list()
# iterate over methods and predict in sample (2015)
for (i in 1:length(methods)) {
  
  # print names
  print(methods[[i]])
  
  # predict
  tmp_10 <- pred(data = data, pb = F, methode = methods[[i]], predict = 2010)
  tmp_15 <- pred(data = data, pb = F, methode = methods[[i]], predict = 2015)
  
  # merge pred for 10 and 15
  tmp <- left_join(tmp_15, tmp_10, c('orig', 'dest'))
  
  # store change
  p_in_sample_change[[i]] <- data.frame(orig = tmp$orig,
                                        dest = tmp$dest,
                                        flow_real = tmp$flow_real.x - tmp$flow_real.y,
                                        flow_pred = tmp$flow_pred.x - tmp$flow_pred.y,
                                        flow_lagged = tmp$flow_lagged.x - tmp$flow_lagged.y)
  
  # name elements
  names(p_in_sample_change)[i] <- methods[[i]]
}

val <- validation(p_in_sample_change)



# to
# use balanced panel
# import function p_change
# check tabel est
# update tabel val

