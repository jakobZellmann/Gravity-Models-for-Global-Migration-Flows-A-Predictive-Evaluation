
# validation --------------------------------------------------------------

validation <- function(prediction, c){
  
  #list for validation output
  valOut <- c()
  
  for(i in 1:length(prediction)){
    
    tmp <- predictionValidation(pred_output = prediction[[i]], change = c)
    
    valOut <- rbind(valOut, tmp)
    
  }
  
  rownames(valOut) <- names(prediction)
  
  return(valOut)
}



# plot --------------------------------------------------------------------

plot_pred <- function(realPred){
  
  #require ggplot2
  pacman::p_load(ggplot2, tidyverse)
  
  for(i in 1:length(realPred)){
    
    print(realPred[[i]] %>%
            
            #plot frame
            ggplot(aes(x = flow_pred,y = flow_real)) +
            
            #plot points
            geom_point(alpha=0.5) +
            
            # emigration form Moldova
            #scale_color_manual(values = c("#66a61e", "#08519c"), name = "Emigration from") +
            #geom_point(subset(realPred[[i]], orig=="MDA"), mapping = aes(x = flow_pred, y = flow_real), color = "#66a61e", size = 3) +
            
            
            #add lables
            labs(x= "Predicted", y="Real")+
            
            #add reg. line
            geom_smooth(method=lm)+
            
            #add 45 degree line
            geom_segment(aes(x = min(min(flow_real), min(flow_pred)), xend = max(max(flow_real), max(flow_pred)), y =  min(min(flow_real), min(flow_pred)), yend =  max(max(flow_real), max(flow_pred))),
                         colour = "darkred", lty = "dashed", size=0.8))
    
    #save
    filename <- paste(names(realPred)[i], '.png', sep = '')
    
    ggsave(filename, path = 'output/plots')
    
    
  }
  
}



# with in transformation --------------------------------------------------

withIn <- function(data, colNames){
  
  #variables to be transformed
  colNames <- c("flow", "flow_lagged", "population_orig", "population_dest", "gdp_pc_orig", "gdp_pc_dest", "age_15_64_orig", "age_15_64_dest", "midclass_orig", "midclass_dest", "education_orig", "education_dest", "unemployment_orig", "unemployment_dest", 'fertility_orig', 'fertility_dest', 'migrant_stock',  'dist')
  
  #levels to log
  for(i in colNames){
    
    data[,i] <- log(data[,i])
    
  }
  
  #1
  dataMean <- aggregate(data, by=list(data$orig, data$dest), function(x){ifelse(is.numeric(x),return(mean(x, na.rm = T)), return(NaN))})                
  
  #1.1
  meanFlow <- dataMean[,c('Group.1','Group.2', 'flow')]
  ## names to meanFlow
  names(meanFlow)[1:2] <- c('orig', 'dest') 
  
  #2
  dataWtTmp <- merge(x=data, y=dataMean, by.x = c('orig', 'dest'), by.y = c('Group.1', 'Group.2'), all.x = T, all.y = F)
  

  #we omit dist
  colNames <- colNames[!colNames=='dist']
  
  #3
  for(i in colNames){
    
    data[,i] <- dataWtTmp[,paste(i, 'x', sep='.')] - dataWtTmp[,paste(i, 'y', sep='.')]
    
  }
    
  return(list(dat_WI = data, FE = meanFlow))
  
}




# transform data ----------------------------------------------------------

data_transformation <- function(data, pb = F, predict){
  
  # data is the data to be used
  # predict \in {2015, 2020} is the prediction period
  # orig list of origin countries (ISO3 character) 
  # dest list of destination countries (ISO3 character) 
  # pb T if Bayesian flow estimates showed be used
  
  if (pb) {
    
    # replace da_min_closed with pb
    data$flow <- data$da_pb_closed.x
    data$flow_lagged <- data$da_pb_closed.y 
    
  }
  
  # exclude columns
  data$da_pb_closed.x <- NULL
  data$da_pb_closed.y <- NULL
  
  dataTmp <- data
  
  # if migration.stock is NA we assume it to be 0
  dataTmp[is.na(dataTmp$migrant_stock), 'migrant_stock'] <- 0
  
  # omit first period
  dataTmp <- subset(dataTmp, year0!=1990)
  
  # flow_lagged + 1
  dataTmp$flow_lagged <- dataTmp$flow_lagged + 1
  
  # migration.stock + 1
  dataTmp$migrant_stock <- dataTmp$migrant_stock + 1
  
  # flow + 1
  dataTmp$flow <- dataTmp$flow + 1
  
  # education_orig + 1
  dataTmp$education_orig <- dataTmp$education_orig + 1
  
  # education_dest + 1
  dataTmp$education_dest <- dataTmp$education_dest + 1
  
  # omit NAs in estimation data
  ## we temporary replace NA in flow for year0==2020 by 0
  dataTmp[dataTmp$year0==2020, 'flow'] <- 0
  ## omit NAs
  dataTmp <- na.omit(dataTmp)
  ## replace 0 by NA
  dataTmp[dataTmp$year0==2020, 'flow'] <- NA
  
  
  # make the data balanced
  key <- paste(dataTmp$orig, dataTmp$dest)
  years <- unique(dataTmp$year0)
  for (y in years){

    tmp <- paste(subset(dataTmp, year0 == y)$orig, subset(dataTmp, year0 == y)$dest)
    key <- intersect(key, tmp)

  }

  index <- which(paste(dataTmp$orig, dataTmp$dest) %in% key)
  dataTmp <- dataTmp[index,]
  
  
  # split data
  dat_est <- subset(dataTmp, year0 < predict)
  dat_pred <- subset(dataTmp, year0 == predict)
  
  
  # return list of dat_est and dat_pred
  return(list(dat_est = dat_est, dat_pred = dat_pred))
  
}



# take logs --------------------------------------------------------------------

log_that <- function(data){
  
  #variables to be transformed
  colNames <- c("flow", "flow_lagged", "population_orig", "population_dest", "gdp_pc_orig", "gdp_pc_dest", "age_15_64_orig", "age_15_64_dest", "midclass_orig", "midclass_dest", "education_orig", "education_dest", "unemployment_orig", "unemployment_dest", 'fertility_orig', 'fertility_dest', 'migrant_stock',  'dist')
  
  #levels to log
  for(i in colNames){
    
    data[,i] <- log(data[,i])
    
  }
  
  # return
  return(data)
  
}


# prediction validation ---------------------------------------------------

predictionValidation <- function(pred_output, change){
  
  #this function takes the results of the function predictInSample (real and predicted flows) and returns the following statistics for forecast validation:
  
  #RMSE: root mean squared error of real and predicted values
  
  #jointP: p-value of the hypothesis a=0 and b=1 where a and b are defind by, lm(real ~ a + b pred)
  
  #interP: p-value of the hypothesis a=0
  
  #slopeP: p-value of the hypothesis b=1
  
  #pacman::p_load(car)
  
  #extract values
  real <- pred_output$flow_real
  pred <- pred_output$flow_pred
  logLaggedFlow <- pred_output$flow_lagged
  
  if(change){
    
    #real direction
    realDirection <- ifelse(real >=0, 1, 0)
    
    #predicted direction
    predDirection <- ifelse(pred >=0, 1, 0)
    
    #mean directional accuracy
    MDA <- sum(realDirection==predDirection)/length(realDirection)
    
    
  }else {
    #real direction
    realDirection <- ifelse(real - logLaggedFlow >=0, 1, 0)
    
    #predicted direction
    predDirection <- ifelse(pred - logLaggedFlow >=0, 1, 0)
    
    #mean directional accuracy
    MDA <- sum(realDirection==predDirection)/length(realDirection)
  }
  
  #calculate RMSE
  RMSE <- sqrt((t(real - pred)%*%(real - pred))/length(real))
  
  #estimate rationability model
  rat <- lm(real~pred)
  
  #results
  res <- c(RMSE = RMSE, MDA = MDA, inter = as.numeric(rat$coefficients[1]), slope = as.numeric(rat$coefficients[2]))
  
  return(res)
  
}



# estimate ----------------------------------------------------------------

estimate <- function(data, methode){
  
  
  # data is the transformed data - dat$est -  to be used
  # methode \in {OLS_small, OLS_large, OLS_large_cFE, OLS_large_interacted_cFE, RW, means, TS} - methode used for estimaton
  
  
  # estimated OLS_small  
  if (methode == 'OLS_small') {
    
    # formula
    formula <- as.formula('log(flow) ~  log(population_orig) + log(population_dest) + log(gdp_pc_orig) + log(gdp_pc_dest) + log(dist) + as.factor(contig) + as.factor(comlang_off) + as.factor(year0)')
    
    # estimation
    model <- lm(formula = formula, data = data)
    
    # result
    return(model)
    
  } 
  
  # estimate OLS_large
  if (methode == 'OLS_large') {
    
    # formula
    formula <- as.formula('log(flow) ~ log(population_orig) + log(population_dest) + log(gdp_pc_orig) + log(gdp_pc_dest) + log(dist) + as.factor(contig) + as.factor(comlang_off)  + log(flow_lagged) + log(migrant_stock) +  log(age_15_64_orig) + log(age_15_64_dest) + log(midclass_orig) + log(midclass_dest) + log(education_orig) + log(education_dest) + log(unemployment_orig) + log(unemployment_dest) + log(fertility_orig) + log(fertility_dest) + as.factor(year0)')
    
    # estimation
    model <- lm(formula = formula, data = data)
    
    # result
    return(model)
    
  }
  
  
  # estimate OLS_large_cFE
  if (methode == 'OLS_large_cFE') {
    
    # formula
    formula <- as.formula('log(flow) ~ log(population_orig) + log(population_dest) + log(gdp_pc_orig) + log(gdp_pc_dest) + log(dist) + as.factor(contig) + as.factor(comlang_off)  + log(flow_lagged) + log(migrant_stock) +  log(age_15_64_orig) + log(age_15_64_dest) + log(midclass_orig) + log(midclass_dest) + log(education_orig) + log(education_dest) + log(unemployment_orig) + log(unemployment_dest) + log(fertility_orig) + log(fertility_dest) + as.factor(year0) + as.factor(orig) + as.factor(dest)')
    
    # estimation
    model <- lm(formula = formula, data = data)
    
    # result
    return(model)
    
  }
  
  # estimate OLS_large_interacted_cFE (Stats should be adjusted)
  if (methode == 'OLS_large_interacted_cFE') {
    
    # apply within transformation
    WI <- withIn(data)
    dat_WI <- WI$dat_WI
    FE <- WI$FE
    
    # bring data and FE in the same order
    dat_tmp <- merge(dat_WI, FE, by = c('orig', 'dest'))
    
    # names
    dat_WI <- rename(dat_tmp, c('flow' = 'flow.x', 'FE' = 'flow.y'))
    
    # formula
    formula <- as.formula('flow ~ population_orig + population_dest + gdp_pc_orig + gdp_pc_dest + dist + as.factor(contig) + as.factor(comlang_off)  + flow_lagged + migrant_stock +  age_15_64_orig + age_15_64_dest + midclass_orig + midclass_dest + education_orig + education_dest + unemployment_orig + unemployment_dest + fertility_orig + fertility_dest + as.factor(year0)')
    # estimation
    model <- lm(formula = formula, data = dat_WI)
    
    # change names of variables
    for (i in 2:18){
      
      names(model$coefficients)[i] <- paste('log(', names(model$coefficients)[i], ')', sep = '')
      
    }
    
    # change df, i.e. subtract number of FE parameter
    df <- model$df.residual - length(unique(paste(dat_WI$orig, dat_WI$dest)))
    model$df.residual <- df
    
    # residuals (FE do not change residuals as they are subtracted on both sides)
    r <- model$residuals
    
    # R^2
    ## SQR
    SSR <- as.numeric(crossprod(r))
    ## SQT
    SST <- as.numeric(crossprod(dat_WI$flow + dat_WI$FE - mean(dat_WI$flow  + dat_WI$FE)))
    ## finally
    R_2 <- 1 - SSR/SST
    
    # adj R^2
    adj_R_2 <- 1 - (1-R_2)*(nrow(dat_WI)-1)/df
    
    # F test
    f <- R_2/(1-R_2)*(df/(length(unique(paste(dat_WI$orig, dat_WI$dest)))+length(model$coefficients)))
    sig_f <- f > qf(0.995, nrow(FE) + length(model$coefficients) -1, df)
    
    # result
    return(list(model = model, FE = FE, R_2 = R_2, adj_R_2 = adj_R_2, f = list(f_stat = f, f_sig = sig_f, number_restrictions = nrow(FE) + length(model$coefficients) -1)))
    
  }
  
  # estimate RW
  if (methode == 'RW') {
    
    # get year0 of last period
    last_period <- max(data$year0)
    
    # use lags of last period as estimate for this period
    model <- subset(data, subset = year0 == last_period, select = c(orig, dest, flow))
    
    # tha logs of flow
    model$flow <- log(model$flow)
    
    # return result
    return(model)
    
  }
  
  # estimate mean
  if (methode == 'mean') {
    
    # get group means (by=list(orig, dest))
    model <- withIn(data)$FE
    
    # result
    return(model)
    
  }
  
  # estimate TS
  if (methode == 'TS') {
    
    # apply within transformation
    WI <- withIn(data)
    dat_WI <- WI$dat_WI
    FE <- WI$FE
    
    # estimate AR(1)
    model <- lm(flow ~ flow_lagged, data = dat_WI)
    
    # result
    return(list(model = model, FE = FE))
    
  }
  
}



# prediction --------------------------------------------------------------

pred <- function(data, pb, methode, predict) {
  
  # raw data
  # methode \in {OLS_small, OLS_large, OLS_large_cFE, OLS_large_interacted_cFE, RW, means, TS} - methode used for estimaton
  # predict is prediction period
  
  # tranform data
  dat <- data_transformation(data = data, pb = pb, predict = predict)
  
  ## use fixed effects of period before for prediction
  dat$dat_pred$year0 <- predict - 5 
  
  # estimation
  est <- estimate(data = dat$dat_est, methode = methode)
  
  # predict OLS_small, OLS_large, OLS_large_cFE
  if (methode %in% c('OLS_small', 'OLS_large', 'OLS_large_cFE')) {
    
    # predict
    pred <- predict(object = est, newdata = dat$dat_pred)
    
    # negative prediction to 0
    pred <- ifelse(pred < 0, 0, pred)
    
    # combined pred with orig and dest and real flow
    pred_orig_dest <- data.frame(orig = dat$dat_pred$orig,
                                 dest = dat$dat_pred$dest,
                                 flow_real = log(dat$dat_pred$flow),
                                 flow_lagged = log(dat$dat_pred$flow_lagged),
                                 flow_pred = pred)
    
    # return
    return(pred_orig_dest)
    
  }
  
  # predict OLS_large_interacted_cFE !!LOGS FOR EXPLANATORY VARS!!
  if (methode == 'OLS_large_interacted_cFE') {
    
    # within transform prediction data ??
    # dat_pred <- withIn(data = dat$dat_pred)$dat_WI
    
    # that logs of prediction data
    dat_pred <- log_that(data = dat$dat_pred)
    
    # predict
    pred <- predict(object = est$model, newdata = dat_pred)
    
    # combined pred with orig, dest and real flows
    pred_orig_dest <- data.frame(orig = dat_pred$orig,
                                 dest = dat_pred$dest,
                                 flow_real = dat_pred$flow,
                                 flow_lagged = dat_pred$flow_lagged,
                                 flow_pred = pred)
    
    # add FE
    pred_orig_dest_FE_tmp <- merge(x = pred_orig_dest, y = est$FE, by = c('orig', 'dest'), all.x = T, all.y = F)
    
    # pred flows + FE
    pred_orig_dest_FE <- data.frame(orig = pred_orig_dest_FE_tmp$orig,
                                    dest = pred_orig_dest_FE_tmp$dest,
                                    flow_real = pred_orig_dest_FE_tmp$flow_real,
                                    flow_lagged = pred_orig_dest_FE_tmp$flow_lagged,
                                    flow_pred = pred_orig_dest_FE_tmp$flow_pred + pred_orig_dest_FE_tmp$flow)
    # negative prediction to 0
    pred_orig_dest_FE$flow_pred <- ifelse(pred_orig_dest_FE$flow_pred < 0, 0, pred_orig_dest_FE$flow_pred)
    
    # return
    return(pred_orig_dest_FE)
    
  }
  
  # predict RW
  if (methode == 'RW') {
    
    # merge real and pred data
    pred_orig_dest <- merge(x = dat$dat_pred[,c('orig', 'dest', 'flow', 'flow_lagged')], y = est, by = c('orig', 'dest'))
    
    # take log
    pred_orig_dest$flow.x <- log(pred_orig_dest$flow.x)
    pred_orig_dest$flow_lagged <- log(pred_orig_dest$flow_lagged)
    
    
    # names
    names(pred_orig_dest)[c(3,5)] <- c('flow_real', 'flow_pred')
    
    # return
    return(pred_orig_dest)
    
  }
  
  # predict means
  if (methode == 'mean') {
    
    # merge real and pred data
    pred_orig_dest <- merge(x = dat$dat_pred[,c('orig', 'dest', 'flow', 'flow_lagged')], y = est, by = c('orig', 'dest'), all.x = T, all.y = F)
    
    # take log
    pred_orig_dest$flow.x <- log(pred_orig_dest$flow.x)
    pred_orig_dest$flow_lagged <- log(pred_orig_dest$flow_lagged)
    
    # names
    names(pred_orig_dest)[c(3,5)] <- c('flow_real', 'flow_pred')
    
    # return
    return(pred_orig_dest)
    
  }
  
  # predict TS
  if (methode == 'TS') {
    
    # within transform prediction data
    # dat_pred <- withIn(data = dat$dat_pred)
    
    # that logs of prediction data
    dat_pred <- log_that(dat$dat_pred)
    
    # predict without FE
    pred <- predict(object = est$model, newdata = dat_pred)
    
    # combined pred with orig and dest
    pred_orig_dest <- data.frame(orig = dat_pred$orig,
                                 dest = dat_pred$dest,
                                 flow_real = dat_pred$flow,
                                 flow_lagged = dat_pred$flow_lagged,
                                 flow_pred = pred)
    
    # add FE
    pred_orig_dest_FE_tmp <- merge(x = pred_orig_dest, y = est$FE, by = c('orig', 'dest'), all.x = T, all.y = F)
    
    # pred flows + FE
    pred_orig_dest_FE <- data.frame(orig = pred_orig_dest_FE_tmp$orig,
                                    dest = pred_orig_dest_FE_tmp$dest,
                                    flow_real = pred_orig_dest_FE_tmp$flow_real,
                                    flow_lagged = pred_orig_dest_FE_tmp$flow_lagged,
                                    flow_pred = pred_orig_dest_FE_tmp$flow_pred + pred_orig_dest_FE_tmp$flow)
    
    # negative prediction to 0
    pred_orig_dest_FE$flow_pred <- ifelse(pred_orig_dest_FE$flow_pred < 0, 0, pred_orig_dest_FE$flow_pred)
    
    # return
    return(pred_orig_dest_FE)
    
  }
  
  
}


# transform log prediction to level ---------------------------------------

log_to_level <- function(data, p_out_of_sample_method){
  
  # get flows of last period
  flow_2015 <- subset(data, subset = year0 == 2015, select = c(orig, dest, flow))
  
  # flows of last period + 1
  flow_2015$flow_level_2015 <- flow_2015$flow + 1 
  
  # log flow
  flow_2015$flow_log_2015 <- log(flow_2015$flow_level_2015)
  
  # merge prediction and real flow
  flow_real_pred_tmp <- merge(x = flow_2015, y = p_out_of_sample_method, by = c('orig', 'dest'), all.x = F, all.y=F)
  
  # give names and omit variables
  flow_real_pred <- data.frame(orig = flow_real_pred_tmp$orig,
                               dest = flow_real_pred_tmp$dest,
                               flow_level_2015 = flow_real_pred_tmp$flow_level_2015,
                               flow_log_2015 = flow_real_pred_tmp$flow_log_2015,
                               flow_log_2020 = flow_real_pred_tmp$flow_pred)
  
  # calculate growth factor
  flow_real_pred$g <- flow_real_pred$flow_log_2020 - flow_real_pred$flow_log_2015 + 1
  
  # calculate predicted level flow
  flow_real_pred$flow_level_2020 <- flow_real_pred$flow_level_2015 * flow_real_pred$g

  # predicted level flow 0 if negative
  flow_real_pred$flow_level_2020 <- ifelse(flow_real_pred$flow_level_2020 < 0, 0, flow_real_pred$flow_level_2020)
  
  # return
  return(flow_real_pred)
  
}



# routine to calculate relative change of prediction ----------------------


pred_change <- function(p){
  
  # iterate over elements of p
  for (i in 1:length(p)){
    
    index <- !(p[[i]]$flow_lagged==0)
    
    p[[i]] <- p[[i]][index,]
    
    # replace flow pred by (log) change
    p[[i]]$flow_pred <- (p[[i]]$flow_pred - p[[i]]$flow_lagged)/p[[i]]$flow_lagged
    
    # replace flow real by (log) change
    p[[i]]$flow_real <- (p[[i]]$flow_real - p[[i]]$flow_lagged)/p[[i]]$flow_lagged
    
    
  }
  
  return(p)
  
}
