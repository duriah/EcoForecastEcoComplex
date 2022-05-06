# function that create all possible predictor combinations and puts them in a list. 
# The target variable is always the first predictor

predictor_combinator <- function(predictors, temperature.included = T, which=NULL, rmTfrompred=T){
  # predictors <- predictors[!is.element(predictors, c(target,"temperature"))]
  if(rmTfrompred) predictors <- predictors[!is.element(predictors, c("temperature"))]
  if(is.null(which)){
    num.predictors <- 1:length(predictors)
  } else {
    num.predictors <- which
  }
  l <- sapply(num.predictors, function(x) {
    comb <- combn(predictors, x, simplify = F)
    if(length(comb)>200) comb <- sample(comb,200,replace = F)
    comb
  })
  
  if(length(which)==1) l <- list(l)
  
  if(temperature.included==T) {
    l2 <- lapply(l, function(lis){lapply(lis, append, values="temperature")})
    l <- c(l,l2)
    l <- do.call(list, unlist(l, recursive=FALSE))
    l <- append(l, "temperature", after=0)
  } else {
    l <- do.call(list, unlist(l, recursive=FALSE))
    }

  
  # names(l) <- paste0(l)
  return(l)
}

# wrapper function for the Multiview function

mv_wrapper <- function(data, k=0, columns, target, E=3, max_lag=3, num_neighbors=0, 
                       excludeTarget=F, lib=NULL, pred=NULL){
  data <- cbind(t=1:nrow(data),data)
  if(is.null(lib)){
    lib <- c(1, floor(NROW(data)*2/3))
  }
  if(is.null(pred)){
    pred <- c(floor(NROW(data)*2/3) + 1, NROW(data))
  }
  
  m <- Multiview(dataFrame = data, lib = lib, pred = pred, E = max_lag, D = E, 
                 knn = num_neighbors, multiview = k, columns = columns, 
                 target = target, excludeTarget=excludeTarget, numThreads = 1)
  return(m)
}

# function that calculates the rmse of all models in list and then only keeps the
# model with the best (smallest) rmse value.
# Returns best model, best RMSE and corresponding k value (=number of views used)

model_selector <- function(mv_wrapper_list,k){
  prediction_list <- lapply(mv_wrapper_list, "[[", 2)
  rmse_list <- lapply(prediction_list, 
                      function(x) sqrt(mean((x$Observations-x$Predictions)^2, 
                                            na.rm = T)))
  idx <- which.min(rmse_list)
  return(list(rmse_list[[idx]], k[idx]))
}

# Main function
# function that for a given dataset, predictor list and target variable calculates
# the range of k values to be tested and then. It calls the mv_wrapper function for 
# each value of k. Is saves the return of the mv_wrapper function as entries in a 
# list and then returns the list. It calls the mv_wrapper function twice: once for 
# when the target is a predictor and once for when it is not.

model_fitter <- function(data, target, predictor_combinations, max_lag=3, E=3, ID,
                         num_neighbors=0, k=0, lib=NULL, pred=NULL, ...){
  
  output_list <- unname(lapply(predictor_combinations, function(y){

    t_excluded = !(target %in% y)

    if(length(y)*max_lag<E) return(list(NA,NA))

    kmax <- choose(length(y)*max_lag, E) 
    
    k_input <- k[k<=kmax]
    input_data <- data %>% ungroup() %>% dplyr::select(all_of(y),target)
    if(t_excluded==T) y <- append(y, target)
    columns <- paste(y, collapse = " ")
    mv_wrapper_list <- lapply(k_input, function(x)
      mv_wrapper(data = input_data, k = x, columns = columns, target = target, E = E,
                 max_lag = max_lag, num_neighbors = num_neighbors, 
                 excludeTarget = t_excluded, lib=lib, pred=pred))
    model_selector(mv_wrapper_list,k_input)}))
  
  # output_list <- output_list[-which(sapply(output_list, is.null))]
  
  df <- mv_housekeeping(predictor_combinations, output_list, data, target, ID)
  return(df)
}

# Function that handles the data by creating a dataframe with all information wanted.

mv_housekeeping <- function(predictor_combinations, output_list, data, 
                            target, ID){
  t_excluded <- sapply(predictor_combinations, function(y){
    !(target %in% y)
  })
  # if(isTRUE(t_excluded)){
  #   predictor_combinations <- lapply(predictor_combinations, function(x) x[-1])
  # }
  
  
  num_predictors <- sapply(predictor_combinations, function(x) length(x))
  num_pred_without_temp <- sapply(predictor_combinations, 
                                  function(x) length(x[x != "temperature"]))
  temperature_included <- sapply(predictor_combinations, 
                                 function(x) "temperature" %in% x)
  names <- unname(sapply(predictor_combinations, function(y) paste(y, collapse = " ")))
  
  
  if(length(output_list)==0) {
    k <- NA
    RMSE <- NA
  } else {
    k <- sapply(output_list, "[[", 2)
    RMSE <- sapply(output_list, "[[", 1)
  }
  
  df <- data.frame(Target=target, 
                   ID = ID,
                   predictor_combination = names,
                   num_pred = num_predictors, 
                   num_pred_without_temp = num_pred_without_temp,
                   temperature_included = temperature_included,
                   target_excluded = t_excluded,
                   treat = unique(data$treat),
                   treat2 = unique(data$treat2), 
                   treat3 = unique(data$treat3), 
                   RMSE = RMSE,
                   k = k)
  return(df)
}

# function that prepares the data that goes into the model_fitter function. It calls the
# model_fitter function for each bottle ID in parallel and then handles the return of the 
# model_fitter function by creating a list with two entries. The first entry is the merged
# dataframe across all bottles for the target species and includes the information about 
# predictors combinations and rmse values. The second entry # is a list with 18 entries
# (one for each bottle ID), and each of these 18 entries contains the best models for each 
# predictor combination tested.

model_fitter_wrapper <- function(whole.data, target, num.clusters, max_lag=3, 
                                 E=3, num_neighbors=0, k=0, predictor_combinations, 
                                 lib=NULL, pred=NULL,  ...){
  # if(isTRUE(t_excluded)) predictor_combinations <- predictor_combinations[-1]
  data.list <- split(x = whole.data, f = whole.data$ID, drop = T)
  
  model_fitter_output_list <- mclapply(data.list, function(d.f){
    model_fitter(data = d.f, target = target, 
                 predictor_combinations = predictor_combinations, max_lag = max_lag,
                 E = E, ID = unique(d.f$ID), num_neighbors = num_neighbors, k = k, 
                 lib=lib, pred=pred, ...)
  }, mc.cores = num.clusters, mc.cleanup = T, mc.allow.recursive = F)
  
  whole_df <- do.call("rbind", model_fitter_output_list)
  return(whole_df)
}


  


####### Univariate Simplex forecasting with optimal E

UniSimplex_determineE <- function(data, Target, maxE, lib, pred) {
  
  mat <- EmbedDimension(dataFrame = data, lib = lib, pred = pred,
                        target = Target, columns = Target, maxE = maxE, showPlot = F)
  best <- mat %>%
    mutate(rho = round(rho,3)) %>%
    filter(rho == max(rho))
  return(best$E)
}

simplexForecasting <- function(data, target, num.clusters, 
                               lib=NULL, pred=NULL, maxE){
  
  data.list <- split(x = data, f = data$ID, drop = T)
  
  model_fitter_output_list <- mclapply(data.list, function(df){
    if(is.null(lib)){
      lib <- c(1, floor(NROW(df)*2/3))
    }
    if(is.null(pred)){
      pred <- c(floor(NROW(df)*2/3) + 1, NROW(df))
    }
    
    ID <- unique(df$ID)
    treat <- unique(df$treat)
    treat2 <- unique(df$treat2)
    treat3 <- unique(df$treat3)
    df <- df[,c("day",target)]
    
    E <- UniSimplex_determineE(df, target, maxE, lib, pred)
    forecast <- Simplex(dataFrame = df, lib = lib, pred = pred, target = target,
                        columns = target, E = E)
    rmse <- sqrt(mean((forecast$Observations-forecast$Predictions)^2, na.rm = T))
    data.frame(Target=target, RMSE=rmse, E=E, ID=ID,
               treat = treat, treat2 = treat2, 
               treat3 = treat3)
  }, mc.cores = num.clusters, mc.cleanup = T, mc.allow.recursive = F)
  
  whole_df <- do.call("rbind", model_fitter_output_list)
  return(whole_df)
}
  


