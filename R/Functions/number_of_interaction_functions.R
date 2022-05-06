## functions to select the best E

best_E_fct <- function(data, target, interactor){
  max1 <- c()
  max2 <- c()
  for(i in 2:5){
    ccm_out <- CCM(dataFrame = data, target = target, column = interactor,
                   libSizes = "60 60 10", Tp = -1, E = i, sample = 100)
    # max1[i-1] <- ccm_out[1,2]
    max2[i-1] <- ccm_out[1,3]
  }
  df <- data.frame(target=target,
                   interactor=interactor,
                   Best_E = which.max(max2)+1)
  return(df)
}

best_E_fct_trimmed <- function(data, target, interactor){
  max1 <- c()
  max2 <- c()
  for(i in 2:5){
    ccm_out <- CCM(dataFrame = data, target = as.character(unlist(target)), column = as.character(unlist(interactor)),
                   libSizes = "29 29 10", Tp = -1, E = i, sample = 100)
    # max1[i-1] <- ccm_out[1,2]
    max2[i-1] <- ccm_out[1,3]
  }
  df <- data.frame(target=target,
                   interactor=interactor,
                   Best_E = which.max(max2)+1)
  return(df)
}