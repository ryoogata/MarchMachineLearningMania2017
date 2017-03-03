summaryResult <- function(MODEL_LIST) {
  # control
  if ( is.null(MODEL_LIST$call$control)) {
    cont <- "NA"
  } else cont <- data.frame(control = MODEL_LIST$call$control)
  
  # preProcess
  if ( is.null(names(MODEL_LIST$preProcess$method))){
    prep <- "NA"
  } else prep <- names(MODEL_LIST$preProcess$method)
  
  # tuneGrid
  if ( is.null(MODEL_LIST$call$tuneGrid)){
    grid <- "NA"
  } else grid <- apply(MODEL_LIST$call$tuneGrid, 2, paste, collapse = ",") %>%
    stringr::str_split(string = ., pattern = ",") %>%
    lapply(., unique) %>%
    lapply(., paste, collapse = ",") %>%
    data.frame(.)
  
  
  names(grid) <- paste0("tuneGrid.", names(model_list[[1]]$call$tuneGrid))
  
  # 
  df <- data.frame(
    test_logLoss = log_loss(tp$flg, tp$yes)
    ,train_logLoss = min(model_list[[1]]$results[,model_list[[1]]$metric])
    ,method = MODEL_LIST$method
    ,elapsed = MODEL_LIST$times$everything["elapsed"]
    ,data_preProcess = data_preProcess
    ,caret_preProcess = data.frame(caret_preProcess = paste(prep, collapse = ", "))
    ,length_exp = length(explanation_variable)
    ,exp = data.frame(exp = paste(explanation_variable, collapse = ", "))
    ,bestTune = MODEL_LIST$bestTune
    ,grid
    ,cont
    ,row.names = NULL
    ,stringsAsFactors = FALSE
  )
  
  return(df)
}

# data.frame の列が factor のものを、character に変換する関数
tf <- function(DF){
  variable.is.factor <- sapply(DF, is.factor)
  variable.factor <- names(variable.is.factor[variable.is.factor == TRUE])
  DF <- dplyr::mutate_each_(DF, dplyr::funs(as.character), variable.factor)
  return(DF)
}

# Dummy 化された data.frame の不要な列名を排除する関数
exclude_variables <- function(variables){
  id <- NULL
  for(i in variables){
    id <- c(id, grep(i, names(TRAIN)))
  }
  return(names(TRAIN[, -id]))
}

#
my.summary <- function(data, lev = NULL, model = NULL){
  if(is.character(data$obs))
    data$obs <- factor(data$obs, levels = lev)
  conf.mat <- table(data$pred, data$obs)
  prec <- conf.mat[1,1]/sum(conf.mat[1,]) # Precision
  rec <- conf.mat[1,1]/sum(conf.mat[,1]) # Recall
  acc <- sum(diag(conf.mat))/sum(conf.mat) # Accuracy
  res <- c(`Precision` = prec, `Recall` = rec,
           `F-Value` = 2 * prec * rec/(prec + rec), `Accuracy` = acc
           )
  res
}

LogLoss <- function (data, lev = NULL, model = NULL) 
{
  probs <- pmax(pmin(as.numeric(data$T), 1 - 1e-15), 1e-15)
  logPreds <- log(probs)        
  log1Preds <- log(1 - probs)
  real <- (as.numeric(data$obs) - 1)
  out <- c(mean(real * logPreds + (1 - real) * log1Preds)) * -1
  names(out) <- c("LogLoss")
  out
}
