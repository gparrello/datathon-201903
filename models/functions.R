pacman::p_load(
  "caret",
  "xgboost",
  "randomForest",
  "e1071",
  "gbm",
  "doMC"
)

do_modeling <- function(x, y, caret = FALSE){

  model <- c()
  
  # Train Control
  cvFoldNum <- 10
  cvRepeatNum <- 3
  cvFolds <- createMultiFolds(
    y,
    k = cvFoldNum,
    times = cvRepeatNum
  )
  
  fitControl <- trainControl(
    method = "repeatedcv",
    index = cvFolds,
    allowParallel = TRUE
  )
  
  if (caret == TRUE) {
      
    # Train
    tuneLength <- 1
    allmodels <- list(
      # "svmLinear",
      # "svmRadial",
      # "svmPoly",
      # "lm",
      # "knn",
      # "gbm",
      # "xgbTree",
      "rf"
    )
    
    for(m in allmodels){
      model[[m]] <- train(
        x = x,
        y = y,
        # data = trainingSet, # the problem is here!
        method = m,
        trControl = fitControl,
        tuneLength = tuneLength
      )
    }
  
  } else {
    
    # Train RF
    bestmtry <- tuneRF(
      x, y,
      ntreeTry = 100,
      stepFactor = 2,
      improve = 0.05,
      trace = TRUE,
      plot = FALSE,
      trControl = fitControl,
      allowParallel = TRUE
    )

    model[["rf"]] <- foreach(
      ntree=rep(100, 5),
      .combine = combine,
      .multicombine = TRUE,
      .packages = "randomForest"
    ) %dopar% {
      randomForest(
      y = y,
      x = x,
      importance = TRUE,
      method = "rf",
      ntree = ntree,
      mtry = bestmtry[[1]],
      trControl = fitControl
      )
    }
    
    temp_data <- as.data.frame(cbind(as.data.frame(x),as.data.frame(y)))
    model[["gbm"]] <- gbm(
      y ~ .,
      data = temp_data,
      n.trees = 500,
      cv.folds = 10,
      n.cores = 6
    )
    rm(temp_data)

  }
  
  return(model)

}

get_predictions <- function(predictors, model){
  
  predicted <- c()
  for(m in names(model)){
    p <- predict(model[[m]], predictors)
    predicted[[m]] <- p
  }
  
  return(predicted)
  
}

get_metrics <- function(predicted, real){
  
  metrics <- data.frame()
  for(m in names(predicted)){
    metric <- as.data.frame(postResample(predicted[[m]], real))
    colnames(metric) <- "value"
    metric$model <- m
    metrics <- rbind(metrics, metric)
  }
  vars <- row.names(metrics)
  metrics <- melt(metrics)
  metrics$variable <- vars
  
  return(metrics)

}


get_errors <- function(predicted, real){
  
  error <- c()
  for(m in names(predicted)){
    p <- predicted[[m]]
    if (is.numeric(p)) {
      e <- p - real
      e <- ggplot(data = as.data.frame(e)) +
        aes(x = e) +
        # geom_histogram(bins = 30, fill = '#0c4c8a') +
        geom_histogram(
          binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),
          fill = '#0c4c8a'
        ) +
        theme_minimal()
    } else {
      e <- confusionMatrix(p, real)
    }
    error[[m]] <- e
  }
  
  return(error)
  
}


get_time <- function(){
  t <- strftime(as.POSIXct(Sys.time()), "%Y%m%d%H%M")
  return(t)
}

save_model <- function(label, x, y){
  
  start_time <- get_time()
  model <- do_modeling(x, y)
  end_time <- get_time()
  
  filename1 <- paste(
    "./models/",
    label,
    "/",
    start_time,
    "_",
    end_time,
    sep=""
  )
  
  for (m in names(model)) {
    filename2 <- paste(filename1, "_", m, sep="")
    save(model, file = paste(filename2, ".rda", sep=""))
    saveRDS(model, file = paste(filename2, ".rds", sep=""))
  }
  
  return(model)
}

get_predictors <- function(data, predictors = c()){
  
  waps <- grep("WAP", names(data), value = TRUE)
  predictors <- c(waps, predictors)  
  
  return(predictors)

}
