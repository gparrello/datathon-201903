pacman::p_load(
  "caret",
  "doMC",
  "randomForest"
)

make_partition <- function(data, dependent){
  
  sets <- c()
  in_training <- createDataPartition(
    dependent,
    p = .75,
    list = FALSE
  )
  sets[["train"]] <- data[in_training,]
  sets[["test"]] <- data[-in_training,]
  
  return(sets)
  
}

do_modeling <- function(x, y){
  
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
    index = cvFolds
  )
  
  # Train RF
  bestmtry <- tuneRF(
    x, y,
    ntreeTry = 100,
    stepFactor = 2,
    improve = 0.05,
    trace = TRUE,
    trControl = fitControl
  )
  
  browser()
  mtry <- 0
  
  model[["rf"]] <- randomForest(
    y = y,
    x = x,
    importance = TRUE,
    method = "rf",
    ntree = 100,
    mtry = mtry,
    trControl = fitControl
  )
  
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

# for revision, do not use!
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
