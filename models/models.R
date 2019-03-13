pacman::p_load(
  "caret",
  "doMC"
)

set.seed(123)
source("./data.R")
source("./model_functions.R")
source("./data_functions.R")


do_training <- function(label, y, added_predictor, sample, load_from_file = FALSE){
  
  # Prepare data
  data <- make_partition(sample, sample[[y]])
  # train <- data[2["train"]]
  # validation <- data[["test"]]
  train <- sample
  validation <- sample
  
  # Modeling
  print(paste("Training set size:", nrow(train)))
  if (load_from_file) {
    load(paste(folder, label, ".rda", sep=""))
  } else {
    data <- train
    predictors <- get_predictors(data, added_predictor)
    model <- save_model(label, data[predictors], data[[y]])
  }
  
  # Get metrics
  print(paste("Validation set size:", nrow(validation)))
  data <- validation
  predictors <- get_predictors(data, added_predictor)
  predicted <- get_predictions(data[predictors], model)
  metric <- get_metrics(predicted, data[[y]])
  error <- get_errors(predicted, data[[y]])
  # browser()
  
  result <- list(
    predicted = predicted,
    model = model,
    metric = metric,
    error = error
  )
  
  return(result)
  
}

sample_size <- 944
orig_sample <- dt[["common"]] %>%
  group_by(BUILDINGID, FLOOR) %>%
  sample_n(sample_size)

results <- c()

labels <- c(
  "building",
  "floor",
  "long",
  "lat",
  "long2"
)
predicted <- c(
  "BUILDINGID",
  "FLOOR",
  "LONGITUDE",
  "LATITUDE",
  "LONGITUDE"
)
added_predictors <- list(
  NULL,
  c("BUILDINGID"),
  c("BUILDINGID", "FLOOR"),
  c("BUILDINGID", "FLOOR", "LONGITUDE"),
  c("BUILDINGID", "FLOOR", "LATITUDE")
  # c("tops", "tops_names"),
  # c("BUILDINGID", "tops", "tops_names"),
  # c("BUILDINGID", "FLOOR", "tops", "tops_names"),
  # c("BUILDINGID", "FLOOR", "LONGITUDE", "tops", "tops_names"),
  # c("BUILDINGID", "FLOOR", "LATITUDE", "tops", "tops_names")
)
names(predicted) <- labels
names(added_predictors) <- labels
folder <- "./best/"
files_to_search <- unlist(lapply(labels, function(x) paste(x, ".rda", sep = "")))
load_from_file <- sort(list.files(folder)) == sort(files_to_search)

# Do the magic
# cores <- 6
# registerDoMC(cores = cores)
for(l in labels){
  results[[l]] <- do_training(
    l,
    predicted[[l]],
    added_predictors[[l]],
    # orig_sample,
    as.data.frame(dt[["common"]]),
    load_from_file = load_from_file
  )
}

# save metrics
metrics <- c()
for(r in names(results)){
  metrics[[r]] <- results[[r]]$metric
}  
metrics <- melt(metrics)
write.csv(metrics, file = paste("./metrics/", get_time(), ".csv", sep = ""))
