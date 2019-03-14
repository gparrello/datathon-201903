pacman::p_load(
  "data.table"
)

set.seed(123)
source('./data/f.R')

data <- fread('./data/0-output/t.csv')
parts <- make_partition(data, data$datediff_next)

train <- parts[["train"]]
predictors <- train[, c("shop", "billing")]
target <- train$datediff_next

ans <- do_modeling(
  predictors,
  target
)

predicted <- get_predictions(predictors, ans)
metrics <- get_metrics(predicted, target)
errors <- get_errors(predicted, target)


# library("randomForest")

# tuneRF(
# x = predictors, y = target, ntreeTry=100,stepFactor=2,improve=0.05
# )
