pacman::p_load(
  "data.table"
)

set.seed(123)
source('./models/functions.R')

data <- fread('./data/0-output/t.csv')
parts <- make_partition(data, data$datediff_next)

train <- parts[["train"]]
train$shop <- as.factor(train$shop)
predictors <- train[, c("shop", "billing")]
target <- train$datediff_next

ans <- do_modeling(
  predictors,
  target
)

predicted <- get_predictions(predictors, ans)
metrics <- get_metrics(predicted, target)
errors <- get_errors(predicted, target)
