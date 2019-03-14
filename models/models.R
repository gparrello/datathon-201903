pacman::p_load(
  "data.table"
)

set.seed(123)
source('./models/functions.R')

data <- fread('./data/0-output/t.csv')
parts <- make_partition(data, data$datediff_next)

train <- parts[["train"]]
train$shop <- as.factor(train$shop)
attributes_to_be_used <- c(
  "shop",
  "billing",
  "month",
  "wday",
  "quantity"
)
predictors <- train[, ..attributes_to_be_used]
target <- train$datediff_next

model <- do_modeling(
  predictors,
  target
)

predicted <- get_predictions(predictors, model)
metrics <- get_metrics(predicted, target)
errors <- get_errors(predicted, target)
