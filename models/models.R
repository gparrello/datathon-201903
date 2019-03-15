pacman::p_load(
  "data.table"
)

set.seed(123)
source('./models/functions.R')
source('./data/data.R')

data <- dt_old[["t"]]
# print(nrow(data))
sample_size <- 10000
sample <- data %>%
  # group_by(wday, month) %>%
  sample_n(sample_size)
# sample <- data
# parts <- make_partition(sample, sample$datediff_next)

train <- sample #parts[["train"]]
train$shop <- as.factor(train$shop)
# train$customer <- as.factor(train$customer)
attributes_to_be_used <- c(
  "shop",
  "num_prod",
  "quantity",
  "month",
  "wday"
)
predictors <- train[, ..attributes_to_be_used]

target <- train$datediff_next
model_days <- do_modeling(
  predictors,
  target
)
predicted <- get_predictions(predictors, model_days)
metrics <- get_metrics(predicted, target)
errors <- get_errors(predicted, target)

target <- train$billing
model_bill <- do_modeling(
  predictors,
  target
)
predicted <- get_predictions(predictors, model_bill)
metrics <- get_metrics(predicted, target)
errors <- get_errors(predicted, target)

test <- dt_new[["t"]] %>% arrange(customer)
# test <- as.data.table(test)
y <- y %>% arrange(customer)
yhat <- y
predictors <- test[, attributes_to_be_used]
predictors <- as.data.table(predictors)
is.data.table(predictors)
predictions <- get_predictions(predictors, model_days)$rf
yhat$date <- test$date + predictions
yhat$billing <- get_predictions(predictors, model_bill)$rf
yhat$correct1 <- y$date == yhat$date
yhat$correct2 <- abs(y$billing - yhat$billing) <= 10
yhat$correct <- yhat$correct1 & yhat$correct2
yhat <- as.data.table(yhat)
accuracy1 <- nrow(yhat[correct1 == TRUE])/nrow(yhat)
accuracy2 <- nrow(yhat[correct2 == TRUE])/nrow(yhat)
accuracy <- nrow(yhat[correct == TRUE])/nrow(yhat)
print(accuracy1)
print(accuracy2)
print(accuracy)

# data <- fread('./data/0-output/v.csv')
# predictors <- data[, ..attributes_to_be_used]
# target <- data$datediff_next
# data$datediff_next_ <- get_predictions(predictors, model_days)
