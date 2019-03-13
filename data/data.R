pacman::p_load(
  "data.table",
  "dplyr"
)

source("./data_functions.R")

files <- c(
  train = "./data/trainingData.csv",
  validation = "./data/validationData.csv",
  test = "./data/testData.csv"
)

dt <- c()
# orig <- c()
longdt <- c()
check_list <- c()
factors <- c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "PHONEID", "USERID")

# Processing both datasets
for (s in names(files)) {
  
  d <- fread(files[[s]])
  # orig[[s]] <- d
  waps <- get_waps(d)
  
  # set 100 to NAs/-105
  for (j in seq_along(d[, ..waps])) {
    set(d, i = which(d[[j]] == 100), j = j, value = NA)
  }
  
  # convert attributes to factor
  d <- d[, (factors) := lapply(.SD, as.factor), .SDcols=factors]
  
  # remove rows and columns with 0 variance
  d <- remove_novar_cols(d)
  d <- remove_novar_rows(d)
  waps <- get_waps(d)
  
  # melt into long format
  melt_ids <- colnames(d[,(length(waps)+1):ncol(d)])
  l <- melt(d, id.vars = melt_ids, na.rm = TRUE)
  names(l)[names(l) == 'variable'] <- 'WAP'
  longdt[[s]] <- l

  # add top WAPs columns
  n <- 1
  for (k in 1:n) {
    tops <- apply(d[,..waps], 1, find_top_waps, k = k, names = FALSE)
    tops_names <- apply(d[,..waps], 1, find_top_waps, k = k, names = TRUE)
    d <- cbind(d, tops)
    d <- cbind(d, tops_names)
    rm(tops, tops_names)
  }
  rm(n)
  d$tops_names <- as.factor(d$tops_names)
  
  # set 100 to -105
  for (j in seq_along(d[, ..waps])) {
    set(d, i = which(is.na(d[[j]])), j = j, value = -105)
  }
  
  d$set <- s
  dt[[s]] <- d
  
  # rm(d, l)
}


# weight signal according to phone brand
common_columns <- Reduce(
  intersect, list(
    colnames(longdt[["train"]]),
    colnames(longdt[["validation"]]),
    colnames(longdt[["test"]])
  )
)
longdt[["common"]] <- rbind(
  longdt[["train"]][,..common_columns],
  longdt[["validation"]][,..common_columns],
  longdt[["test"]][,..common_columns]
)

devs <- longdt[["common"]] %>%
  group_by(PHONEID) %>%
  summarize(
    abs_dev = mean(value) - mean(l$value),
    rel_dev = 1+(mean(value) - mean(l$value))/abs(mean(l$value))
  )
for (dn in names(dt)) {
  d <- dt[[dn]]
  d <- merge(
    d, devs,
    by = "PHONEID"
  )
  waps <- get_waps(d)
  d <- d[, (waps) := lapply(.SD, function(x) x-abs_dev), .SDcols=waps]
  # dt[[dn]] <- d
}

# intersect
common_columns <- intersect(
  colnames(dt[["train"]]),
  colnames(dt[["validation"]])
)
dt[["common"]] <- rbind(
  dt[["train"]][,..common_columns],
  dt[["validation"]][,..common_columns]
)
dt[["common"]] <- remove_novar_cols(dt[["common"]])
dt[["common"]] <- remove_novar_rows(dt[["common"]])