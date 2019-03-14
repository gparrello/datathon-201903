pacman::p_load(
  "data.table",
  "dplyr",
  "lubridate"
)

files <- c(
  t = "X.csv",
  v = "X_val.csv"
)

orig <- c()
dt <- c()

for (f in names(files)) {
  file_to_read <- paste(sep = '', './data/1-input/', files[[f]])
  print(file_to_read)
  d <- fread(file_to_read)
  
  orig[[f]] <- d
  
  d <- head(d, 3000) # REMOVE THIS
  
  d$timestamp2 <- as.POSIXct(as.POSIXct(d$timestamp, '%Y-%m-%d %H:%M%S'))
  d$date <- lubridate::date(d$timestamp)
  
  # check quantity and billing for negative numbers, imputate with something
  
  # browser()
  d <- d %>%
    group_by(
      transaction,
      customer,
      chain,
      shop,
      seller,
      timestamp,
      date
    ) %>% 
    summarize(
      quantity = sum(quantity),
      billing = sum(billing)
    ) %>%
    arrange(
      customer,
      date
    )
  
  d <- as.data.table(d)
  d <- d[, datediff := c(0, diff(as.numeric(date))), customer]
  
  dt[[f]] <- d
}

train <- dt[["t"]]
# print(train)

write.csv(train, file = './test.csv')