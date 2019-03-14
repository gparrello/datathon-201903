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
  # check customers that do several transactions in the same day with little time in between each transaction
  
  # browser()
  d <- d %>%
    group_by(
      customer,
      chain,
      shop,
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
  d <- d[, datediff_last := c(0, diff(as.numeric(date))), customer]
  d <- d[, datediff_next := c(diff(as.numeric(date)), 0), customer]
  d <- d[, max_date := date == max(date), customer]
  d <- d[max_date == FALSE]
  d$max_date <- FALSE
  
  dt[[f]] <- d
  print(nrow(d))
  
  # write.csv(d, file = paste(sep = '', './data/0-output/', f, '.csv'), row.names = FALSE)

}

train <- dt[["t"]]
View(train)
