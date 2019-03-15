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
dt_old <- c()
dt_new <- c()

for (f in names(files)) {
  file_to_read <- paste(sep = '', './data/1-input/', files[[f]])
  print(file_to_read)
  d <- fread(file_to_read)
  
  orig[[f]] <- d
  
  # d <- head(d, 3000) # REMOVE THIS
  
  d$timestamp2 <- as.POSIXct(as.POSIXct(d$timestamp, '%Y-%m-%d %H:%M%S'))
  d$date <- lubridate::date(d$timestamp)
  d$month <- lubridate::month(d$date)
  d$wday <- lubridate::wday(d$date)
  
  # check quantity and billing for negative numbers, imputate with something
  # check customers that do several transactions in the same day with little time in between each transaction
  
  d <- d %>%
    group_by(
      customer,
      shop,
      date,
      month,
      wday
    ) %>% 
    summarize(
      quantity = sum(quantity),
      billing = sum(billing),
      num_prod = n()
    ) %>%
    arrange(
      customer,
      date
    )
  
  d <- as.data.table(d)
  # d <- d[, datediff_last := c(0, diff(as.numeric(date))), customer]
  d <- d[, datediff_next := c(diff(as.numeric(date)), 0), customer]
  d <- d[, max_date := date == max(date), customer]
  old <- d[max_date == FALSE]
  new <- d[max_date == TRUE]
  # old$max_date <- NULL
  # new$max_date <- NULL
  
  # browser()
  
  # d$shop <- as.factor(d$shop)
  
  dt[[f]] <- d
  dt_old[[f]] <- old
  dt_new[[f]] <- new
  
  fwrite(d, file = paste(sep = '', './data/0-output/', f, '.csv'), row.names = FALSE)
  fwrite(old, file = paste(sep = '', './data/0-output/', f, '_old.csv'), row.names = FALSE)
  fwrite(new, file = paste(sep = '', './data/0-output/', f, '_new.csv'), row.names = FALSE)
  
}

y <- fread('./data/1-input/y.csv')
