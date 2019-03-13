pacman::p_load(
  "dplyr"
)

find_top_waps <- function(x, names = FALSE, k = 1){
  
  # from:
  # - https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
  # - https://stackoverflow.com/questions/17619782/how-to-find-the-largest-n-elements-in-a-list-in-r
  
  x <- sort(x, na.last = TRUE, decreasing = TRUE)
  if (names == TRUE) x <- names(x)
  x <- x[1:k]
  return(x)

}

remove_novar_cols <- function(df){
  
  # Remove columns (WAP) where all the values are equal
  uniquelength <- sapply(df, function(x) length(unique(x)))
  df <- subset(df, select = uniquelength>1)

  return(df)

}
  
remove_novar_rows <- function(df){
  
  # Remove rows (WAP) where all the values are equal
  waps <- grep("WAP", names(df), value = TRUE)
  keep <- apply(df[, ..waps], 1, function(x) length(unique(x[!is.na(x)])) != 1)
  df <- df[keep, ]
  
  return(df)

}

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

get_waps <- function(df){
  waps <- grep("WAP", names(df), value=TRUE)
  return(waps)
}