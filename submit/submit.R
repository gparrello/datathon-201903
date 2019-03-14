source('./submit/client.R')
df <- read.csv('./submit/3.csv')
config <- './submit/config.ini'
submit_predictions(config, df)