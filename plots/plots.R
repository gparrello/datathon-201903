pacman::p_load(
  "data.table",
  "ggplot2",
  "ggridges",
  "rbokeh"
  # "plotly"
)

source("./data.R")
source("./data_functions.R")
df <- dt[["train"]]
p <- ggplot(data = df) +
  aes(x = LONGITUDE, y = LATITUDE, color = TIMESTAMP) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(FLOOR))

# plot density functions for all WAPs for each building
density_plots <- c()
bar_plots <- c()
ridges <- c()
outliers <- c()
lowest_x <- 0
highest_x <- 0
for (s in names(longdt)){
  dt <- longdt[[s]][which(!is.na(longdt[[s]]$value)), ]
  dt$FLOOR <- as.factor(dt$FLOOR)
  dt$BUILDING <- as.factor(dt$BUILDING)
  lowest_x <- min(dt$value, lowest_x)
  highest_x <- max(dt$value, highest_x)

  density_plots[[s]] <- ggplot(data = dt) +
    aes(x = value) +
    geom_histogram(bins = 40, fill = '#0c4c8a') +
    labs(title = 'Signal for all WAPs',
         x = 'Signal',
         y = 'Frequency',
         subtitle = 'Separated by building and floor') +
    theme_minimal() +
    xlim(lowest_x,highest_x) +
    # facet_wrap(vars(BUILDINGID))
    facet_grid(rows = vars(FLOOR), cols = vars(BUILDINGID))

  bar_plots[[s]] <- ggplot(data = dt) +
    aes(x = WAP) +
    geom_bar(fill = '#0c4c8a') +
    labs(title = 'All WAPs',
         x = 'WAP',
         y = 'Frequency',
         subtitle = 'Separated by building') +
    theme_minimal() +
    facet_grid(rows = vars(FLOOR), cols = vars(BUILDINGID))
  
  ridges[[s]] <- ggplot(dt, aes(x = value, y = WAP, fill = FLOOR)) +
    geom_density_ridges(scale = 10) +
    labs(title = 'Signal for each WAP',
         x = 'Signal',
         y = 'Frequency',
         subtitle = 'Separated by building') +
    theme_ridges() +
    scale_colour_viridis_d(option  = "magma") +
    theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
    xlim(lowest_x,highest_x) +
    # facet_grid(rows = vars(FLOOR), cols = vars(BUILDINGID))
    # facet_wrap(vars(BUILDINGID))
    facet_wrap(vars(FLOOR))
  
  aggdt <- dt %>%
    group_by(WAP) %>%
    summarize(
      signal = mean(value),
      obs = n()
    )
  # browser()
  outliers[[s]] <- figure() %>%
    ly_points(aggdt, x=WAP, y=signal, color=obs, hover = c(WAP, signal, obs)) %>%
    set_palette(continuous_color = pal_color(c("blue", "green", "yellow", "orange", "red", "purple")))
  
  # rm(dt, aggdt)
}

# plot different ridges each containing 25 WAPs
l <- longdt[["train"]]
l2 <- l[!which(is.na(l$value)), ]

waps <- unique(l$WAP)
n <- ceiling(length(waps)/25)
ridges2 <- c()
for(i in 1:n){
  lowest_x <- min(l[which(!is.na(l$value)),]$value)
  highest_x <- max(l[which(!is.na(l$value)),]$value)
  first_wap <- 1+(i-1)*25
  last_wap <- 25+(i-1)*25
  waps_subset <- waps[first_wap:last_wap]
  waps_subset <- waps_subset[!is.na(waps_subset)]
  ridges2[[i]] <- ggplot(subset(l, WAP %in% waps_subset), aes(x = value, y = WAP)) +
    geom_density_ridges(scale = 10) +
    xlim(lowest_x,highest_x) +
    facet_wrap(vars(BUILDINGID))
}
# rm(l, waps, n)
