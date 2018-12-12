setwd("/users/mateuszzezula/Box/1. First Semester/Programming for Data Science/final_project/")
library(pacman)
p_load(magrittr, ggplot2, stringr, dplyr, reshape2, tseries, zoo, maps, tidyverse, urbnmapr, plotly, Rd2md)

datasetIndex <- function() {
  main <- read.table("../final_project/Final/Zezula/README_SERIES_ID_SORT.txt", 
                      header = T, sep = ";", quote = "", row.names = NULL, 
                      fill = TRUE, stringsAsFactors = F)
  subjects <- read.csv("../final_project/Final/Zezula/subjects.csv", 
                       header = T, stringsAsFactors = F)
  return(list(main = main, subjects = subjects))
}

get.subject <- function(index, number) {
  subject <- index$subjects$Title[number]
  return(subject)
}

get.csv <- function(index, title, county, state) {
  search <- paste0(title, " ", county,", ", state)
  filter <- dplyr::filter(index$main, Title == search)
  clean <- stringr::str_replace_all(filter[, 1], "\\\\", "/")
  path <- str_trim(paste0("../final_project/raw_data/data/", clean), side = "right")
  
  output <- tryCatch(read.csv(path, 
                              header = T, 
                              stringsAsFactors = F), error = function(e) NULL) 
  return(output)
}

get.all.counties <- function(index, state) {
  reference.title <- index$subjects$Title[5]
  filt.1 <- dplyr::filter(index$main, grepl(reference.title, Title))
  filt.2 <- dplyr::filter(filt.1, grepl(state, Title))
  counties <- filt.2$Title %>%
    gsub(reference.title, "", .) %>% 
    gsub(state, "", .) %>% 
    gsub(",", "", .) %>% 
    str_trim(side = c("both"))
  return(counties)
}

get.filtered.counties <- function(index, state, year) {
  counties.all <- get.all.counties(index, state)
  counties.filt <- rep(NA, length(counties.all))
  for (i in 1:length(counties.all)) {
    df <- get.csv(index, "Income Inequality in", counties.all[i], state)
    if (is.null(df)) {
      county <- NA
    } else {
      df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
      if (all(year %in% df$DATE)) {
        county <- counties.all[i]
      } else {
        county <- NA 
      }
    }
    counties.filt[i] <- county
  }
  counties.filt <- counties.filt[!is.na(counties.filt)]
  return(counties.filt)
}

df.state.average <- function(index, title, state, year) {
  counties <- get.filtered.counties(index, state, year)
  vector <- rep(NA, length(counties))
  for (i in 1:length(counties)) {
    df <- get.csv(index, title, counties[i], state)
    if (is.null(df)) {
      vector[i] <- 0
    } else {
      df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
      vector[i] <- dplyr::filter(df, DATE == year)[, 2]
    }
  }
  return(mean(vector))
}

create.us.df <- function(index, title, year) {
  df <- setNames(data.frame(matrix(ncol = 3, nrow = length(state.abb))), c("id", "value", "region"))
  for (i in 1:length(state.abb)) {
    df[i, "id"] <- state.abb[i]
    df[i, "region"] <- tolower(state.name[i])
    df[i, "value"] <- df.state.average(index, title, state.abb[i], year)
  }
  return(df)
}

create.us.heatmap <- function(index, title, year) {
  df <- create.us.df(index, title, year)
  state.data <- map_data("state")
  df_map <- merge(state.data, df, sort = FALSE, by = "region")
  df_map <- df_map[order(df_map$order), ]
  
  map <- ggplot(data = df_map, mapping = aes(long, lat, group = group, fill = value)) +
    geom_polygon(color = NA) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    labs(title = paste0(title, " ", year), x = "Longitude", y = "Latitude", fill = "Value") +
    theme_minimal()
  return(map)
}

graph.diverging.lollipop <- function(index, title, year) {
  df <- create.us.df(index, title, year)
  plot <- df %>% 
    arrange(value) %>%
    mutate(region = factor(region, region)) %>%
    ggplot(aes(x = region, y = value)) +
    geom_segment( aes(x = region, xend = region, y = mean(df$value), yend = value), color = "black", size = 1) +
    geom_point(color = "black", size = 3, alpha = 0.6) +
    labs(title = paste0(title, " ", year), x = "Value", y = "State") +
    coord_flip() + 
    theme_minimal()
  return(plot)
}