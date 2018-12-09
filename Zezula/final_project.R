# The U.S. Regional Dataset from the St. Louis Federal Reserve
# (link: https://fred.stlouisfed.org/categories/3008/downloaddata) contains time-series
# economic data for each country in the United States. The dataset amounts to 330,000 .csv
# files and over 500 megabytes in uncompressed form.
# 
# Although the analytical possibilities of the dataset are endless, I limit myself
# to answering the following:
# 
# 1. How do changes in economic variables vary from state-to-state?
# Display those changes using a heatmap. Which states faced greatest progress?
# For instance, which state had the greatest increase in per capita personal income?
# 2. How does ethnic changes vary across states and counties? Do there exist plausible
# correlations between the demographic variables and other economic variables?
# 
# The creation of functions that answer the above questions presupposes the existence of
# many supporting functions; these will be explained on an ad hoc basis.

setwd("/users/mateuszzezula/Box/1. First Semester/Programming for Data Science/")
library(pacman)
p_load(magrittr, ggplot2, stringr, dplyr, reshape2, tseries, zoo, maps, tidyverse, urbnmapr, plotly, Rd2md)

datasetIndex <- function() {
  main <- read.table("../Programming for Data Science/final_project/data_clean/README_SERIES_ID_SORT.txt", 
                      header = T, sep = ";", quote = "", row.names = NULL, 
                      fill = TRUE, stringsAsFactors = F)
  subjects <- read.csv("../Programming for Data Science/final_project/subjects.csv", 
                       header = T, stringsAsFactors = F)
  return(list(main = main, subjects = subjects))
}
head(datasetIndex()$main)
# The dataset includes an index file outlining every .csv file contained in the dataset.
# Part of the index file is shown for illustrative purposes; the format and information is 
# self-explanatory.

head(datasetIndex()$subjects)
# I manually parsed the index file and identified 33 variables of analytical interest. 
# Any variable included in the list can be analyzed with the extant functions.

# Self-explanatory (ensures the effective parsing of the entire databsase), 
# but a foundational function notwithstanding.
get.csv <- function(index, title, county, state) {
  search <- paste0(title, " ", county,", ", state)
  filter <- dplyr::filter(index$main, Title == search)
  clean <- stringr::str_replace_all(filter[, 1], "\\\\", "/")
  path <- paste0("../Programming for Data Science/final_project/raw_data/data/", clean)

  output <- read.csv(path, 
                     header = T, 
                     stringsAsFactors = F)
  return(output)
}

# Calculates the compound annual growth rate
calc.cagr <- function(df, start.year, end.year) {
  df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
  start.value <- dplyr::filter(df, DATE == start.year)[, 2]
  end.value <- dplyr::filter(df, DATE == end.year)[, 2]
  years <- end.year - start.year
  calc <- (end.value/start.value)**(1/years)-1
  return(calc)
}

# Returns counties for a particular state; includes those that would return errors
# (Errors likely caused by mislabelling or .csv inconsistency. Fixing rather than ommitting such
# errors are beyond the scope of this project.)
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

# Returns counties that will not throw error in get.csv() function.
get.filtered.counties <- function(index, state, year) {
  counties.all <- get.all.counties(index, state)
  counties.filt <- rep(NA, length(counties.all))
  for (i in 1:length(counties.all)) {
    df <- tryCatch(get.csv(index, "Income Inequality in", counties.all[i], state), 
                   error = function(e) NULL) 
    # Assuming Income Inequality counties have data for other variables
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

# Capitalizes the first letter of a string; makes graphs more aesthetically pleasing
string.cap <- function(string) {
  x <- strsplit(string, " ")[[1]]
  paste(toupper(substring(x, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}


df.cagr <- function(df1, df2, years) {
  cagr <- (df2$value/df1$value)**(1/years)-1
  subregion <- tolower(df1$county) %>% 
    gsub("county", "", .) %>% 
    str_trim(side = c("right"))
  df <- cbind.data.frame(subregion, cagr, 
                         stringsAsFactors = FALSE)
  return(df)
}

# Calculates the average of a particular economic variable for a particular year.
df.state.average <- function(index, title, state, year) {
  counties <- get.filtered.counties(index, state, year)
  vector <- rep(NA, length(counties))
  for (i in 1:length(counties)) {
    df <- get.csv(index, title, counties[i], state)
    df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
    vector[i] <- dplyr::filter(df, DATE == year)[, 2]
  }
  return(mean(vector))
}

df.county.level <- function(index, title, state, year) {
  counties <- get.filtered.counties(index, state, year)
  df_county <- setNames(data.frame(matrix(ncol = 2, nrow = length(counties))), c("county", "value"))
  for (i in 1:nrow(df)) {
    df <- get.csv(index, title, counties[i], state)
    df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
    df_county[i, "county"] <- counties[i]
    df_county[i, "value"] <- dplyr::filter(df, DATE == year)[, 2]
  }
  return(na.omit(df_county))
}


df.ethnicity.series <- function(index, state) {
  ethnicity.vector <- get.available.titles()[21:25]
  ethnicity.name <- c("White", "Latino", "Asian", "Indian", "Black")
  year <- 2010:2016
  df <- setNames(data.frame(matrix(ncol = length(ethnicity.name), nrow = length(year))), ethnicity.name)
  for (i in 1:length(ethnicity.name)) {
    for (a in 1:length(years)) {
      df[a, i] <- df.state.sum(index, ethnicity.vector[i], state, year[a])
      #browser()
    }
  }
  df <- cbind(df, Year = year)
  return(df)
}

df.state.sum <- function(index, title, state, year) {
  counties <- get.filtered.counties(index, state, year)
  vector <- rep(NA, length(counties))
  for (i in 1:length(counties)) {
    df <- get.csv(index, title, counties[i], state)
    df$DATE <- as.numeric(gsub("-01-01", "", df$DATE))
    vector[i] <- dplyr::filter(df, DATE == year)[, 2]
    browser()
  }
  return(sum(vector))
}

create.us.df <- function(index, title, year) {
  df <- setNames(data.frame(matrix(ncol = 3, nrow = length(state.abb))), c("id", "value", "region"))
  for (i in 1:length(state.abb)) {
    df[i, "id"] <- state.abb[i]
    df[i, "value"] <- df.state.average(index, title, state.abb[i], year)
    df[i, "region"] <- tolower(state.name[i])
  }
  return(df)
}


# Creates a heatmap; dynamic function that allows the analysis of each of the 33 variables.
# (Alaska and Hawaii are excluded given coding difficulties.)
create.us.heatmap <- function(df) {
  state.data <- map_data("state")
  df_map <- merge(state.data, df, sort = FALSE, by = "region")
  df_map <- df_map[order(df_map$order), ]
  
  map <- ggplot(data = df_map, mapping = aes(long, lat, group = group, fill = value)) +
    geom_polygon(color = NA) + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    labs(title = "[placeholder]", x = "Longitude", y = "Latitude", fill = "Value") +
    theme_minimal()
  return(map)
}

create.state.heatmap <- function() {
  df_cagr <- df.cagr(df1, df2, 5)
  county.data <- map_data("county")
  df_test <- merge(county.data, df_cagr, sort = FALSE, by = "subregion")
  
  map.1 <- ggplot(data = df_test, mapping = aes(long, lat, group = group, fill = cagr)) +
    geom_polygon() + 
    geom_polygon(data = state_df, colour = "white", fill = NA) +
    labs(title = "[placeholder]", x = "Longitude", y = "Latitude", fill = "CAGR") +
    theme_minimal()
  map.1
}

# Returns a diverging lollipop graph, allowing for an alternative viewing of data
graph.diverging.lollipop <- function(df) {
  plot <- df %>% 
    arrange(value) %>%
    mutate(region = factor(region, region)) %>%
    ggplot(aes(x = region, y = value)) +
    geom_segment( aes(x = region, xend = region, y = mean(df_test$value), yend = value), color = "black", size = 1) +
    geom_point(color = "black", size = 3, alpha = 0.6) +
    labs(title = paste0(title, "..."), x = "Value", y = "State") +
    coord_flip() + 
    theme_minimal()
  return(plot)
}


# area chart of race
/