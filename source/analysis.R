library(tidyverse)
library(ggplot2)


# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Read data
county_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# White percentage in 1988 jail rate
white_percentage_1988 <- county_level_data %>%
  select(year, total_jail_pop, white_jail_pop) %>% 
  filter(year == "1988") %>% 
  replace(is.na(.), 0) %>% 
  summarize(total_jail_pop = sum(total_jail_pop), white_jail_pop = sum(white_jail_pop)) %>% 
  mutate(percentage = 100 * white_jail_pop / total_jail_pop) %>% 
  pull(percentage)

# Average female jail population in 1988
avg_1988_female_jail_pop <- county_level_data %>%
  filter(year == "1988") %>%
  select(state, female_jail_pop) %>%
  group_by(state) %>%
  summarize(state_female_jail_pop = sum(female_jail_pop, na.rm = TRUE)) %>%
  summarize(avg_jail_pop = round(mean(state_female_jail_pop))) %>%
  pull(avg_jail_pop)

avg_1988_female_jail_pop <- prettyNum(avg_1988_female_jail_pop, big.mark = ",", scientific = FALSE)
avg_1988_female_jail_pop

# Black percentage in 1988 jail rate
black_percentage_1988 <- county_level_data %>%
  select(year, total_jail_pop, black_jail_pop) %>% 
  filter(year == "1988") %>% 
  replace(is.na(.), 0) %>% 
  summarize(total_jail_pop = sum(total_jail_pop), black_jail_pop = sum(black_jail_pop)) %>% 
  mutate(percentage = 100 * black_jail_pop / total_jail_pop) %>% 
  pull(percentage)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
library(dplyr)
library(ggplot2)

# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# Read data
jurisdiction_level_data<-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")


# This function calculates the total jail population per year in the US
get_year_jail_pop <- function() {
  jail_pop_df <- jurisdiction_level_data %>% 
    group_by(year) %>% 
    filter(total_jail_pop != "NA") %>% 
    summarise(total_jail_pop = sum(total_jail_pop)) 
  return(jail_pop_df)
}

# This function creates a chart of total jail population per year
plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), 
         aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Years", 
         y = "Total Jail Population",
         title = "Increase of Jail Population in U.S. (1970-2018)", 
         caption="The U.S. prison population, which had been increasing from 1980 to 2005, has been on a downward trend since 2010."
         )
}

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Jail Population by State 
states = c("WA", "OR", "CA", "UT", "AZ")
southern_states <- c("AL", "GA", "FL", "SC", "NC", "TX", "LA", "MS")

get_jail_pop_by_states <- function(states) {
  data <- county_level_data %>%
    select(year, state, total_jail_pop) %>%
    filter(state == state)
  return(data)
}

plot_jail_pop_by_states <- function (states) {
  ggplot(get_jail_pop_by_states(states), 
         aes(x = year, total_jail_pop, colour = state)) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018) By State") + 
   labs (
      x = "Year",
      y = "Total Jail Population",
      caption="The graph shows how the prison population has changed over time and varies by state, with California and New York having the highest number of admissions in most years."
      ) +
    geom_line()
}
plot_jail_pop_by_states()
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
get_white_pop_by_states <- function() {
  white<-county_level_data %>% 
    group_by(year, state) %>% 
    summarise(white_jail_pop = sum(white_jail_pop, na.rm = TRUE))
  return(white)
}

white_pop <- get_white_pop_by_states()

plot_white_pop_by_states <- function() {
  chart_white <- ggplot(white_pop, aes(x = year, y = white_jail_pop)) +
    geom_line(aes(color = state)) +
    ggtitle("White Population by States. (1970-2018)") +
    xlab("Year") +
    ylab("White Jail Population")
  labs(caption = "Increase of White Population in U.S by states. (1970-2018).")
  return(chart_white)
}

plot_white_pop_by_states()


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
library(usmap)

get_female_value <- function(){
  df<-county_level_data %>%
    filter(year == max(year)) %>%
    select(state, female_jail_pop, total_pop) %>%
    mutate(female_prop = female_jail_pop/total_pop * 100) %>%
    group_by(state) %>%
    summarise(female_prop = mean(female_prop, na.rm = TRUE))
  
  is.na(df)<-sapply(df, is.nan)
  df[is.na(df)]<-0
  return(df)
}

plot_female_value <- function(){
  df<-get_female_value() %>% filter(female_prop != is.na(female_prop))
  PL<- plot_usmap(region = "state", data = df, values = "female_prop") +
    scale_fill_continuous(
      na.value = "gray48", low = "pink", high = "yellow", name = "Percentage Female in Prison") +
    labs(title = "Female Population Ratio per State (2018)",
         )
  return(PL)
}
plot_female_value()
#----------------------------------------------------------------------------#

## Load data frame ---- 


