#---------------------------------------
# THis script sets out to produce
# time-series feature analysis of
# emissions data by country
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 23 March 2021
#---------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(data.table)
library(catch22)
library(caTools)
library(brms)
library(bayesplot)
library(Cairo)
library(broom)

#-------------------- Data loads -------------------------------

# Country list

country_list <- read_csv("hctsa/data/country_list.csv") %>%
  clean_names() %>%
  dplyr::select(c(country_name, three_letter_country_code, continent_name))

# Quantitative data

emissions <- read_excel("hctsa/data/owid-co2-data.xlsx") %>%
  clean_names()

#-------------------- Wrangling and processing -----------------

main_set <- emissions %>%
  inner_join(country_list, by = c("iso_code" = "three_letter_country_code")) %>%
  dplyr::select(c(iso_code, country, continent_name, year, co2_per_capita)) %>%
  filter(!is.na(iso_code)) # Removes continents and world aggregations

# Dictionary of just countries and continents

dictionary <- main_set %>%
  group_by(iso_code, country, continent_name) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  dplyr::select(-c(counter))

#-------------------- Spaghetti plot ---------------------------

#' Function to draw a 'spaghetti plot' where a country of interest is called out
#' @param iso_code the 3-digit code to identify a country
#' @param do_log Boolean of whether to log-scale the y-axis. Defaults to FALSE
#' @return an object of class ggplot containing the graphic
#' @author Trent Henderson
#' 

draw_lines <- function(iso_code = "AUS", do_log = FALSE){
  
  the_codes <- unique(main_set$iso_code)
  '%ni%' <- Negate('%in%')
  
  if(iso_code %ni% the_codes){
    stop("iso_code should be a single 3-digit code available in the main_set dataframe.")
  }
  
  if(length(iso_code) != 1){
    stop("iso_code should be a single 3-digit code available in the main_set dataframe.")
  }
  
  # Helper
  
  my_code <- iso_code
  
  # Parse data
  
  my_iso <- main_set %>%
    filter(iso_code == my_code)
  
  to_filter <- main_set %>%
    filter(iso_code != my_code) %>%
    group_by(iso_code, continent_name) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    dplyr::select(-c(counter)) %>%
    group_by(iso_code) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(counter > 1)
  
  others <- main_set %>%
    filter(iso_code != my_code) %>%
    filter(iso_code %ni% to_filter$iso_code)
  
  # Draw graphic
  
  if(do_log){
    
    p <- others %>%
      ggplot() +
      geom_line(aes(x = year, y = log(co2_per_capita), group = iso_code),
                colour = "grey50", alpha = 0.6, size = 0.1) +
      geom_line(data = my_iso, aes(x = year, y = log(co2_per_capita)),
                colour = "#ffa600", size = 0.8) +
      labs(title = "Time-series of log-scaled Co2 emissions per capita by country",
           x = "Year",
           y = "log(Co2 Emissions per Capita)")
  } else{
    
    p <- others %>%
      ggplot() +
      geom_line(aes(x = year, y = co2_per_capita, group = iso_code),
                colour = "grey50", size = 0.1) +
      geom_line(data = my_iso, aes(x = year, y = co2_per_capita),
                colour = "#ffa600", size = 0.8) +
      labs(title = "Time-series of Co2 emissions per capita by country",
           x = "Year",
           y = "Co2 Emissions per Capita")
  }
  
  p <- p +
    scale_x_continuous(limits = c(1800,2020)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")
  
  return(p)
}

CairoPNG("hctsa/output/ts_plot.png",800,600)
draw_lines(iso_code = "AUS", do_log = FALSE)
dev.off()

CairoPNG("hctsa/output/ts_plot_log.png",800,600)
draw_lines(iso_code = "AUS", do_log = TRUE)
dev.off()

#-------------------- Time-series feature calculations ---------

# Remove duplicate countries and ones with short time-series (need a proper workaround)

remove_dups <- function(){
  
  '%ni%' <- Negate('%in%')
  
  to_filter <- main_set %>%
    group_by(iso_code, continent_name) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    dplyr::select(-c(counter)) %>%
    group_by(iso_code) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(counter > 1)
  
  retained <- main_set %>%
    filter(iso_code %ni% to_filter$iso_code)
  
  max_years <- max(main_set$year)-min(main_set$year)
  
  length_check <- main_set %>%
    filter(iso_code %in% retained$iso_code) %>%
    group_by(country, year) %>%
    summarise(counter = n()) %>%
    group_by(country) %>%
    summarise(counter = sum(counter)) %>%
    ungroup() %>%
    filter(counter > 30)
  
  message("Returning list of countries with >= 30 years' of data.")
  
  keepers <- unique(length_check$country)
  return(keepers)
}

countries <- as.vector(remove_dups())
storage <- list()

# Loop through each country to get feature vectors

for(i in countries){
  
  tmp <- main_set %>%
    filter(country == i) %>%
    arrange(year)
  
  x <- tmp$co2_per_capita
  
  calcs <- catch22_all(x) %>%
    mutate(country = i) %>%
    left_join(dictionary, by = c("country" = "country"))
  
  storage[[i]] <- calcs
}

outs <- rbindlist(storage, use.names = TRUE)

#-------------------- Data visualisation -----------------------

# Data quality check

CairoPNG("hctsa/output/data-quality.png",800,600)
plot_quality_matrix(outs)
dev.off()

# Render heatmap graphic

CairoPNG("hctsa/output/feature_matrix.png",800,600)
plot_feature_matrix(outs, is_normalised = FALSE, id_var = "country", method = "RobustSigmoid")
dev.off()

CairoPNG("hctsa/output/pca.png",800,600)
plot_low_dimension(outs, is_normalised = FALSE, id_var = "country", group_var = "continent_name", method = "RobustSigmoid", plot = TRUE)
dev.off()
