#---------------------------------------
# This script sets out to pull and 
# visualise some AEMO price and demand
# data
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 13 April 2021
#---------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(lubridate)
library(xts)
library(janitor)

# Helper colour palettes

mypal <- c("RRP" = "#bc5090",
           "Total Demand" = "#ffa600")

forpal <- c("Historical Training Data" = "#003f5c",
           "Hold-Out Test Data" = "#bc5090",
           "Model Forecasts" = "#ffa600")

#--------------------- Webscrape data ---------------

#' Function to pull AEMO data into tempfiles and prepare it for visualisation and analysis
#' 
#' @param min_year the minimum year's data to pull. Should be >= 1998 and defaults to 1998
#' @param max_year the maximum year's data to pull. Should be <= 2021 and defaults to 2021
#' @param latest_month the last month of max_year to pull. Should be <= 12 and defaults to 04 as the year 2021 is only in April
#' @return a dataframe in tidy format ready for visualisation and analysis
#' @author Trent Henderson
#' 

grab_aemo_data <- function(min_year = 1999, max_year = 2021, latest_month = 04){
  
  if(!is.numeric(min_year) | !is.numeric(max_year) | !is.numeric(latest_month)){
    stop("min_year, max_year, and latest_month input parameters should all be numeric")
  }
  
  if(min_year < 1999 | max_year > 2021){
    stop("min_year should be an integer >= 1999 and max_year should be an integer <= 2021")
  }
  
  # Create a vector of concatenated years and months for user-specified time period to match AEMO file naming
  
  the_dates <- data.frame(dates = seq(from = as.Date(paste0(min_year,"-01-01")), to = as.Date(paste0(max_year,"-",latest_month,"-01")) , by = "days")) %>%
    mutate(year = as.numeric(gsub("-.*", "\\1", dates)),
           month = sub("^([^-]*-[^-]*)-.*", "\\1", dates),
           month = as.numeric(gsub(".*-", "\\1", month)),
           month = ifelse(month <10,paste0("0",month),as.character(month)),
           concat = as.numeric(paste0(year,month)))
  
  # Cycle through user-specified years and months to get dynamic filenames
  
  concat <- unique(the_dates$concat)
  storage <- list()
  
  for(i in concat){
    
    ex_month <- as.numeric(substr(i,(nchar(i)+1)-2,nchar(i)))
    
    temp <- tempfile()
    url <- paste0("https://aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND_",i,"_NSW1.csv")
    download.file(url, temp, mode = "wb")
    
    tmp <- read.csv(temp) %>%
      clean_names() %>%
      mutate(settlementdate = gsub("/", "-", settlementdate)) %>% # Catch '/' instead of '-' time format difference
      mutate(year = as.numeric(gsub("-.*", "\\1", settlementdate)),
             month = sub("^([^-]*-[^-]*)-.*", "\\1", settlementdate),
             month = as.numeric(gsub(".*-", "\\1", month))) %>%
      mutate(settlementdate = ifelse(nchar(settlementdate) == 16, paste0(settlementdate,":00"), settlementdate)) %>% # Catch missing :00
      mutate(settlementdate = ymd_hms(settlementdate, tz = "Australia/Brisbane")) %>%
      filter(month == ex_month)
    
    if(any(is.na(tmp$settlementdate))){
      message(paste0("NAs detected in: ",unique(tmp$year),"-",unique(tmp$month)))
    }
    
    storage[[i]] <- tmp
  }
  
  outs <- data.table::rbindlist(storage, use.names = TRUE)
  return(outs)
}

nsw <- grab_aemo_data(min_year = 1999, max_year = 2021, latest_month = 04)

#--------------------- Visualise data ---------------

# Overall

nsw %>%
  pivot_longer(cols = c("totaldemand", "rrp"), names_to = "quantity", values_to = "values") %>%
  mutate(quantity = ifelse(quantity == "rrp", "RRP", "Total Demand")) %>%
  ggplot(aes(x = settlementdate, y = values)) +
  geom_line(aes(colour = quantity), size = 1.15) +
  labs(title = "Raw time series of price and total demand for NSW",
       x = "Date",
       y = "Value",
       colour = "Quantity Type") +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  facet_wrap(~quantity, dir = "v", scales = "free_y")

# Just most recent month to visualise seasonality

nsw %>%
  filter(settlementdate >= as.Date("2021-04-01")) %>%
  pivot_longer(cols = c("totaldemand", "rrp"), names_to = "quantity", values_to = "values") %>%
  mutate(quantity = ifelse(quantity == "rrp", "RRP", "Total Demand")) %>%
  ggplot(aes(x = settlementdate, y = values)) +
  geom_line(aes(colour = quantity), size = 1.15) +
  labs(title = "Raw time series of price and total demand for NSW",
       x = "Date",
       y = "Value",
       colour = "Quantity Type") +
  scale_y_continuous(labels = comma) +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  facet_wrap(~quantity, dir = "v", scales = "free_y")

#--------------------- Build forecaster -------------

#' Function to split time series data into train and hold-out sets
#' 
#' @param data the dataframe to pull data from
#' @param test_start the datetime from which the test window should start
#' @return a list object with a dataframe from train and a dataframe for test
#' @author Trent Henderson
#' 

create_traintest_data <- function(data, test_start = "2019-01-01 00:30:00"){
  
  test_start <- ymd_hms(test_start, tz = "Australia/Brisbane")
  
  # Compute windowed train and test sets
  
  train <- data %>%
    filter(settlementdate < test_start)
  
  test <- data %>%
    filter(settlementdate >= test_start)
  
  storage <- list(train,test)
  
  return(storage)
}

train <- create_traintest_data(data = nsw, test_start = "2019-01-01 00:30:00")[[1]]
test <- create_traintest_data(data = nsw, test_start = "2019-01-01 00:30:00")[[2]]

# Check stationarity



# Set up data for Stan

stan_data <- list(N = nrow(train),
                  y = train$values,
                  N_forecast = nrow(test))

# Fit probabilistic forecast model

options(mc.cores = parallel::detectCores()) # Parallel processing for simultaneous chain computations

probmod <- stan(file = "stan/probmod.stan",
                data = stan_data,
                seed = 123,
                chains = 3,
                iter = 3000)

# Extract forecast estimates



# Compare predictions to actual test data



#--------------------- Produce graphics -------------

#------------------
# Model diagnostics
#------------------

# PPC

bayesplot::color_scheme_set("red")
bayesplot::ppc_dens_overlay(train$y, yrep = posterior_predict(probmod, draws = 100))

# Chain convergence

bayesplot::mcmc_trace(probmod)

# LOO-CV

loo1 <- loo::loo(probmod)
loo1
plot(loo1)

# Plot forecasts

train %>%
  ggplot() +
  geom_ribbon(data = forecasts, aes(x = settlementdate, ymin = lower, ymax = upper), fill = "#ffa600", alpha = 0.4) +
  geom_line(data = forecasts, aes(x = settlementdate, y = fit, colour = category)) +
  geom_line(aes(x = settlementdate, y = values, colour = category)) +
  geom_point(aes(x = settlementdate, y = values, colour = category), size = 1.5) +
  geom_line(data = test, aes(x = settlementdate, y = values, colour = category)) +
  geom_point(data = test, aes(x = settlementdate, y = values, colour = category), size = 1.5) +
  labs(title = "Probabilistic forecasts of AEMO data for NSW",
       subtitle = stringr::str_wrap("Probabilistic forecasts used an ARIMA model coded in Stan.", 
                                    width = 100),
       x = "Date",
       y = "Value",
       colour = NULL,
       caption = "Analysis: Orbisant Analytics") +
  scale_colour_manual(values = forpal) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

#--------------------- Multivariate tests -----------

# Graph demand as X and price as Y

nsw %>%
  ggplot(aes(x = totaldemand, y = rrp)) +
  geom_point(colour = "#003f5c", alpha = 0.8) +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Correlation between demand and price",
       x = "Total Demand",
       y = "RRP") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

# Graph demand as Xt-1 and price as Yt

laggeR::plot_ar_multiv(timeseriesx = nsw$totaldemand, timeseriesy = nsw$rrp, lags = c(1)) # devtools::install_github("hendersontrent/laggeR")

# Graph demand as Yy and price as Xt-1

laggeR::plot_ar_multiv(timeseriesx = nsw$rrp, timeseriesy = nsw$totaldemand, lags = c(1))

# Check for temporal dependence between RRP and Total Demand using transfer entropy

set.seed(123)
te <- RTransferEntropy::transfer_entropy(x = nsw$total_demand, y = nsw$rrp)
te
