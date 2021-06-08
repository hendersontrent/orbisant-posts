#---------------------------------------
# This script sets out to see how well
# a GAM can forecast the standard ausbeer
# dataset
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 12 April 2021
#---------------------------------------

library(dplyr)
library(ggplot2)
library(xts)
library(mgcv)

# Retrieve some data

d <- data.frame(beer = window(fpp::ausbeer, start = 1992, end = c(2007, 4)))

# Add quarter and year indicators

d1 <- d %>%
  mutate(date = seq(as.Date("1992-01-01"), length = nrow(d), by = "quarters")) %>%
  mutate(year = as.numeric(gsub("-.*", "\\1", date)),
         quarter = sub("^([^-]*-[^-]*)-.*", "\\1", date),
         quarter = as.numeric(gsub(".*-", "\\1", quarter)),
         quarter = case_when(
           quarter == 1  ~ 1,
           quarter == 4  ~ 2,
           quarter == 7  ~ 3,
           quarter == 10 ~ 4))

# Split historical and hold-out test sets

train <- d1 %>%
  filter(year < 2005) %>%
  mutate(category = "Historical Training Data")

test <- d1 %>%
  filter(year >= 2005) %>%
  mutate(category = "Hold-Out Test Data")

# Plot the time series

mypal <- c("Historical Training Data" = "#003f5c",
           "Hold-Out Test Data" = "#bc5090",
           "Model Forecasts" = "#ffa600")

train %>%
  ggplot() +
  geom_line(aes(x = date, y = beer, colour = category)) +
  geom_point(aes(x = date, y = beer, colour = category), size = 1.5) +
  geom_line(data = test, aes(x = date, y = beer, colour = category)) +
  geom_point(data = test, aes(x = date, y = beer, colour = category), size = 1.5) +
  labs(title = "Historical train and hold-out test periods of quarterly ausbeer data",
       x = "Date",
       y = "Beer",
       colour = NULL,
       caption = "Data source: {fpp} R package. Analysis: Orbisant Analytics") +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Fit a GAM

m <- gam(formula = beer ~ s(quarter, k = 4, bs = "cc") + s(year),
         data = train,
         method = "REML")

# Produce forecasts

forecasts <- test

forecasts <- cbind(forecasts, as.data.frame(predict(m, newdata = test, se.fit = TRUE))) %>%
  mutate(lower = fit - (2*se.fit),
         upper = fit + (2*se.fit)) %>%
  mutate(category = "Model Forecasts")

# Re-produce original time series graphic

train %>%
  ggplot() +
  geom_ribbon(data = forecasts, aes(x = date, ymin = lower, ymax = upper), fill = "#ffa600", alpha = 0.4) +
  geom_line(data = forecasts, aes(x = date, y = fit, colour = category)) +
  geom_line(aes(x = date, y = beer, colour = category)) +
  geom_point(aes(x = date, y = beer, colour = category), size = 1.5) +
  geom_line(data = test, aes(x = date, y = beer, colour = category)) +
  geom_point(data = test, aes(x = date, y = beer, colour = category), size = 1.5) +
  labs(title = "Forecasted quarterly ausbeer data",
       subtitle = stringr::str_wrap("Forecasts produced by GAM with smooth term of annual trend and smooth term of quarterly seasonality.", 
                                    width = 100),
       x = "Date",
       y = "Beer",
       colour = NULL,
       caption = "Data source: {fpp} R package. Analysis: Orbisant Analytics") +
  scale_colour_manual(values = mypal) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
