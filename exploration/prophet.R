#---------------------------------------
# This script sets out to fit a Gaussian
# Process model to Prophet's time-series
# examples to assess feasibility
#
# NOTE: Broadly follows this presentation 
# https://juanitorduz.github.io/gaussian_process_time_series/
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 22 April 2021
#---------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(reticulate)

# Read in data from Prophet's github

df <- read_csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv")

#----------------- Graph time series ----------

df %>%
  mutate(ds = as.Date(ds)) %>%
  ggplot(aes(x = ds, y = y)) +
  geom_point(size = 1.25) +
  labs(title = "{prophet} example data of log daily Wikipedia page views for Peyton Manning",
       x = "Date",
       y = "Log Daily Page Views")

#----------------- Fit GP model ---------------

# Convert datetime to integer for easy NumPy integration

df1 <- df %>%
  mutate(dateindex = row_number())

dates <- df1 %>%
  dplyr::select(c(ds, dateindex))

reticulate::use_python("~/opt/anaconda3/bin/python", required = TRUE)
reticulate::source_python("/Users/trenthenderson/Documents/Git/orbisant-posts/exploration/gp.py")

# Fit GP

gp <- init_gp()
m <- fit_gp(spec = gp, X = df1$dateindex, y = df1$y)

# Extract prior samples

priors <- sample_gp_prior()

priors2 <- as.data.frame(priors) %>%
  tidyr::pivot_longer(tidyr::everything(), names_to = "iter", values_to = "y") %>%
  group_by(iter) %>%
  mutate(dateindex = row_number()) %>%
  ungroup() %>%
  left_join(dates, by = c("dateindex" = "dateindex"))

# Generate predictions

preds <- predict_gp(model = m, X = df1$dateindex)

#----------------- Graph GP model -------------

# Samples from prior distribution

df %>%
  mutate(ds = as.Date(ds)) %>%
  ggplot(aes(x = ds, y = y)) +
  geom_line(data = priors2, colour = "steelblue2", alpha = 0.7) +
  geom_point(size = 1.25) +
  labs(title = "100 samples from Gaussian process prior distribution",
       subtitle = "GP kernel comprised of summation of white noise + exponential sine squared kernels.",
       x = "Date",
       y = "Log Daily Page Views",
       caption = "Analysis: Orbisant Analytics")

# Predictions


