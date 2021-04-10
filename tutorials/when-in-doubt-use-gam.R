#---------------------------------------
# This script sets out to simulate some
# noisy data and fit a GAM and plot it
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 10 April 2021
#---------------------------------------

library(dplyr)
library(magrittr)
library(ggplot2)
library(mgcv)
library(tidymv)
library(Cairo)

# Simulate data

set.seed(123)

d <- data.frame(y = cumsum(rnorm(1000, mean = 0, sd = 1))) %>%
  mutate(x = row_number())

# Draw summary plot

CairoPNG("tutorials/output/raw-data.png", 800, 600)
d %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.8, colour = "steelblue2") +
  labs(title = "Raw data",
       x = "X",
       y = "Y") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()

# Fit GAM model

m1 <- gam(formula = y ~ s(x), data = d)
dev.expl <- round(summary(m1)$dev.expl, digits = 4)*100

# Predict new data and replot

model_p <- predict_gam(m1)

CairoPNG("tutorials/output/model-preds.png", 800, 600)
model_p %>%
  ggplot(aes(x, fit)) +
  geom_smooth_ci() +
  geom_point(data = d, aes(x = x, y = y), size = 0.8, colour = "steelblue2") +
  labs(title = "Raw data with model-predicted 95% confidence interval",
       subtitle = paste0("Model deviance explained: ",dev.expl,"%"),
       x = "X",
       y = "Y") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
dev.off()
