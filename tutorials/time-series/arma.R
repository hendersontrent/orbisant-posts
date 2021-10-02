#-------------------------------------------
# This script sets out to produce a basic
# Bayesian ARMA model in Stan
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 28 September 2021
#-------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(rstan)

#------------- Simulate some ARMA data --------------

set.seed(123)
y <- arima.sim(model = list(ar = 0.8, ma = 0.2), n = 1000)

# Plot it

p <- data.frame(y = y) %>%
  mutate(x = row_number()) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Our simulated ARMA data",
       x = "Time",
       y = "Value") +
  theme_bw()

print(p)

ggsave("tutorials/time-series/arma.png", p)

#------------- Specify Bayesian model ---------------

m <- "
data {
  int<lower=1> N;
  real y[N];
}
parameters {
  real mu;
  real<lower = -1, upper = 1> phi;
  real<lower = -1, upper = 1> theta;
  real<lower=0> sigma;
}
model {
  // Model specification 
  
  vector[N] nu;
  vector[N] epsilon;
  nu[1] = mu + phi * mu;
  epsilon[1] = y[1] - nu[1];
  
  for (n in 2:N) {
    nu[n] = mu + phi * y[n-1] + theta * epsilon[n-1];
    epsilon[n] = y[n] - nu[n];
  }
  
  // Priors
  
  mu ~ normal(0, 10);
  phi ~ normal(0, 2);
  theta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  epsilon ~ normal(0, sigma);
}
"

#------------- Fit the model --------------

# Set up data for Stan

stan_data <- list(N = length(y),
                  y = as.vector(y))

# Fit model

options(mc.cores = parallel::detectCores())

mod <- stan(data = stan_data, 
            model_code = m,
            iter = 3000,
            chains = 3,
            seed = 123)

# Plot posterior distributions

true_params <- data.frame(parameter = c("phi", "theta"),
                          value = c(0.8, 0.2))

p1 <- as.data.frame(mod) %>%
  dplyr::select(c(phi, theta)) %>%
  mutate(iteration = row_number()) %>%
  pivot_longer(cols = 1:2, names_to = "parameter", values_to = "values") %>%
  ggplot(aes(x = values)) +
  stat_halfeye(fill = "grey90", .width = c(0.80, 0.95)) +
  geom_vline(data = true_params, aes(xintercept = value), lty = "dashed", size = 0.7) +
  labs(title = "Parameter posterior distributions",
       subtitle = "Vertical dashed lines represent true parameter values of phi = 0.8, and theta = 0.2.",
       x = "Value",
       y = "Posterior density") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 10)) +
  facet_wrap(~parameter, scales = "free")

print(p1)

ggsave("tutorials/time-series/posteriors.png", p1)
