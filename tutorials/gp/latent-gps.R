#-------------------------------------------
# This script sets out to run some basic
# latent GP models
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 22 September 2021
#-------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(rstan)
library(titanic)

#--------------------- Binomial example -------------------

# Clean up datasets

tmpTrain <- titanic_train %>%
  dplyr::select(c(Survived, Fare)) %>%
  drop_na()

tmpTest <- titanic_test %>%
  dplyr::select(c(PassengerId, Fare)) %>%
  drop_na()

# Specify model

m <- "
data {
  int<lower=1> N1;
  real x1[N1];
  int<lower=0, upper=1> y1[N1];
  int<lower=1> N2;
  real x2[N2];
}

transformed data {
  real delta = 1e-9;
  int<lower=1> N = N1 + N2;
  real x[N];
  for (n1 in 1:N1) x[n1] = x1[n1];
  for (n2 in 1:N2) x[N1 + n2] = x2[n2];
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real a;
  vector[N] eta;
}

transformed parameters {

  vector[N] f;
  {
    matrix[N, N] L_K;
    matrix[N, N] K = cov_exp_quad(x, alpha, rho);

    // Add small number to matrix diagonal for numerical stability
    
    for (n in 1:N)
      K[n, n] = K[n, n] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}

model {
  
  // Priors

  rho ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  a ~ std_normal();
  eta ~ std_normal();
  
  // Likelihood

  y1 ~ bernoulli_logit(a + f[1:N1]);
}

generated quantities {
  int y2[N2];
  
  for (n2 in 1:N2)
    y2[n2] = bernoulli_logit_rng(a + f[N1 + n2]);
}
"

# Set up data for Stan

stan_data <- list(N1 = nrow(tmpTrain),
                  y1 = tmpTrain$Survived,
                  x1 = tmpTrain$Fare,
                  N2 = nrow(tmpTrain),
                  x2 = tmpTrain$Fare)

# Fit model

options(mc.cores = parallel::detectCores())

mod <- stan(data = stan_data, 
            model_code = m,
            iter = 3000,
            chains = 3,
            control = list(adapt_delta = 0.99),
            seed = 123)

#--------------------- Poisson example --------------------

# Simulate some data

set.seed(123)
n <- 20
x <- seq(from = -3, to = 3, length.out = n)
f <- 2 * cos(2 * x)
y <- rpois(n, exp(f))

# Plot it

data.frame(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(from = 0, to = 12, by = 2)) +
  theme(panel.grid.minor.y = element_blank())

# Specify model

m2 <- "
data {
  int<lower=1> N1;
  real x1[N1];
  int<lower=0> y1[N1];
  int<lower=1> N2;
  real x2[N2];
}

transformed data {
  real delta = 1e-9;
  int<lower=1> N = N1 + N2;
  real x[N];
  for (n1 in 1:N1) x[n1] = x1[n1];
  for (n2 in 1:N2) x[N1 + n2] = x2[n2];
}

parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real a;
  vector[N] eta;
}

transformed parameters {

  vector[N] f;
  {
    matrix[N, N] L_K;
    matrix[N, N] K = cov_exp_quad(x, alpha, rho);

    // Add small number to matrix diagonal for numerical stability
    
    for (n in 1:N)
      K[n, n] = K[n, n] + delta;

    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}

model {
  
  // Priors

  rho ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  a ~ std_normal();
  eta ~ std_normal();
  
  // Likelihood

  y1 ~ poisson_log(a + f[1:N1]);
}

generated quantities {
  int y2[N2];
  
  for (n2 in 1:N2)
    y2[n2] = poisson_log_rng(a + f[N1 + n2]);
}
"

# Set up data for Stan

stan_data2 <- list(N1 = n,
                   y1 = y,
                   x1 = x,
                   N2 = n,
                   x2 = x)

# Fit model

options(mc.cores = parallel::detectCores())

mod2 <- stan(data = stan_data2, 
            model_code = m2,
            iter = 3000,
            chains = 3,
            control = list(adapt_delta = 0.99),
            seed = 123)

# Extract quantiles for predicted values

xDat <- data.frame(x = x) %>%
  mutate(id = row_number())

preds <- as.data.frame(mod2) %>%
  dplyr::select(contains("y2")) %>%
  mutate(iteration = row_number()) %>%
  pivot_longer(cols = 1:20, names_to = "x_index", values_to = "values") %>%
  mutate(x_index = gsub("y2\\[", "\\1", x_index),
         x_index = gsub("\\]", "\\1", x_index)) %>%
  mutate(x_index = as.numeric(x_index)) %>%
  left_join(xDat, by = c("x_index" = "id")) %>%
  group_by(x) %>%
  summarise(median = median(values),
            lower = quantile(values, probs = 0.95),
            upper = quantile(values, probs = 0.05)) %>%
  ungroup()

# Plot our model predictions

data.frame(x, y) %>%
  ggplot() +
  geom_point(aes(x = x, y = y), size = 3) +
  geom_ribbon(data = preds, aes(x = x, ymin = lower, ymax = upper), fill = "steelblue2", alpha = 0.3) +
  geom_line(data = preds, aes(x = x, y = median), colour = "steelblue2", size = 1) +
  labs(title = "Predictions from latent Gaussian process model",
       subtitle = "Ribbon indicates 90% credible interval. Line indicates posterior median.",
       x = "X",
       y = "Y") +
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(from = 0, to = 12, by = 2)) +
  theme(panel.grid.minor.y = element_blank())
