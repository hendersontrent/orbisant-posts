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
library(Rcatch22)
library(theft)
library(caTools)
library(rstan)
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
                colour = "#67a9cf", alpha = 0.6, size = 0.2) +
      geom_line(data = my_iso, aes(x = year, y = log(co2_per_capita)),
                colour = "#ef8a62", size = 0.8) +
      labs(title = "Time-series of log-scaled Co2 emissions per capita by country",
           x = "Year",
           y = "log(Co2 Emissions per Capita)")
  } else{
    
    p <- others %>%
      ggplot() +
      geom_line(aes(x = year, y = co2_per_capita, group = iso_code),
                colour = "#67a9cf", size = 0.1) +
      geom_line(data = my_iso, aes(x = year, y = co2_per_capita),
                colour = "#ef8a62", size = 0.8) +
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
plot_low_dimension(outs, is_normalised = FALSE, id_var = "country", group_var = "continent_name", method = "RobustSigmoid", low_dim_method = "PCA",
                   plot = TRUE)
dev.off()

#-------------------- Correlation analysis ---------------------

# Normalise values

normed <- outs %>%
  dplyr::select(-c(iso_code, continent_name)) %>%
  dplyr::select(c(country, names, values)) %>%
  dplyr::group_by(names) %>%
  dplyr::mutate(values = normalise_catch(values, method = "RobustSigmoid")) %>%
  dplyr::ungroup() %>%
  tidyr::drop_na()

# Calculate correlations

countrs_to_keep <- normed %>%
  group_by(country) %>%
  summarise(counter = n()) %>%
  filter(counter == 22)

countrs_to_keep <- countrs_to_keep$country

cor_dat <- normed %>%
  filter(country %in% countrs_to_keep) %>%
  tidyr::pivot_wider(id_cols = "names", names_from = "country", values_from = "values") %>%
  dplyr::select(-c(names))

result <- cor(cor_dat)

# Produce data visualisation

melted <- reshape2::melt(result)

CairoPNG("hctsa/output/cormat.png",1500,1500)
pcor <- melted %>%
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  labs(title = "Feature value correlations between countries",
       x = NULL,
       y = NULL,
       fill = "Correlation Coefficient") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(-1,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
print(pcor)
dev.off()

# Hierarchically-cluster and reproduce graphic

row.order <- hclust(dist(result))$order # Hierarchical cluster on rows
col.order <- hclust(dist(t(result)))$order # Hierarchical cluster on columns
dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe

CairoPNG("hctsa/output/cormat-clustered.png",1500,1500)
pcorc <- cluster_out %>%
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  labs(title = "Feature value correlations between countries with hierarchical clustering",
       x = NULL,
       y = NULL,
       fill = "Correlation Coefficient") +
  scale_fill_distiller(palette = "RdYlBu",
                       limits = c(-1,1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1))
print(pcorc)
dev.off()

#-------------------- Statistical modelling --------------------

# Create groups

group_labs <- outs %>%
  group_by(country, continent_name) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  dplyr::select(c(country, continent_name))

outs1 <- normed %>%
  filter(country %in% countrs_to_keep) %>%
  inner_join(group_labs, by = c("country" = "country")) %>%
  mutate(cont_group = ifelse(continent_name == "Asia", 1,0)) %>%
  dplyr::select(-c(continent_name)) %>%
  pivot_wider(id_cols = c(country, cont_group), names_from = names, values_from = values)

options(mc.cores = parallel::detectCores()) # Parallel processing

# Set up data for Stan

name_list <- as.vector(colnames(outs1))
name_list <- name_list[!name_list %in% c("country", "cont_group")]

X <- outs1 %>%
  dplyr::select(-c(country, cont_group))

stan_data <- list(N = nrow(X),
                  P = ncol(X),
                  K = as.integer(length(name_list)),
                  y = outs1$cont_group,
                  X = as.matrix(X))

# Fit a classification model

m1 <- rstan::stan(file = "/Users/trenthenderson/Documents/Git/sawlog/inst/stan/BayesGLM.stan", 
                  data = stan_data, iter = 3000, chains = 3, seed = 123, control = list(max_treedepth = 10))

#------------------
# Model Diagnostics
#------------------

bayesplot::color_scheme_set("red")

# Posterior predictive checks

bayesplot::ppc_bars(m1) +
  ggplot2::labs(title = "Posterior predictive check",
                x = "Group",
                y = "Count")

bayesplot::ppc_ecdf_overlay(m1) +
  ggplot2::labs(title = "Posterior predictive check of cumulative probability function",
                x = "Group",
                y = "Cumulative Probability")

# Chain convergence

bayesplot::color_scheme_set("mix-blue-red")
bayesplot::mcmc_trace(m1, regex_pars = c("beta"))

# LOO-CV

loo1 <- loo::loo(m1, save_psis = TRUE)
plot(loo1)

# Coefficients

bayesplot::color_scheme_set("red")

bayesplot::mcmc_hist(m1, regex_pars = c("beta")) +
  ggplot2::labs(title = "Coefficient posterior distributions") +
  ggplot2::geom_vline(xintercept = 0, lty = "dashed", colour = "black", size = 1)

# Overall model summary with 90% credible intervals

m1_dat <- as.data.frame(m1) %>%
  dplyr::select(dplyr::contains("beta"))

name_list_stan <- as.vector(colnames(m1_dat))

m1_dat_re <- m1_dat %>%
  dplyr::rename_at(vars(dplyr::all_of(name_list_stan)), ~ name_list) %>%
  tidyr::pivot_longer(tidyr::everything(), names_to = "names", values_to = "values") %>%
  dplyr::group_by(names) %>%
  dplyr::summarise(lower = quantile(values, prob = 0.1),
                   median = median(values),
                   upper = quantile(values, prob = 0.9)) %>%
  dplyr::ungroup()
