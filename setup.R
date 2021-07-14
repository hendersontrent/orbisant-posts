#--------------------------------------
# This script sets out to load all
# the folders necessary for the project
#--------------------------------------

#---------------------------------------
# Author: Trent Henderson, 21 March 2021
#---------------------------------------

#-------------- Create important folders if none exists ------------

# Tutorials

if(!dir.exists('tutorials')) dir.create('tutorials')
if(!dir.exists('tutorials/output')) dir.create('tutorials/output')
if(!dir.exists('tutorials/heteroscedasticity')) dir.create('tutorials/heteroscedasticity')
if(!dir.exists('tutorials/gp')) dir.create('tutorials/gp')
if(!dir.exists('tutorials/gp/output')) dir.create('tutorials/gp/output')
if(!dir.exists('tutorials/julia')) dir.create('tutorials/julia')
if(!dir.exists('tutorials/julia/output')) dir.create('tutorials/julia/output')

# hctsa

if(!dir.exists('hctsa')) dir.create('hctsa')
if(!dir.exists('hctsa/data')) dir.create('hctsa/data')
if(!dir.exists('hctsa/output')) dir.create('hctsa/output')

# Probabilistic programming

if(!dir.exists('probabilistic-programming')) dir.create('probabilistic-programming')

# Forecasting

if(!dir.exists('forecasting')) dir.create('forecasting')

# Exploration

if(!dir.exists('exploration')) dir.create('exploration')
