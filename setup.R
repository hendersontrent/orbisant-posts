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

# hctsa

if(!dir.exists('hctsa')) dir.create('hctsa')
if(!dir.exists('hctsa/data')) dir.create('hctsa/data')
if(!dir.exists('hctsa/output')) dir.create('hctsa/output')

# Probabilistic programming

if(!dir.exists('probabilistic-programming')) dir.create('probabilistic-programming')

# Forecasting

if(!dir.exists('forecasting')) dir.create('forecasting')
