#--------------------------------------
# This script sets out to build and
# plot some basic Gaussian process
# regression models
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 12 July 2021
#--------------------------------------

#%%
import random
import numpy as np
import matplotlib
import GPy
matplotlib.use("Agg")
GPy.plotting.change_plotting_library("matplotlib")

#%%
random.seed(123) # Fix seed for reproducibility
X = np.random.uniform(-5., 5., (10, 1)) # Generate vector of X values
Y = np.sin(X) # Generate vector of y values

#%%
kernel = GPy.kern.RBF(input_dim = 1, variance = 1., lengthscale = 1.)
m = GPy.models.GPRegression(X, Y, kernel)
fig = m.plot()
dataplot = fig['dataplot'][0] 
dataplot.figure.savefig('/Users/trenthenderson/Documents/Git/orbisant-posts/tutorials/gp/output/m1.png')

#%%
kernel2 = GPy.kern.RBF(input_dim = 1, variance = 5., lengthscale = 1.)
m2 = GPy.models.GPRegression(X, Y, kernel2)
fig2 = m2.plot()
dataplot2 = fig2['dataplot'][0] 
dataplot2.figure.savefig('/Users/trenthenderson/Documents/Git/orbisant-posts/tutorials/gp/output/m2.png')

#%%
kernel3 = GPy.kern.RBF(input_dim = 1, variance = 5., lengthscale = 2.)
m3 = GPy.models.GPRegression(X, Y, kernel3)
fig3 = m3.plot()
dataplot3 = fig3['dataplot'][0] 
dataplot3.figure.savefig('/Users/trenthenderson/Documents/Git/orbisant-posts/tutorials/gp/output/m3.png')

#%%
# Optimise parameters
m4 = GPy.models.GPRegression(X, Y, kernel3)
m4.optimize(messages = True)
fig4 = m4.plot()
dataplot4 = fig4['dataplot'][0] 
dataplot4.figure.savefig('/Users/trenthenderson/Documents/Git/orbisant-posts/tutorials/gp/output/m4.png')

#%%
# Some plot edits

import matplotlib; matplotlib.rcParams['figure.figsize'] = (8,5)
from matplotlib import pyplot as plt

# Draw matrix of plots

figure, axes = plt.subplots(3, 3, figsize = (10, 10), tight_layout = True)
kerns = [GPy.kern.RBF(1), GPy.kern.Exponential(1), GPy.kern.Matern32(1), GPy.kern.Matern52(1), GPy.kern.Brownian(1), GPy.kern.Bias(1), GPy.kern.Linear(1), GPy.kern.PeriodicExponential(1), GPy.kern.White(1)]

for k,a in zip(kerns, axes.flatten()):
    k.plot(ax = a, x = 1)
    a.set_title(k.name.replace('_', ' '))

plt.savefig('/Users/trenthenderson/Documents/Git/orbisant-posts/tutorials/gp/output/kernels.png')
