#---------------------------------------
# This script sets out to define a 
# bunch of GP related computations
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 22 April 2021
#---------------------------------------

#----------------- GP specification ---------------

def init_gp():
  
  from sklearn.gaussian_process.kernels import WhiteKernel, ExpSineSquared, ConstantKernel
  from sklearn.gaussian_process import GaussianProcessRegressor
  
  # Add kernels together for white noise and cyclic fluctuations

  k0 = WhiteKernel(noise_level=0.3**2, noise_level_bounds=(0.1**2, 0.5**2))
  k1 = ConstantKernel(constant_value=2) * \
    ExpSineSquared(length_scale=1.0, periodicity=40, periodicity_bounds=(35, 45))
  kernel_1  = k0 + k1 
  
  # Instantiate regressor
  
  m = GaussianProcessRegressor(
    kernel = kernel_1, 
    n_restarts_optimizer = 10, 
    normalize_y = True,
    alpha = 0.0
    )
  
  return m
  
#----------------- GP training --------------------

def fit_gp(spec, X, y):
  
  import numpy as np
  
  X = np.array(X).reshape(-1,1)
  
  m = spec.fit(X, y)
  return m

#----------------- Prior sampling check -----------

def sample_gp_prior(model, X, n, n_samples):
  
  import pandas as pd
  import numpy as np
  
  data_df = pd.DataFrame({'X' : X})
  
  X1 = data_df['X'].values.reshape(n, 1)
  
  prior_samples = model.sample_y(X = X1, n_samples = n_samples)
  return prior_samples
  
#----------------- GP predictions -----------------

def predict_gp(model, X):
  
  y_pred, y_std = model.predict(X, return_std = True)
  return y_pred, y_std
