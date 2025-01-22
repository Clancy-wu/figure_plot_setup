import os
import pandas as pd
import numpy as np
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller, grangercausalitytests

# 1. load data
np.random.seed(123)
x = np.cumsum(np.random.normal(size=100))
y = np.cumsum(np.random.normal(size=100)) + 0.5 * x  # y influenced by x
data = pd.DataFrame({'x': x, 'y': y})

# 2. determine lag length
model = VAR(data)
lag_selection = model.select_order(maxlags=10)
print(lag_selection.summary())

# 3. fit with VAR model
optimal_lag = lag_selection.selected_orders['aic']
print(f"Optimal Lag Length (AIC): {optimal_lag}")

# 4. Perform Granger Causality Test
# maxlag = int: All lags up to max lag
# maxlag = list: Only lag the max lag
# ssr_ftest: Sum of Squared Residuals F-Test
# params_ftest: Parameter F-Test
grangercausalitytests(data[['y', 'x']], maxlag=[3])
grangercausalitytests(data[['x', 'y']], maxlag=[optimal_lag])

