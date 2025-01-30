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

# 5. Measure effect size
# 5.1 Use Log-Likelihood Ratio (LLR)
# The F-statistic in Granger causality tests compares the goodness-of-fit between models with and without lagged variables. 
# The effect size can be estimated using the log-likelihood ratio
# Measure: A higher LLR indicates a stronger effect.

for lag in range(1, max_lag + 1): # Extract LLR from the F-statistic test at the best lag
    f_test = gc_test[lag][0]['ssr_ftest']
    effect_size = np.log(f_test[0]) if f_test[0] > 0 else 0
    print(f"Lag {lag}: Effect size (LLR) = {effect_size:.4f}")
  
# 5.2 Use Partial R² (Variance Explained, recommendation)
# Another approach is to compute the proportion of variance in Y explained by past values of X.
# Measure: Higher R² → Stronger Granger causality effect.

def granger_r2(df, max_lag=3):
    results = {}
    for lag in range(1, max_lag + 1):
        # Restricted Model (without past X)
        model_restricted = sm.OLS(df['Y'][lag:], sm.add_constant(df['Y'].shift(lag).dropna())).fit()
        RSS_restricted = np.sum(model_restricted.resid ** 2)
        
        # Unrestricted Model (with past X)
        predictors = df[['Y', 'X']].shift(lag).dropna()
        model_unrestricted = sm.OLS(df['Y'][lag:], sm.add_constant(predictors)).fit()
        RSS_unrestricted = np.sum(model_unrestricted.resid ** 2)
        
        # Compute R² effect size
        r2 = 1 - (RSS_unrestricted / RSS_restricted)
        results[f'Lag {lag}'] = r2

    return results

effect_sizes = granger_r2(df)
print(effect_sizes)

# 5.3 Use Transfer Entropy (Nonlinear Effect Size)
# Granger causality is linear, but Transfer Entropy (TE) measures nonlinear dependencies. Use pyinform
# Higher TE → Stronger causal effect, even capturing nonlinear relationships.

from pyinform.transferentropy import transferentropy
# Convert data to discrete bins
def discretize(series, bins=5):
    return np.digitize(series, np.histogram(series, bins=bins)[1])
# Compute Transfer Entropy
X_discrete = discretize(df['X'])
Y_discrete = discretize(df['Y'])
TE = transferentropy(X_discrete, Y_discrete, k=3)  # k is the history length
print(f"Transfer Entropy (TE) from X → Y: {TE:.4f}")

####################################################
##  Final Recommendation
##  If your model is linear → Use Partial R² or LLR.
##  If your model may be nonlinear → Use Transfer Entropy.
If you want an intuitive measure → Use Partial R².
####################################################
