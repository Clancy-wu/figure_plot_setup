library(lmtest)
library(vars)

# 1. prepare file ---------------------------------------------------------
#library(freesurferformats)
set.seed(123)
x <- cumsum(rnorm(100))
y <- cumsum(rnorm(100)) + 0.5 * x[1:100]

# 2. determine lag length -------------------------------------------------
data <- cbind(x, y)
lag_selection <- VARselect(data, lag.max = 10, type = "const")
print(lag_selection$selection)  # Suggested lag order

# 3. fit with VAR model ---------------------------------------------------
var_model <- VAR(data, p = lag_selection$selection["AIC(n)"], type = "const")

# 4. Perform the Granger Causality Test -----------------------------------
# default is the finite sample F statistic (with approximate F distribution).
library(lmtest)
grangertest(y ~ x, order = lag_selection$selection["AIC(n)"])
grangertest(x ~ y, order = lag_selection$selection["AIC(n)"])

