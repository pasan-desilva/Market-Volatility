#Import Packages
import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import datetime as dt
import seaborn as sns
import pandas_datareader as web
import investpy
plt.rcParams['font.family'] = 'serif'

#Import data BRK-A
start = '01/01/2016'
end = '31/12/2020'

df = investpy.get_stock_historical_data(stock='BRKa', country='United States', from_date = start, to_date = end)

#Daily Returns
returns = 100 * df['Close'].pct_change().dropna()
returns


#GARCH (1,1)
model_garch = arch_model(returns, p=2, o=0, q=2, power=1.0)
residual_garch = model_garch.fit()

#GJR-GARCH
gjr_garch_model = arch_model(returns, p=2, q=2)
residual_gjr = gjr_garch_model.fit(update_freq=5, disp="off")

#E-GARCH
egarch_model = arch_model(returns, p = 2, q = 2, vol = 'EGARCH', dist = 't')
residual_egarch = egarch_model.fit(disp = 'off')

#GARCH Volatilities
gjrgarch_volatility = residual_gjr.conditional_volatility
garch_volatility = residual_garch.conditional_volatility
egarch_volatility = residual_egarch.conditional_volatility

#Comparison - GARCH V GJR-GARCH
plt.figure(figsize=(14,8))
plt.plot(returns, color = 'grey', alpha = 0.4, label = 'Price Returns')
plt.plot(gjrgarch_volatility, color = 'green', label = 'GJR-GARCH Volatility')
plt.plot(garch_volatility, color = 'red', label = 'GARCH Volatility')
plt.legend(loc = 'upper right')
plt.show()

#Comparison - GARCH V E-GARCH
plt.figure(figsize=(14,8))
plt.plot(returns, color = 'grey', alpha = 0.4, label = 'Price Returns')
plt.plot(garch_volatility, color = 'red', label = 'GARCH Volatility')
plt.plot(egarch_volatility, color = 'blue', label = 'EGARCH Volatility')
plt.legend(loc = 'upper right')
plt.show()

#Comparison - GJR-GARCH V E-GARCH
plt.figure(figsize=(14,8))
plt.plot(returns, color = 'grey', alpha = 0.4, label = 'Price Returns')
plt.plot(gjrgarch_volatility, color = 'green', label = 'GJR-GARCH Volatility')
plt.plot(egarch_volatility, color = 'blue', label = 'EGARCH Volatility')
plt.legend(loc = 'upper right')
plt.show()

#Full Comparison
plt.figure(figsize=(14,8))
plt.plot(returns, color = 'grey', alpha = 0.4, label = 'Price Returns')
plt.plot(gjrgarch_volatility, color = 'green', label = 'GJR-GARCH Volatility')
plt.plot(egarch_volatility, color = 'blue', label = 'EGARCH Volatility')
plt.plot(garch_volatility, color = 'red', label = 'GARCH Volatility')
plt.legend(loc = 'upper right')
plt.show()