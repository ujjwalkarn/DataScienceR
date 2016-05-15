Assuming an additive decomposition, the decomposed time series can be written as 
yt=S^t+A^t
where A^t=T^t+E^t is the seasonally adjusted component.

Or if a multiplicative decomposition has been used, we can write
yt=S^tA^t, where A^t=T^tE^t.

To forecast a decomposed time series, we separately forecast the seasonal component, S^t, and the seasonally adjusted component A^t. It is usually assumed that the seasonal component is unchanging, or changing extremely slowly, and so it is forecast by simply taking the last year of the estimated component. In other words, a seasonal naÔve method is used for the seasonal component.

To forecast the seasonally adjusted component, any non-seasonal forecasting method may be used. For example, a random walk with drift model, or Holtís method (discussed in the next chapter), or a non-seasonal ARIMA model (discussed in Chapter 8), may be used
