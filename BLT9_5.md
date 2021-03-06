# Analyzing Stock Volatility
Johnny Quick  
July 15, 2016  

## Analyzing volatility of JNJ stock 
## Johnson and Johnson


```r
#uncomment install package if not installed
#install.packages("tseries")
library(tseries)

# get data for Johnson and Johnson starting with year 2000
JNJData <- get.hist.quote("JNJ", quote="Close", start = "2000-01-01")
```

```
## time series starts 2000-01-03
## time series ends   2016-07-15
```

```r
JNJRet <- log(lag(JNJData)) - log(JNJData)

# calculate volatility (standard deviation of JNJRet * sqrt(250) * 100)
## 250 is used because that is the approximate number of days the stock market is open
## multiplied by 100 to get percentage
JNJVol <- sd(JNJRet) * sqrt(250) * 100

# get volatility
JNJVol
```

```
## [1] 25.87466
```

```r
# create function to calculate volatility with weighting
Vol <- function(d, logrets){
        var = 0
        lam = 0
        varlist <- c()
        for (r in logrets) {
              lam = lam*(1 - 1/d) + 1
        var = (1 - 1/lam)*var + (1/lam)*r^2
                  varlist <- c(varlist, var)
        }
        sqrt(varlist)}

# weight of .9
volest <- Vol(10, JNJRet)
# weight of .5
volest2 <- Vol(50, JNJRet)
# weight of .2
volest3 <- Vol(80, JNJRet)
```

Looking at the plot below, the volatility starts out fairly low, then it spikes about 400 periods in, goes back down, goes slightly up again around 600 periods in, and then goes back down and remains steady with low volatiility until about 2200 periods in. Then, it goes back down and stays down. As the weight goes down, the volatility goes down as well.


```r
plot(volest, type = "l")
lines(volest2, type="l", col="red")
lines(volest3, type="l", col="blue")
```

![](BLT9_5_files/figure-html/plot-1.png)<!-- -->
