# A look at the Relationship Between Individual Beta and Trading Volume

## Summary

There is a relationship between the average trading volume and Beta, and we would expect the Beta of a stock that trades at double the volume (in dollar terms) to have a Beta of around 0.05 higher. 

The relationship is statistically significant, which is useful for our purposes, although the model isn't reasonable to use for prediction of a security's Beta.

## Background

Beta's are commonly used to value securities and diversify portfolios, and are used as a measurement of the riskiness of an individual security.

The question was, "Are the Beta's of thinly traded securities lower than we would expect?"

The reason for the question was to figure out if it made sense to  build up a multiple to value a privately held company by using the Beta's of similar publically listed companies, if the publically listed companies had low trading volume.

It would be helpful if we could build a model to restate betas of public companies to fit a valuation situation, based on their trading volume.

### Limitations

We did not adjust for companies that:
  - Are in administration or liquidation
  - Recently listed and so have a short trading history
  - Have issued or split stock

Also
  - We should think about whether to take the absolute value of Betas
  - Seems to be a remaining trend in the NZX data

We were unable to scrape data for six securities, which have been disregarded:
```
    ticker  src   download.status total.obs perc.benchmark.da~ threshold.decisi~
1 BRK.B   yahoo NOT OK                  0                  0 OUT              
2 BF.B    yahoo NOT OK                  0                  0 OUT              
3 NTLOB.~ yahoo NOT OK                  0                  0 OUT              
4 CO2.NZ  yahoo NOT OK                  0                  0 OUT              
5 CBL.NZ  yahoo NOT OK                  0                  0 OUT              
6 MVT.NZ  yahoo NOT OK                  0                  0 OUT    
```
## Analysis
### Data sources
  - A listing of all tickers included in the current S&P500 index was obtained from Wikipedia.
  - A listing of all NZX tickers was obtained from https://www.nzx.com/markets/NZSX
  - Index data for the S&P500 and NZX50 were taken from Yahoo! Finance under the tickers "^GSPC" and "^NZ50", respectively
  - Price and volume history was obtained from Yahoo! Finance
  - Current market capitalisation data was obtained from NZX
  - The date range used was from 3 years ago until today.

### Data Tidying and Manipulation
We removed all tickers from the NZX list that corresponded to an ETF, of which there were 23.

For our analysis we used the adjusted daily closing prices, and took the increase/decrease % from the previous day as the return.

Beta's were derived by applying a simple linear model in R between the security's return and the index's return, and taking the regression coefficient: 

```R
model_run<-function(df){
  lm(ret.adjusted.prices~index.ret.adjusted.prices, data=df)
}
```

For Volume, we used a simple average daily volume for each security, multiplied by the closing price for the day to provide an approximation of daily dollar volume of trades. This was taken over the 3 year period (and if the security didn't trade over the whole period, then over the period it was trading).

For NZX tickers, current market capitalisation was also easily available.

### Exploratory Analysis

Average Dollar Volume traded is clearly right-skewed.

![Plot of Average Dollar Volume with different filters](https://github.com/hhar056/Beta-Analysis/blob/master/01averagedollarvolume.png?raw=true)

Taking a log transform improves the data for each of the subsets, although the aggregate data does not look normal. This could be expected from the way the data were sampled, but care must be taken to interpret the results of any analysis on this set. 

![Plot of the Log of the Average Dollar Volume with different fileters](https://github.com/hhar056/Beta-Analysis/blob/master/02lnaveragedollarvolume.png?raw=true)

Beta appeared reasonably normal with an outlier in the NZX set causing a mild skewness, ATM.NZ (the A2 Milk Company), with a Beta of 2.5.

![Histograms of Betas from each set](https://github.com/hhar056/Beta-Analysis/blob/master/03beta.png?raw=true)

Plotting beta against the Log of the Average Dollar Volume shows an apparent increasing trend in each data set.

![Plot of Beta ~ Log Average Dollar Volume](https://github.com/hhar056/Beta-Analysis/blob/master/04relationship.png?raw=true)

Using the log transformed Average Dollar Volume and used the model Beta ~ ln(Average Dollar Volume). We applied the model to the NZ data, the S&P 500 data, and a combination of both with similar results.

The NZX set below shows strong evidence against H0, that the Beta is not related to Ln Average Dollar Volume (p=2.3e-7, R2 = 0.2064)

```
Call:
lm(formula = beta ~ ln.average.dollar.volume, data = (share_data %>% 
    filter(Source == "NZ")))

Residuals:
     Min       1Q   Median       3Q      Max 
-0.68682 -0.22826 -0.07593  0.16334  1.67615 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              -0.39746    0.16408  -2.422    0.017 *  
ln.average.dollar.volume  0.07444    0.01350   5.512  2.3e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3713 on 112 degrees of freedom
Multiple R-squared:  0.2134,	Adjusted R-squared:  0.2064 
F-statistic: 30.38 on 1 and 112 DF,  p-value: 2.297e-07
```

The S&P set below shows strong evidence against H0, that the Beta is not related to Ln Average Dollar Volume (p=3.061e-5, R2 = 0.03252)

```
Call:
lm(formula = beta ~ ln.average.dollar.volume, data = share_data %>% 
    filter(Source == "SP"))

Residuals:
     Min       1Q   Median       3Q      Max 
-0.84108 -0.18892  0.02706  0.21967  0.97254 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              -0.47537    0.34425  -1.381    0.168    
ln.average.dollar.volume  0.07623    0.01812   4.208 3.06e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3279 on 496 degrees of freedom
Multiple R-squared:  0.03447,	Adjusted R-squared:  0.03252 
F-statistic: 17.71 on 1 and 496 DF,  p-value: 3.061e-05
```

The combined dataset below shows strong evidence against H0, that the Beta is not related to Ln Average Dollar Volume (p=2.2e-16, R2 = 0.2874)


```
A combination of

Call:
lm(formula = beta ~ ln.average.dollar.volume, data = share_data)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.84063 -0.20628  0.00893  0.21437  1.70521 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)    
(Intercept)              -0.34639    0.07924  -4.372 1.45e-05 ***
ln.average.dollar.volume  0.06952    0.00442  15.728  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.3359 on 610 degrees of freedom
Multiple R-squared:  0.2885,	Adjusted R-squared:  0.2874 
F-statistic: 247.4 on 1 and 610 DF,  p-value: < 2.2e-16
```

### Checking Assumptions

The data are independent by design

Histograms of the residuals seem to follow a normal distribution for each model.

![Histograms of residuals](https://github.com/hhar056/Beta-Analysis/blob/master/08residuals.png?raw=true)

Shapiro Wilk test shows strong evidence against normality

```
> shapiro.test(residuals(nzlm))

	Shapiro-Wilk normality test

data:  residuals(nzlm)
W = 0.90272, p-value = 4.786e-07

> shapiro.test(residuals(splm))

	Shapiro-Wilk normality test

data:  residuals(splm)
W = 0.9884, p-value = 0.0005485

> shapiro.test(residuals(mainlm))

	Shapiro-Wilk normality test

data:  residuals(mainlm)
W = 0.98838, p-value = 8.846e-05

```

However, this may be due to sample size. Plots of the residuals for each model seem to show reasonably even scatter around 0 withone or two outliers. The Normal QQ plot shows points lying reasonably close to the straight line, although with some potential residual skewness, primarily caused by the same points. The cook's distance shows that these points are not overly influential to the models:

![Model Integrity plots - NZX](https://github.com/hhar056/Beta-Analysis/blob/master/05nzlm.png?raw=true)

![Model Integrity plots - S&P500](https://github.com/hhar056/Beta-Analysis/blob/master/06splm.png?raw=true)

![Model Integrity plots - All](https://github.com/hhar056/Beta-Analysis/blob/master/07mainlm.png?raw=true)



### Inference

Beta appears to depend on the average trading volume (in dollar terms) of the security, and Beta increases as volume increases.

For NZX data, the relationship described around 20% of the variance in beta

For the S&P500 data, the relationship described around 3% of the variance in beta

For the combined data set, the relationship described around 29% of the variance in beta

The coefficient of each model is about 0.07, meanin gif we increase the average trading volume of a security by 1%, we would expect an increase in Beta of 0.07%.



