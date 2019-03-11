# Dillinger

[![N|Solid](https://cldup.com/dTxpPi9lDf.thumb.png)](https://nodesource.com/products/nsolid)

[![Build Status](https://travis-ci.org/joemccann/dillinger.svg?branch=master)](https://travis-ci.org/joemccann/dillinger)

# A look at the Relationship Between Individual Beta and Trading Volume

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

Average Dollar Volume was clearly right-skewed.

Beta appeared reasonably normal with an outlier, ATM.NZ (the A2 Milk Company), with a Beta of 2.5

We log transformed Average Dollar Volume and used the model Beta ~ ln(Average Dollar Volume). We applied the model to the NZ data, the S&P 500 data, and a combination of both with similar results.

### Checking Assumptions

The data are independent

Plots of the residuals seem to show even scatter around 0 with some potentially influential outliers.

Histograms of the residuals seem to follow a normal distribution

Shapiro Wilk test shows no evidence against normality

### Inference

Beta appears to depend on the average trading volume (in dollar terms) of the security, and Beta increases as volume increases.

For NZX data, the relationship described a large amount of the variance in Beta, i.e. volume was a reaonably good predictor of Beta.

For the S&P500 data, the relationship did not describe a large amount of the variance in Beta, i.e. the model is not a good predictor of Beta

For the combined data set, the relationship described a large amount of the variance in Beta, i.e. volume was a reaonably good predictor of Beta.

The relationship is multiplicative



