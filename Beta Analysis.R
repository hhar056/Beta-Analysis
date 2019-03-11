install.packages("BatchGetSymbols")
install.packages("tidyverse")
install.packages("modelr")
library(BatchGetSymbols)
library(dplyr)
library(tidyverse)
library(modelr)
library(stringr)

setwd("G:/GS Value Added Team/01 GS Analytics/Beta Analysis")

# Get the date range
last_date <- Sys.Date()
first_date<-as.POSIXlt(last_date)
first_date$year<-first_date$year-3
first_date<-as.Date(first_date)

# Get Current S&P500 tickers
SP500_df <- GetSP500Stocks()


# Get NZX tickers
webpage<-read_html("https://www.nzx.com/markets/NZSX")
nz_df<-webpage %>% html_nodes("table") %>% html_table() 
nz_df<-as_tibble(nz_df[[1]])

#Remove ETFs
etfs<-read_html("https://en.wikipedia.org/wiki/List_of_New_Zealand_exchange-traded_funds")
etfs<-etfs %>% html_nodes("table") %>% html_table() 
etfs<-as_tibble(etfs[[1]])
nz_df<-nz_df %>% filter(!(Code %in% etfs$Code))

# Add .NZ to ticker name so we can use it with Yahoo finance
nz_df<-mutate(nz_df,Code = paste0(Code,".NZ"))
nz_df$Code

tickers <- c(SP500_df$company,nz_df$Code,"^GSPC","^NZ50")

# Read Yahoo data
output <- BatchGetSymbols(tickers = tickers,
                         first.date = first_date,
                         last.date = last_date)



# Convert to Tibbles
output_control<-as_tibble(output$df.control)
share_data<-as_tibble(output$df.tickers)

# Check that it's all good summarize(group_by(output_control,download.status),n())
output_control %>%
  group_by(download.status) %>%
    summarize(n())
output_control %>% filter(download.status=="NOT OK")

# Create a tibble of lists
share_data<-share_data %>%
  group_by(ticker) %>%
  nest()  
  
# Split the S&P data out
sp_index_data<-filter(share_data,ticker=="^GSPC")
sp_index_data<-sp_index_data$data[[1]]

sp_index_data<-sp_index_data %>%
  select(ref.date,ret.adjusted.prices,ret.closing.prices) %>%
  rename(index.ret.adjusted.prices=ret.adjusted.prices,index.ret.closing.prices=ret.closing.prices)

share_data<- share_data %>% filter(ticker!="^GSPC")

# Split the NZ data out
nz_index_data<-filter(share_data,ticker=="^NZ50")
nz_index_data<-nz_index_data$data[[1]]

nz_index_data<-nz_index_data %>%
  select(ref.date,ret.adjusted.prices,ret.closing.prices) %>%
  rename(index.ret.adjusted.prices=ret.adjusted.prices,index.ret.closing.prices=ret.closing.prices)

share_data<- share_data %>% filter(ticker!="^NZ50")

# Merge index return into each data set, label and merge data
merge_return<-function(stocksdf,indexdf){
    left_join(stocksdf,indexdf, by = "ref.date")
}
nz_share_data<-share_data %>% filter(str_detect(ticker,".NZ"))
nz_share_data$data<-map(nz_share_data$data,merge_return,indexdf=nz_index_data)
nz_share_data<-nz_share_data %>% mutate(Source="NZ")

sp_share_data<-share_data %>% filter(!str_detect(ticker,".NZ"))
sp_share_data$data<-map(sp_share_data$data,merge_return,indexdf=sp_index_data)
sp_share_data<-sp_share_data %>% mutate(Source="SP")

share_data<-rbind(nz_share_data,sp_share_data)

# Run model on each data set
model_run<-function(df){
  lm(ret.adjusted.prices~index.ret.adjusted.prices, data=df)
}
share_data<-share_data %>%
  mutate(model=map(data,model_run))

# Get Beta
model_beta<-function(model){
  coefficients(model)[[2]]
}
share_data<-share_data%>%
  mutate(beta=lapply(share_data$model,model_beta)) %>%
  unnest(beta)

#Get average volume over the same period
share_volume<-function(historical_data){
  mean(historical_data$volume, na.rm=T)
}
share_dollar_volume<-function(historical_data){
  mean(historical_data$volume*historical_data$price.adjusted,na.rm=T)
}

share_data<-share_data%>%
  mutate(average.volume=lapply(data,share_volume)) %>%
  mutate(average.dollar.volume=lapply(data,share_dollar_volume))%>%
  unnest(average.volume)%>%
  unnest(average.dollar.volume)


#Log transform of average volume
share_data<-share_data%>%
  mutate(ln.average.volume=log(average.volume))%>%
  mutate(ln.average.dollar.volume=log(average.dollar.volume))


# Exploratory Analysis


#Look at Average Dollar Volume in NZ and S&P500
png(file="01averagedollarvolume.png",width=600,height=200)
par(mfrow=c(1,3),mar=c(4,4,2,1))
with(share_data%>%filter(Source=="NZ"),{ 
  hist(average.dollar.volume,breaks=10, col="red",xlab="NZX Average Dollar Volume",main="NZX Average Dollar Volume")
} )
with(share_data%>%filter(Source=="SP"),{ 
  hist(average.dollar.volume,breaks=10, col="red",xlab="S&P500 Average Dollar Volume",main="S&P500 Average Dollar Volume")
})
with(share_data,{ 
  hist(average.dollar.volume,breaks=10, col="red",xlab="All Average Dollar Volume",main="All Average Dollar Volume")
} )


par(mfrow=c(1,1))
dev.off()

#Look at Ln Average Dollar Volume in NZ and S&P500
png(file="02lnaveragedollarvolume.png",width=600,height=200)
par(mfrow=c(1,3))
with(share_data%>%filter(Source=="NZ"),{ 
  hist(ln.average.dollar.volume,breaks=10, col="red",xlab="NZX Log Average Dollar Volume",main="NZX Ln Average Dollar Volume")
} )
with(share_data%>%filter(Source=="SP"),{ 
  hist(ln.average.dollar.volume,breaks=10, col="red",xlab="S&P500 Log Average Dollar Volume",main="S&P500 Ln Average Dollar Volume")
})
with(share_data,{ 
  hist(ln.average.dollar.volume,breaks=10, col="red",xlab="All Log Average Dollar Volume",main="All Ln Average Dollar Volume")
})
par(mfrow=c(1,1))
dev.off()


#Look at Ln Average Dollar Volume in NZ and S&P500
png(file="03beta.png",width=600,height=200)
par(mfrow=c(1,3))
with(share_data%>%filter(Source=="NZ"),{ 
  hist(beta,breaks=10, col="red",xlab="NZX Betas",main="NZX Betas")
} )
with(share_data%>%filter(Source=="SP"),{ 
  hist(beta,breaks=10, col="red",xlab="S&P500 Betas",main="S&P500 Betas")
})
with(share_data,{ 
  hist(beta,breaks=10, col="red",xlab="All Betas",main="All Betas")
})
par(mfrow=c(1,1))
dev.off()


#Look at whether there's a discernable relationship between Beta and the Ln of Average Dollar Volume

png(file="04relationship.png",width=600,height=200)
par(mfcol=c(1,3))
with(share_data%>%filter(Source=="NZ"),{
  plot(ln.average.dollar.volume,beta,col="red",xlab="Log Average Dollar Volume",ylab="Beta",main="NZX")
})
with(share_data%>%filter(Source=="SP"),{
  plot(ln.average.dollar.volume,beta,col="blue",xlab="Log Average Dollar Volume",ylab="Beta",main="S&P500")
})
with(share_data,{
  plot(ln.average.dollar.volume,beta, type="n",xlab="Log Average Dollar Volume",ylab="Beta",main="All")
})
with(share_data%>%filter(Source=="NZ"),{
  points(ln.average.dollar.volume,beta,col="red")
})
with(share_data%>%filter(Source=="SP"),{
  points(ln.average.dollar.volume,beta,col="blue")
})
par(mfrow=c(1,1))
dev.off()

#Fit Linear Models, check stats and check assumptions

nzlm<-lm(beta~ln.average.dollar.volume,data=(share_data%>%filter(Source=="NZ")))
summary(nzlm)
shapiro.test(residuals(nzlm))

png(file="05nzlm.png")
par(mfcol=c(2,2),oma=c(0,0,2,0))
plot(nzlm,sub.caption="NZX Linear Model: Beta ~ Log Average Dollar Value")
dev.off()

splm<-lm(beta~ln.average.dollar.volume,data=share_data%>%filter(Source=="SP"))
summary(splm)
shapiro.test(residuals(splm))

png(file="06splm.png")
par(mfcol=c(2,2),oma=c(0,0,2,0))
plot(splm,sub.caption="S&P500 Linear Model: Beta ~ Log Average Dollar Value")
dev.off()

mainlm<-lm(beta~ln.average.dollar.volume,data=share_data)
summary(mainlm)
shapiro.test(residuals(mainlm))

png(file="07mainlm.png")
par(mfcol=c(2,2),oma=c(0,0,2,0))
plot(mainlm,sub.caption="Main Linear Model: Beta ~ Log Average Dollar Value")
dev.off()

#Fit regression to larger plot
png(file="08main.png")
par(mfcol=c(1,1))
with(share_data,{
  plot(ln.average.dollar.volume,beta, type="n",xlab="Ln Average Dollar Volume",ylab="Beta",main="Main Linear Model: Beta ~ Log Average Dollar Value")
})
with(share_data%>%filter(Source=="NZ"),{
  points(ln.average.dollar.volume,beta,col="red")
})
with(share_data%>%filter(Source=="SP"),{
  points(ln.average.dollar.volume,beta,col="blue")
})
abline(lm(beta~ln.average.dollar.volume,data=share_data),lwd=3,col="purple")
par(mfrow=c(1,1))
dev.off()

#Market Capitalisation Tests - not used
nz_df<-nz_df%>%rename(ticker=Code)
nz_df
nz_sub<-share_data%>%filter(Source=="NZ")

nz_sub<-left_join(nz_sub,select(nz_df,ticker,Capitalisation), by = "ticker")
nz_sub<-nz_sub%>%
  mutate(Capitalisation=gsub("[\\$,]","",nz_sub$Capitalisation))%>%
  mutate(Capitalisation=as.numeric(Capitalisation)) %>%
  mutate(ln.Capitalisation=log(Capitalisation))


nz_sub
par(mfcol=c(1,3))
with(nz_sub,{
  hist(ln.Capitalisation,breaks=20)
  plot(ln.Capitalisation,beta)
  plot(ln.Capitalisation,ln.average.dollar.volume)
})

nzlm2<-lm(beta~ln.average.dollar.volume+ln.Capitalisation,data=nz_sub)
summary(nzlm2)
plot(nzlm2)
