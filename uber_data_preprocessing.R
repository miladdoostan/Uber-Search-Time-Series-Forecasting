# requiring necessary packages, and functions
source('require_pckg.R')
source('four_plot.R')
source('residual_diognastics.R')
source('trend_seasonality_strength.R')

# importing uber data
df <- as.data.frame(fread('Uber.csv'))

# creating the uber time series
start_year <- year('2013-11-03')
start_week <- week('2013-11-03')
uber <- ts(df$Count, start = c(start_year, start_week), frequency = 52)

# displaying the data
autoplot(uber)

# exploring the outliers
tsoutliers(uber)

# handling outlirs and cleansing the data
uber <- tsclean(uber)
uber <- (uber/max(uber)) * 100 # the deta should be converted again to scale of 0 to 100
autoplot(uber)


# creating training and testing sets
uber_train <- window(uber, end=c(2017,52))
uber_test <- window(uber, start=c(2018,1))
autoplot(uber_train)
