require(data.table)

require(lubridate)
require(forecast)
require(ggplot2)
require(GGally)


############# Reading and processing datas ##########
data_path="C:/Users/serca/OneDrive/Masaüstü/Spring 22'/IE 360/HW_2/IE360_Spring22_HW2_data.csv"
data=read.csv(data_path)

data$Unleaded.Gasoline.Sale..UGS. = gsub( " ", "", data$Unleaded.Gasoline.Sale..UGS. )
data$Unleaded.Gasoline.Sale..UGS. = as.numeric(data$Unleaded.Gasoline.Sale..UGS.)

data$X..LPG.Vehicles..NLPG. = gsub( " ", "", data$X..LPG.Vehicles..NLPG. )
data$X..LPG.Vehicles..NLPG. = as.numeric(data$X..LPG.Vehicles..NLPG)

data$X..Unleaded.Gasoline.Vehicles..NUGV. = gsub( " ", "", data$X..Unleaded.Gasoline.Vehicles..NUGV. )
data$X..Unleaded.Gasoline.Vehicles..NUGV. = as.numeric(data$X..Unleaded.Gasoline.Vehicles..NUGV.)

data$GNP.Agriculture = gsub( " ", "", data$GNP.Agriculture )
data$GNP.Agriculture = as.numeric(data$GNP.Agriculture)

data$GNP.Commerce = gsub( " ", "", data$GNP.Commerce )
data$GNP.Commerce = as.numeric(data$GNP.Commerce)

data$GNP.Total = gsub( " ", "", data$GNP.Total )
data$GNP.Total = as.numeric(data$GNP.Total)


data$Quarter = as.character(data$Quarter)
data$Quarter <- as.Date(data$Quarter)  

data = data.table(data)

head(data,25)
str(data)
##############################################

########## Converting UGS to time series and plotting #################
data$Unleaded.Gasoline.Sale..UGS. = ts(data$Unleaded.Gasoline.Sale..UGS., start = 2000, deltat= 1/4)

ggplot(data, aes(Quarter,Unleaded.Gasoline.Sale..UGS. , group=1)) + geom_line() +geom_point()

# Comments on the shape !!!!
#Looking at the shape of the plot it can be observed that the number of sales
# have a decreasing trend with seasonal peaks and lows. Visiting back to the definition of stationary we know that
# stationary time series data is not predictable since it has no any detectable trend and seasonality. Our UGS data
# is clearly shrinking with a slight decrase in it's variance. We conclude that the data is not stationary thus prediction models 
# can be prepared at tested.
############################################################

#################### Looking at the Autocorrelation of UGS ###########

acf(data$Unleaded.Gasoline.Sale..UGS.[0:28], 12) # The last 4 values are missing. We use the 
# available data to measure autocorrelation func.
# Below plotting shows the autocorrelation of the UGS sales. One can easily detect that
# ACF is having peaks with lags that are multiplier of 4. This is a indication of seasonality with 
# respect to quarters. 



############################# Adding needed datas as column (Seasonality, Trend etc.)  ##########################################


data[,trend:=1:.N] # Adding the trend factor as a column to the data

data[,seasonality:=((1:.N)%%4)] # remainder from 4. 4th quarters are represented with "0".
data[seasonality == 0, seasonality := 4]
data$seasonality = as.factor(data$seasonality) # Taking seasonality as factor

str(data)

###################### Starting to create the model #################

ggpairs(data[,-1,with=FALSE]) # We will build our model with the variables which are in high correlation with our output variable 



model_0= lm(Unleaded.Gasoline.Sale..UGS.~trend ,data)
summary(model_0)
checkresiduals(model_0$residuals) # There's high autocorrelation with lag 2 and 4's
checkresiduals(model_0)

model_1= lm(Unleaded.Gasoline.Sale..UGS.~trend + seasonality,data) # One of the seasonality is omitted into the intercept
summary(model_1)
checkresiduals(model_1$residuals)
checkresiduals(model_1)

#Let's start to add the variables to the model respectively with their correlation with our output variables
#to increase the performance (Adj R^2) of the model and drop the insignificant variables.

model_2= lm(Unleaded.Gasoline.Sale..UGS.~trend + seasonality 
            + Price.of.Diesel.Gasoline..PG.+
              X..Unleaded.Gasoline.Vehicles..NUGV. + X..of.Diesel.Gasoline.Vehicles..NDGV.
             + GNP.Commerce , data)
#+X..Unleaded.Gasoline.Vehicles..NUGV. + X..LPG.Vehicles..NLPGGNP.Total,data)
summary(model_2)
checkresiduals(model_2$residuals)
checkresiduals(model_2) # Our adjusted R^2 increased but there's still a high autocorrelation
# in our residuals with a lag of one.

#Even though with the addition of seasonality
#the autocorrelation with lag 1 is still consistent. Let's add y(t-1) and y(t-2) and y(t-4) to the data
setDT(data)
data[,.(two_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=2L, fill=NA))]
data$two_lagged_sales = data[,.(two_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=2L, fill=NA))]

data[,.(one_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=1L, fill=NA))]
data$one_lagged_sales = data[,.(two_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=1L, fill=NA))]

data[,.(four_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=4L, fill=NA))]
data$four_lagged_sales = data[,.(four_lagged_sales = shift(data$Unleaded.Gasoline.Sale..UGS., n=4L, fill=NA))]


# we added the 4, 2 and 1 time interval lagged y(t)'s to their corresponding rows. The previous model
# had a high autocorrelation with a lag of 1. Let's add one_lagged_sales to the model

model_3= lm(Unleaded.Gasoline.Sale..UGS.~trend + seasonality 
            + Price.of.Diesel.Gasoline..PG.+
              X..Unleaded.Gasoline.Vehicles..NUGV. + X..of.Diesel.Gasoline.Vehicles..NDGV.
            + GNP.Commerce + one_lagged_sales  , data)
summary(model_3)
checkresiduals(model_3$residuals)
checkresiduals(model_3)


# The high autocorrelation with lag 1 is ommitted with the addition of 1 lagged output data 
# ACF values are currently below the upper and lower limits. However significance of other variables
# reduced. Let's reduce insignificant variables from the model except the seasonality. Even though
# seasonality variable is insignificant, when it's reduced there occurs spikes in autocorrelation of
# residuals with period of 4 despite the little increase in the Adj R^2. For that reason we will keep it 
#in the model for the sake of normality of our residuals. 


model_4= lm(Unleaded.Gasoline.Sale..UGS.~trend + seasonality 
            + Price.of.Diesel.Gasoline..PG.+
              X..Unleaded.Gasoline.Vehicles..NUGV. + X..of.Diesel.Gasoline.Vehicles..NDGV.
          + one_lagged_sales  , data)
summary(model_4)
checkresiduals(model_4$residuals)
checkresiduals(model_4)
###### We have dropped the  X..Unleaded.Gasoline.Vehicles..NUGV." variable since it was found to be
# insignificant from the t test. Looking at the residuals and it's autocorrelation, we still see
# some negative autocorrelation with lags of 4 and 8. We might consider to add four_lagged_sales
# to the model. 

model_5= lm(Unleaded.Gasoline.Sale..UGS.~trend + seasonality 
            + Price.of.Diesel.Gasoline..PG.+
              X..Unleaded.Gasoline.Vehicles..NUGV. + X..of.Diesel.Gasoline.Vehicles..NDGV.
            + one_lagged_sales + four_lagged_sales , data)
summary(model_5)
checkresiduals(model_5$residuals)
checkresiduals(model_5)

# We have added the four_lagged_sales to the model in order to reduce the autocorrelation
# with the lag of 4 but contrary to our expectation this variable
# is found to be insignificant from the t test and our model's adjusted R^2 is reduced heavily. 
# In conclusion we will select the model_4 as our main model. It's residual's autocorrelation values
# are within the decided upper and lower limits. Adj R^2 is 0.9651.
summary(model_4)
checkresiduals(model_4$residuals)
checkresiduals(model_4)


tmp=copy(data)

tmp[,actual:=Unleaded.Gasoline.Sale..UGS.]
tmp[,predictions:=predict(model_4,tmp)]
print(tmp)


tmp[ 30, "one_lagged_sales"] = 66210.0
tmp[,predictions2:=predict(model_4,tmp)]
print(tmp)

tmp[ 31, "one_lagged_sales"] = 1110417.4
tmp[,predictions2:=predict(model_4,tmp)]
print(tmp)

tmp[ 32, "one_lagged_sales"] = 848645.6
tmp[,predictions2:=predict(model_4,tmp)]
print(tmp)


ggplot(tmp ,aes(x=Quarter)) +
  geom_line(aes(y=actual,color='real', group = 1)) + 
  geom_line(aes(y=predictions2, color = 'predictions', group = 1) ) 














