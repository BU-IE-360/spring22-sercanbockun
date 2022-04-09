require(data.table)
require(lubridate)
require(forecast)
require(ggplot2)
require(skimr)
require(GGally)
require(readxl)
require(ggcorrplot)
library(xts)
library("readxl")
setwd("C:/Users/serca/OneDrive/Masaüstü/Spring 22'/IE 360/HW1")

##### Data Reading, Merging and Creating a Time Series Object ##############
total_exports = read_excel("Total_Exports.xlsx")
total_exports = data.table(total_exports)

export_receivables = read_excel("Export_Receivables.xlsx")
export_receivables = data.table(export_receivables)

gold_production = read_excel("Gold_Production_KG.xlsx")
gold_production = data.table(gold_production)

google_trends_koza = read.csv("KOZAL_Google_Trends.csv")
google_trends_koza = data.table(google_trends_koza)


export_table = merge(total_exports, export_receivables)
data_ = merge(export_table, gold_production)
data = merge(data_, google_trends_koza)
data$Date <- as.Date(as.yearmon(x = data$Date))
colnames(data) <- c("Date","Total_Export", "Export_Receivables", "Gold_Production_KG", "Google_Trends_Koza")
data$Google_Trends_Koza <- as.numeric((data$Google_Trends_Koza))
#data<-data[,c(2,3,4)]
str(data)

#data_xts <- xts(data[ , colnames(data) != "Date"], data$Date)
data_xts = xts(x = data, data$Date)

str(data_xts)


####### Visualizing the data with ggplot2 package ###########
ggplot(data,aes(x=Date))+
  geom_line(size=1,color="brown",aes(y=Total_Export,group =1))+
  ggtitle("Monthly Total Exports in Thousand USD")

ggplot(data,aes(x=Date))+
  geom_line(size=1,color="brown",aes(y=Export_Receivables,group =1))+
  ggtitle("Monthly Export_Receivables in Million USD")

ggplot(data,aes(x=Date))+
  geom_line(size=1,color="brown",aes(y=Gold_Production_KG,group =1))+
  ggtitle("Monthly Gold Mining of Public Companies in KG's")

ggplot(data,aes(x=Date))+
  geom_line(size=1,color="brown",aes(y=Google_Trends_Koza,group =1))+
  ggtitle("Monthly Google Search Trend for the KOZAA")

tmp=copy(data)

tmp$Total_Export_Normalized=(tmp$Total_Export-min(tmp$Total_Export))/(max(tmp$Total_Export)-min(tmp$Total_Export))

tmp$Export_Receivables_Normalized=(tmp$Export_Receivables-min(tmp$Export_Receivables))/(max(tmp$Export_Receivables)-min(tmp$Export_Receivables))

tmp$Gold_Production_KG_Normalized=(tmp$Gold_Production_KG-min(tmp$Gold_Production_KG))/(max(tmp$Gold_Production_KG)-min(tmp$Gold_Production_KG))


ggplot(tmp)+geom_line(aes(x=Date, y=Total_Export_Normalized,color="Total_Export_Normalized"))+
  geom_line(aes(x=Date, y=Export_Receivables_Normalized ,color="Export_Receivables_Normalized"))+
  geom_line(aes(x=Date, y=Gold_Production_KG_Normalized , color= "Gold_Production_KG_Normalized"))+
  ggtitle("Comparision of Normalized Datas")+
  ylab("Normalized Values")+xlab("Date (Monthly)")
#################### Checking and plotting correlation data ########
ggpairs(data)

corr_data = data[,c(2,3,4,5)]

correl_info=cor(corr_data)

ggcorrplot(correl_info, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


lm_base=lm(Total_Export~ Google_Trends_Koza+Gold_Production_KG + Export_Receivables,data)
summary(lm_base)

checkresiduals(lm_base$residuals)

checkresiduals(lm_base)


tmp=copy(data)
tmp[,actual:=Total_Export]
tmp[,predicted_trend:=predict(lm_base,tmp)]
tmp[,residual_trend:=actual-predicted_trend]
head(tmp)
ggplot(tmp ,aes(x=Date)) +
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))

