library(plotly)
trend<- read.csv('~/Documents/FinalProject/Data/trend2014.csv')
df2014<-data.frame(month<-c(8,9,10,11,12),counter<-c(31980, 19013, 28540,22285,29733))
p <- plot_ly(data = df2014, x = month, y = counter, name = "trend in 2014")
p %>% add_trace(y = fitted(loess(counter ~ as.numeric(month))))
p %>% add_trace(y = fitted(counter ~ month))

df2015<-data.frame(month<-c(1,2,3,4,5,6,7,8,9,10,11,12),counter<-c(584915, 1198642, 20786,13202,18617,12930,13689,13070,20496,14747,17603,32149))
p <- plot_ly(data = df2015, x = month, y = counter, name = "trend in 2015")
p %>% add_trace(y = fitted(counter ~ month))

df2016<-data.frame(month<-c(1,2,3,4),counter<-c(142009, 1009735, 26712,19233))
p <- plot_ly(data = df2016, x = month, y = counter, name = "trend in 2016")
p %>% add_trace(y = fitted(counter ~ month))

df<-data.frame(month<-c(2014.08,2014.09,2014.10,2014.11,2014.12,2015.01,2015.02,2015.03,2015.04,2015.05,2015.06,2015.07,2015.08,2015.09,2015.10,2015.11,2015.12,2016.01,2016.02,2016.03,2016.04),counter<-c(31980, 19013, 28540,22285,29733,584915, 1198642, 20786,13202,18617,12930,13689,13070,20496,14747,17603,32149,142009, 1009735, 26712,19233))
p <- plot_ly(data = df, x = month, y = counter, name = "trend")
p %>% add_trace(y = fitted(counter ~ month))

beyonce<- read.csv('~/Documents/FinalProject/Data/beyonce.csv')

p <- plot_ly(data = df, x = hour, y = counter, name = "Beyonce")
p %>% add_trace(y = fitted(counter ~ hour))


df<-data.frame(hour<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),counter<-c(66,62,160,189,71,116,93,61,75,66,75,98,331,231,442,486,867,862,950,992,2315,519,1426,342))
p <- plot_ly(data = df, x = hour, y = counter, name = "trend")
p %>% add_trace(y = fitted(counter ~ hour))


df<-data.frame(month<-c(2014.08,2014.09,2014.10,2014.11,2014.12,2015.01,2015.02,2015.03,2015.04,2015.05,2015.06,2015.07,2015.08,2015.09,2015.10,2015.11,2015.12,2016.01,2016.02,2016.03,2016.04),counter<-c(1419,1673,1584,1794,2624,30665,45769,833,683,953,490,761,873,1302,789,987,2013,6120,25379,542,359))
p <- plot_ly(data = df, x = month, y = counter, name = "trend")
p %>% add_trace(y = fitted(counter ~ month))

