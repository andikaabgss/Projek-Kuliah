library(readxl)
library(tseries)
library(lmtest)
library(MASS)
library(forecast)

dataadw = read_excel('C:/Users/A S U S/Downloads/inflasi dan ihk.xlsx')
dataadw

inflasi = ts(dataadw$Inflasi, start = c(2018, 1), frequency = 12)
plot.ts(inflasi,
        main = "Plot Deret Waktu Inflasi Kota Bekasi (2018-2025)", 
        ylab = "Inflasi",
        xlab = "Tahun",
        lwd = 1.5, 
        type = "o")


#Uji Stasioneritas Ragam
c = 1.001 - min(inflasi)
inflasipositif = inflasi + c
plot.ts(inflasipositif, type = 'o')

bc = boxcox(lm(inflasipositif~1))
lambda = bc$x[which.max(bc$y)]
lambda 

inflasitrans = log(inflasipositif)
plot(inflasitrans)

bct = boxcox((inflasitrans~1))
lambdat = bct$x[which.max(bct$y)]
lambdat

#Uji Stasioneritas Rata-rata
adf.test(inflasitrans, alternative = 'stationary')

inflasi.diff1 = diff(inflasitrans, differences = 1)
plot.ts(inflasi.diff1, type = 'o')
adf.test(inflasi.diff1, alternative = 'stationary')

#Identifikasi Model
acf(inflasi.diff1) #cut off di lag 2
pacf(inflasi.diff1) #cut off di lag 4
#model ARIMA(4,1,1)

#pendugaan parameter
arima411 = Arima(inflasi.diff1,
                 order = c(4,1,1),
                 method = 'ML')
summary(arima411)
coeftest(arima411)

#diagnostik model
jarque.bera.test(arima411$residuals)

Box.test(arima411$residuals, type = "Ljung-Box")

 #peramalan
ramal = forecast(arima411, h = 5)
plot(ramal)
lines (fitted(arima411),col="red",lwd=2)
legend("topleft",legend=c("Dataaktual","ARIMA(4,1,1)", 'Ramalan'),
       lty=c(1,1,1), lwd=c(1,2,1), col=c
       ("black","red", 'blue1'), cex=0.7)

