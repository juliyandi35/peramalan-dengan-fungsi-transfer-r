#-NOTES :
#-apabila packages dibawah ini belum diintall, silahakn diinstall terlabih dahulu
library(tfarima)
library(astsa)
library(forecast)
library(tseries)
library(TTR)
library(TSA)
library(graphics)
library(MTS)
library(readxl)

data <- read_excel('Data Penelitian2.xlsx')
data$`Curah Hujan` <- as.numeric(data$`Curah Hujan`) # as numeric
data$Kelembaban <- as.numeric(data$Kelembaban) # as numeric
data$`Suhu Udara` <- as.numeric(data$`Suhu Udara`) # as numeric
data$`Kecepatan Angin` <- as.numeric(data$`Kecepatan Angin`) # as numeric
dim(data) # size data
y <- data$`Curah Hujan`
x1 <- data$Kelembaban
X1 <- ts(data$Kelembaban,frequency = 12)
x2 <- data$`Suhu Udara`
X2 <- ts(data$`Suhu Udara`,frequency = 12)
x3 <- data$`Kecepatan Angin`
X3 <- ts(data$`Kecepatan Angin`,frequency = 12)
par(mfrow = c(2,2))
plot.ts(X1,ylab="Kelembaban",
        xlab="Periode waktu",
        main="Plot Time Series Kelembaban",
        col="blue")
plot.ts(X2,ylab="Suhu Udara",
        xlab="Periode waktu",
        main="Plot Time Series Suhu Udara",col="yellow")
plot.ts(X3,ylab="Kecepatan Angin",
      xlab="Periode waktu",
      main="Plot Time Series Kecepatan angin",col="green")

Y <- ts(data$`Curah Hujan`,frequency = 12) # Inflasi as Input
plot.ts(Y,
        ylab="Curah Hujan",
        xlab="Periode waktu",
        main="Plot Time Series Curah Hujan",
        col="red")

Xt <- cbind(X1,X2,X3)
Xt
Yt <- Y
Yt
par(mfrow = c(1,1))
# Split the data
train_percentage <- 0.7
train_size <- round(train_percentage * nrow(data))
X.train <- Xt[1:train_size, ]
X.test <- Xt[(train_size + 1):nrow(data), ]
plot.ts(X.train, col="blue", lty=1,main="Data X (Data Latih)")

Y.train <- Yt[1:train_size]
Y.test <- Yt[(train_size + 1):nrow(data)]
plot.ts(Y.train, col="red" , lty=1, main="Data Y (Data Latih)",
        ylab = "Curah Hujan",
        xlab = "Periode waktu")

adf.test(Y.train)

acf(Y.train, lag.max=12)
axis(1, at=1:12, labels=1:12)

pacf(Y.train, lag.max=12)
axis(1, at=1:12, labels=1:12)

eacf(Y.train)

arima203 <- arima(Y.train,xreg=X.train,order=c(2,0,3),method = "ML")
arima303 <- arima(Y.train,xreg=X.train,order=c(3,0,3),method = "ML")
arima403 <- arima(Y.train,xreg=X.train,order=c(4,0,3),method = "ML")
arima503 <- arima(Y.train,xreg=X.train,order=c(5,0,3),method = "ML")

# Forecasting for Accuracy
forecast203 = forecast(arima203$coef,h=10)
forecast303 = forecast(arima303$coef,h=10)
forecast403 = forecast(arima403$coef,h=10)
forecast503 = forecast(arima503$coef,h=10)

accuracy(forecast203)
accuracy(forecast303)
accuracy(forecast403)
accuracy(forecast503)

mm1=arima(Y.train,xreg=X.train,order=c(4,0,3),method = "ML")
mm1
#Box-L Jung test
sisaan1 <- residuals(mm1)
ljung <- Box.test(sisaan1, lag=23, type="Ljung")
ljung
library(tseries)
jarque.bera.test(residuals(mm1))
qqnorm(sisaan1)
qqline(sisaan1)

checkresiduals(mm1)
f1=c(1,-mm1$coef) # Creates a filter to transform Y
f1

#Use convolution method for AR model, recursive method for MA model.

acf(Y.train)

adf.test(Y.train)

Yf=filter(Y.train,f1,method=c("convolution"),sides=1)

yprev=Yf[12:84] # transformed Y
xprev=mm1$residuals[12:84] # transformed X

CCF=ccf(yprev,xprev) # computes the cross-correlations
CCF # retrieves the cross-correlations

vk=(sd(yprev)/sd(xprev))*CCF$acf # impulse response function
print(vk)

ACF=acf(yprev) # autocorrelations of transformed Y
plot(CCF, ylab="CCF",main="Cross-correlations after prewhitening")

###########################
# using tfarima
###########################
#acf and pacf plot for X
#acf(X, lag.max=24)
#pacf(X, lag.max=24)
#mengindikasikan AR(3)

umx <- um(X.train, ar=4, ma = 1)
umy <- fit(umx, Y.train)

#This umx model is used to prewhiten the input X and the output Y .
#The residuals() function of the tfarima package compute the conditional
#or exact residuals for a time series from an object of class um:

a <- residuals(umx, Y.train, method = "cond")
b <- residuals(umx, X.train, method = "cond")

#Now we can use the ccf() function of the stats package to display the estimated cross
#correlation function for the data after filtering. Alternatively, we can use the
#pccf() function of the tfarima defined as
pccf(a, b, um.x = umx, um.y = NULL, lag.max = 16)

#or
CCF =ccf(a,b, ylab="CCF", main="Cross-correlations after prewhitening")

# Karena tidak ada penundaan, maka nilai b = 0
# Karena lag setelah lag pertama tidak signifikan, mana s=0
# Karena terdapat pola sinus terendam, maka r=2
# r=2,s=0,b=0
#delay=b
#r=p
#s=q

tfx <- tfest(Y.train, X.train, delay = 0, p = 2, q = 0, um.x = umx, um.y = umy)
tfx
summary(tfx)
print(tfx$theta)
print(tfx$phi)

# to fitting the TF model can be estimated as follows

noise.um <- um(ar=4)
class(noise.um)

tfmy <- tfm(Y.train, X.train, b = 0, s = 0, p = 2, q = 0)

tfmy$nt


#predict.tfm
tfmy <- tfarima::tfm(Y.train, inputs = tfx, noise = um(ar=4, ma=1))
p <- predict.tfm(tfmy, n.ahead = 10)
p

plot(p, n.back = 60)

accuracy1 <- accuracy(p$z, Y.test)
accuracy1
