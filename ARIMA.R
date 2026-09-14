# ARIMA Model
# Step 1: Import data
library(readxl)
data = read_excel('Data Penelitian2.xlsx')

# Step 2: Attach file nya
attach(data)

# Step 3: Install package yang digunakan
# install.packages(tseries)
# install.packages(forecast)
# Step 4: Load package yang digunakan
library(tseries)
library(forecast)

# Step 5: Aplikasikan test stationeritas
plot.ts(data$`Curah Hujan`)
adf.test(data$`Curah Hujan`)

# Jika data tidak stationer atau p-value lebih dari 0,05
rcurahhujan = diff(log(data$`Curah Hujan`))
plot.ts(rcurahhujan)
adf.test(rcurahhujan)

# Step 6: Gunakan auto.arima untuk melihat model ARIMA terbaik
auto.arima(data$`Curah Hujan`,stepwise=FALSE, approximation=FALSE)

# Step 7: Buat modelnya
modelrcurahhujan = arima(data$`Curah Hujan`,order = c(4,0,1))
modelrcurahhujan

# Step 8: Diagnostic check
resid = residuals(modelrcurahhujan)
acf(resid)
plot.ts(resid)
gghistogram(resid)
Box.test(resid,lag = 10 ,type = "Ljung-Box")

# Step 9: Forecast datanya
forecastcurahhujan = forecast(modelrcurahhujan,h=10)
forecastcurahhujan
plot(forecastcurahhujan)
accuracy(forecastcurahhujan)
