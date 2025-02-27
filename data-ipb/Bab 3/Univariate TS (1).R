## ARIMA
## import data from excel

# set variabel hargaberas sebagai objek time series bulanan
hargaberas = ARIMA
hargaberas = hargaberas[,c(-1)]
View(hargaberas)
hargaberas = ts(hargaberas, start=c(2012,1), frequency=12) #ts untuk mendefenisikan data timeseries 
hargaberas = ts(hargaberas, start=c(2012,1), frequency=12) #bulanan
hargaberas = ts(hargaberas, start=c(2012,1), frequency=4) #kuatalan 
hargaberas = ts(hargaberas, start=2012, frequency=1) #tahunan
?ts
hargaberas
par(mfrow=c(1,1))
plot(hargaberas, main="Harga Beras di Perdagangan Besar")
dekomposisi = decompose(hargaberas)
plot(dekomposisi)

# plot acf dan pacf
par(mfrow=c(2,1)) #untuk membuat jendela window menjadi dua baris satu kolom
acf(hargaberas, lag=48)
pacf(hargaberas, lag=48)
?acf
# uji stasioneritas
install.packages("aTSA")
library(aTSA)
library(help = aTSA)
?adf.test
adf.test(hargaberas) #pada level
adf.test(diff(hargaberas))

# fitur pemilihan model terbaik secara otomatis
install.packages("forecast")
library(forecast)
auto.arima(hargaberas, trace=TRUE)
?auto.arima

library(help = forecast)
install.packages("lmtest")
library(lmtest)
#ARIMA(0,1,3)(2,1,0)[12] model terbaik karena AIC terkecil
model1 = arima(hargaberas, order=c(0,1,3), seasonal=list(order=c(2,1,0), period=12)) #Model arima manual
coeftest(model1)
?coeftest
?arima

#these seasonal fluctuations can sometimes be so large that they can mask important business trends hiding in the data.
# uji heteroskedastisitas --> cukup ARIMA atau perlu dilanjutkan dengan ARCH/GARCH
arch.test(model1)
?arch.test
# uji diagnostik
install.packages("FitAR")
library(FitAR)
LjungBoxTest(model1$residuals)
?LjungBoxTest

# peramalan
?forecast
forecast(model1, h=12)
plot(forecast(model1, h=12))
par(mfrow=c(1,1))

### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### 

## ARCH-GARCH
## import data from excel

# set variabel hargaberas sebagai objek time series harian
kurs = ARCH_GARCH
View(kurs)
kurs = kurs[,c(-1)]
library(xts)
Dates = seq(as.Date("2019-01-01"), as.Date("2020-12-31"), "day") 
kurs = xts(kurs, order.by = Dates)
View(kurs)
?seq
?xts
plot(kurs, main="Nilai Tukar US Dollar terhadap Rupiah")

# fitur pemilihan model terbaik secara otomatis
auto.arima(kurs, trace=TRUE)

#ARIMA(2,1,2) model terbaik karena AIC terkecil
model2 = arima(kurs, order=c(2,1,2))
coeftest(model2)

# uji heteroskedastisitas --> cukup ARIMA atau perlu dilanjutkan dengan ARCH/GARCH
arch.test(model2)

# dilanjutkan ke pemodelan ARCH/GARCH
install.packages("fGarch")
library(fGarch)


# differencing variabel kurs
pp.test(kurs)
pp.test(diff(kurs))
e = diff(kurs)[-1]
par(mfrow=c(1,1))
acf(e)
pacf(e)
plot(e)
View(e)

# ARCH(1) = GARCH(1,0)
?garchFit
model10 = garchFit(~garch(1,0), data=e, trace=FALSE)
summary(model10)


# GARCH(1,1)
model11 = garchFit(~garch(1,1), data=e, trace=FALSE)
summary(model11)

# GARCH(1,1) dengan mean equation ARMA(0,1)
model11b = garchFit(~arma(0,1)+garch(1,1), data=e, trace=FALSE)
summary(model11b)

#Model terbaik adalah model "model11b"
# plot simpangan baku
Dates2 = seq(as.Date("2019-01-02"), as.Date("2020-12-31"), "day") 
stdev = xts(model11b@sigma.t, order.by = Dates2)
plot(stdev, main="Simpangan Baku")

# peramalan
predict(model11b, n.ahead=20, plot=TRUE, nx=731)
?predict


library(aTSA)
?forecast
library(forecast)
?forecast
