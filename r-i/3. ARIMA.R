library(readxl)
harga_beras =
  read_excel("C:\R\ARIMA.xlsx")

# Prosedur Box Jenkins
# 1. Identifikasi
# 2. Estimasi Model
# 3. Evaluasi Model
# 4. Peramalan

View(ARIMA)

# Mendefinisikan Time Series
ARIMA = ARIMA[,c(-1)]
ARIMA = ts(ARIMA, start=c(2012, 1), frequency = 12) # Freq = Jumlah observasi per unit waktu. 12 == bulanan, 4==kuartalan, 1==tahunan
# fungsi *ts* tidak cocok dengan data harian, harian == ARCH/GARCH
# Berurutan sesuai data, start, dan frequency

plot(ARIMA, main="Harga Beras di Perdagangan Besar")

dekomposisi=decompose(ARIMA) # dekomposisi harga beras (random, seasonal, trend, observed) == pertimbangan dalam membuat model
plot(dekomposisi)

# BOX JENKINS
# Perilaku korelogram ACF dan PCAF (AR, MA, ARMA)
par (mfrow=c(2, 1)) # Jendela plot 2 baris 1 kolom
acf(ARIMA, lag=48)
pacf(ARIMA, lag=48)

# Uji Stationeritas
install.packages("aTSA")
library(aTSA)
adf.test(ARIMA)
adf.test(diff(ARIMA))

# Pemilihan Model Terbaik
install.packages("forecast")
library(forecast)
auto.arima(ARIMA, trace=TRUE)

install.packages("lmtest")
library(lmtest)
model1 = arima(ARIMA, order=c(0,1,3),
               seasonal = list(order=c(2,1,0), period=12))
coeftest(model1)

# Uji Heteroskedastisitas == Memutuskan cukup ARIMA atau perlu dilanjutkan ARCH/GARCH
arch.test(model1)

# Uji Keacakan Residual
install.packages("devtools")
library(devtools)
install_github("cran/FitAR")
install.packages("FitAR")
githubinstall("FitAR")
library(FitAR)
LjungBoxTest(model1$residuals)

# Permalan
par (mfrow=c(1, 1))
forecast(model1, h=12)
plot(forecast(model1, h=12))