library(readxl)
hargaberas <- read_excel('C:\\Users\\Thinkpad\\iCloudDrive\\repository\\R-tutorial\\data-i\\Bab 3\\ARIMA.xlsx')

## Prosedur Box Jenkins
# 1. Identifikasi
# 2. Estimasi Model
# 3. Evaluasi Model
# 4. Peramalan

View(hargaberas)

# Mendefinisikan Time Series
hargaberas = hargaberas[,c(-1)]
hargaberas = ts(hargaberas, start=c(2012, 1), frequency = 12) # Freq = Jumlah observasi per unit waktu. 12 == bulanan, 4==kuartalan, 1==tahunan
# fungsi *ts* tidak cocok dengan data harian, harian == ARCH/GARCH
# Berurutan sesuai data, start, dan frequency

plot(hargaberas, main="Harga Beras di Perdagangan Besar")

dekomposisi=decompose(hargaberas) # dekomposisi harga beras (random, seasonal, trend, observed) == pertimbangan dalam membuat model
plot(dekomposisi)

# BOX JENKINS
# Perilaku korelogram ACF dan PCAF (AR, MA, ARMA)
par (mfrow=c(2, 1)) # Jendela plot 2 baris 1 kolom
acf(hargaberas, lag=48)
pacf(hargaberas, lag=48)

# Uji Stationeritas
install.packages("aTSA")
library(aTSA)
adf.test(hargaberas)
adf.test(diff(hargaberas))

# Pemilihan Model Terbaik
install.packages("forecast")
library(forecast)
auto.arima(hargaberas, trace=TRUE)

install.packages("lmtest")
library(lmtest)
model1 = arima(hargaberas, order=c(0,1,3),
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