# VAR
install.packages("urca")
install.packages("vars")
library(urca)
library(vars)

# Mendefinisikan data menjadi time series
tsdata = data1[,c(-1,-2,-3,-4)]
View(data1)
tsdata = ts(tsdata, start=c(2010, 1),
            frequency = 12)
plot(tsdata)
plot(tsdata, nc=2, xlab="") # nc == menetapkan jumlah kolom pada jendela plot
                            # xlab == memberi label pada sumbu x
                            # kata awal == disimpan sebagai tsdata

# Uji Stationeritas
library(aTSA)
adf.test(tsdata[,"LnIHK"])
adf.test(tsdata[,"LnRM2"])
adf.test(tsdata[,"LnIPI"])
adf.test(diff(tsdata[,"LnIHK"]))
adf.test(diff(tsdata[,"LnRM2"]))
adf.test(diff(tsdata[,"LnIPI"]))

# Uji Lag Optimum
VARselect(tsdata, lag.max = 10)

# Uji Kointegrasi
cointest_eigen = ca.jo(tsdata, K=2,
                       type="eigen", ecdet="const", spec="longrun")
summary(cointest_eigen)