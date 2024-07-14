# Import Excel
library(readxl)
data1 <- read_excel('C:\\Users\\hisbi\\iCloudDrive\\repository\\R-tutorial\\data-i\\Bab 2\\data1.xlsx')
length(data1)

# Mengganti nama variabel
names(data1)[2] <- "Outstanding KPR"
names(data1)[7] <- "Inflasi"
names(data1)[8] <- "CreditRate"

# Estimasi Regresi OLS
regresi1 = lm(LnKPR ~ LnPDB + Inflasi + CreditRate + LnJII + DFTV, data=data1)
summary(regresi1)

# Interpretasi


# Uji Normalitas
# install.packages("gvlma")
library("gvlma")
gvlma(regresi1)

# Heteroksedastisitas