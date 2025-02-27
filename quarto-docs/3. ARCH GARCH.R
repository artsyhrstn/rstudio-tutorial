library(readxl)
kurs = read_excel("C:\Users\hisbi\iCloudDrive\repository\R-tutorial\data - R IPB\Bab 3\ARCH-GARCH.xlsx")

kurs = kurs[,c(-1)] # Menghapus kolom satu untuk mendefinisikan data kurs sebagai data time series
library(xts) # Lebih cocok untuk data harian

# Seq 0 membuat objek waktu dates
Dates =
