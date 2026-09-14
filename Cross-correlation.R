## Cross Correlation Function ##

# Baca data
library(readxl)
data = read_excel('Data Penelitian2.xlsx')
# fix(data)
data = data[-1]

# Cari Koefisien Korelasi
cor_coef = cor(data) # Jika terdapat error "'x' must be numeric, gunakan solusi ini"
is.na(cor_coef)
# cor_coef = cor(data[sapply(data,is.numeric)])
# cor_coef # Ini array yang ada NA nya
# cor_coef = cor(data[sapply(data,is.numeric)],use="complete.obs")
# cor_coef = cor(data[sapply(data,is.numeric)],use="pairwise.complete.obs")
# cor_coef # Akan dihitung nilai korelasi antar 2 variabel

# Hapus nilai NA yang ada
data = na.omit(data)

# Cross-correlation Kelembaban - Curah Hujan
# Temukan Cross Correlation
ccf(data$Kelembaban,data$`Curah Hujan`,main = "Cross-correlation Kelembaban dan Curah Hujan")

# Tampilkan nilai lag nya
ccfvalues1 = ccf(data$Kelembaban,data$`Curah Hujan`,main = "Cross-correlation Kelembaban dan Curah Hujan")
ccfvalues1

# Cross-correlation Suhu udara - Curah Hujan
# Temukan Cross Correlation
ccf(data$`Suhu Udara`,data$`Curah Hujan`,main = "Cross-correlation Suhu Udara dan Curah Hujan")

# Tampilkan nilai lag nya
ccfvalues2 = ccf(data$`Suhu Udara`,data$`Curah Hujan`,main = "Cross-correlation Suhu Udara dan Curah Hujan")
ccfvalues2

# Cross-correlation Kecepatan angin - Curah Hujan
# Temukan Cross Correlation
ccf(data$`Kecepatan Angin`,data$`Curah Hujan`,main = "Cross-correlation Kecepatan Angin dan Curah Hujan")

# Tampilkan nilai lag nya
ccfvalues3 = ccf(data$`Kecepatan Angin`,data$`Curah Hujan`,main = "Cross-correlation Kecepatan Angin dan Curah Hujan")
ccfvalues3
