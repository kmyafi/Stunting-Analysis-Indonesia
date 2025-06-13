# Semifinal NSC 2023 - Team Lunar Universitas Indonesia ----
# NSC23A008
#1. Kamal Muftie Yafi
#2. Nasywa Safira Ardanty

## Load package ----
library(cluster)
library(dplyr)
library(ggplot2)

## Load data ----
library(readxl)
nsc <- read_excel("D:/OneDrive - UNIVERSITAS INDONESIA/Competition/NSC UB 2023/SEMIFINAL NSC 2023/Data Semifinal Analisis Data NSC 2023.xlsx")
nsc <- as.data.frame(nsc)
View(nsc)
str(nsc)

#set specific column as row names
rownames(nsc) <- nsc$Provinsi

#remove original column from data frame
nsc$Provinsi <- NULL
nsc$No. <- NULL

nsc <- nsc %>% 
  rename('Xa1' = '0 bulan',
         'Xb1' = '0-5 bulan',
         'Xc1' = '6-11 bulan',
         'Xd1' = '12-23 bulan',
         'Xe1' = '24-35 bulan',
         'Xf1' = '36-47 bulan',
         'Xg1' = '48-59 bulan',
         'X2' = 'Presentase Bayi yang Menerima ASI Eksklusif (%)',
         'X3' = 'Rata-Rata Kalori yang Dikonsumi per Kapita (Kkal)',
         'X4' = 'Rata-Rata Protein yang Dikonsumi per Kapita (Gram)',
         'X5' = 'Presentase Rumah Tangga Memanfaatkan Air Bekas untuk Keperluan Lain (%)',
         'X6' = 'Presentase Gizi Kurang Balita (%)',
         'X7' = 'Rata-Rata Umur Ibu Ketika Kehamilan Pertama (Tahun)')

# Pilih variabel yang akan digunakan untuk analisis klaster
X <- nsc[, c('Xa1', 'Xb1', 'Xc1', 'Xd1', 'Xe1', 'Xf1', 'Xg1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7')]

nsc_z <- scale(nsc)

# menghitung distance
nsc_dist <- dist(x = nsc_z, method = "euclidean"); nsc_dist

nsc_hc_complete <- hclust(d = nsc_dist, method = "complete")

complete_clust <- cutree(nsc_hc_complete,  k = 2)
View(complete_clust)

library(factoextra)
fviz_dend(nsc_hc_complete, k = 2, k_colors = "jco", rect = T, 
          main = "Complete Linkage Cluster")

complete_coph <- cophenetic(nsc_hc_complete)
cor(complete_coph, nsc_dist)

library(clValid)
# internal measures
internal <- clValid(nsc_z, nClust = 2:4, 
                    clMethods = "agnes", 
                    validation = "internal", 
                    metric = "euclidean",
                    method = "complete")

summary(internal)

# stabilitas measures
stability <- clValid(nsc_z, nClust = 2:4, 
                     clMethods = "agnes", 
                     validation = "stability", 
                     metric = "euclidean",
                     method = "complete")

# hanya menampilkan skor optimal
optimalScores(stability)

metode_c1<-hclust(dist(scale(nsc)),method = "complete")
plot(metode_c1)


#Ward method
metode_ward<-hclust(dist(scale(nsc)),method = "ward.D")
plot(metode_ward)
rect.hclust(metode_ward, 2)

rect.hclust(metode_c1, 2)

anggota<-cutree(metode_c1, 2)
anggota
tabel=data.frame(anggota)
tabel
nsc$Label <- tabel$anggota

## Analisis Diskriminan
library(MASS)
model <- lda(Label~X2+X3+X4+X5+X6+X7, data = nsc)
model

lda.data <- cbind(nsc, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

# Memisahkan dataset menjadi data latih dan data uji
set.seed(123) # Untuk hasil yang dapat direproduksi
sample_size <- floor(0.7 * nrow(nsc))
train_indices <- sample(seq_len(nrow(nsc)), size = sample_size)
data_train <- nsc[train_indices, ]
data_test <- nsc[-train_indices, ]

# Membuat model analisis diskriminan
disc_model <- lda(Label ~ X2 + X3 + X4 + X5 + X6 + X7, data = data_train)

# Melakukan prediksi dengan model analisis diskriminan
predictions <- predict(disc_model, newdata = data_test)
predictions
# Menampilkan hasil prediksi
table(predictions$class, data_test$Label)

# Menampilkan statistik diskriminan
print(disc_model)

shapiro.test(nsc$X2)
shapiro.test(nsc$X3)
shapiro.test(nsc$X4)
shapiro.test(nsc$X5)
shapiro.test(nsc$X6)
shapiro.test(nsc$X7)

library(car)
model <- lm(Label ~ ., data = nsc)
vif(model)
plot(model)

library("olsrr")
ols_vif_tol(model)
