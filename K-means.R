# Membaca data
data <- read.csv("E:/KULIAH/MAGISTER/BISNIS ANALITIK/Praktikum_Unsupervised.csv",sep = ",")
head(data)

# Menghilangkan kolom 1 dan 2
data_unsu = data[ ,-c(1,2)] 
head(data_unsu)

# Mengganti nama kolom
colnames(data_unsu) = c("Umur", "Pendapatan", "Skor")
head(data_unsu)

# Cek missing value
summary(data_unsu)
sum(is.na(data_unsu))

# Menentukan k optimum
library(factoextra)
fviz_nbclust(data_unsu, kmeans, method = "wss") 
fviz_nbclust(data_unsu, kmeans, method = "silhouette") 

# Algoritma clutsering
set.seed(123)
k.means.fit = kmeans(data_unsu, iter.max = 1000, 7)
k.means.fit$centers
k.means.fit

# Menghitung Silhouette score
library(cluster)
jarak <- as.matrix(dist(data_unsu))
score <- mean(silhouette(k.means.fit$cluster,dmatrix=jarak)[,3])
print(paste("silhouette score = ", round(score, 3)))

# Visualisasi
clusplot(data_unsu, k.means.fit$cluster, main = "Visualisasi Cluster",
         color = TRUE, shade = TRUE, lines = 0)

# =====================================================================

# Menghitung matriks distance
d <- dist(x = data_unsu, method = "euclidean")

# Membangun Cluster
hc_single <- hclust(d = d, method = "single")

# Membuat Dendogram
plot(hc_single, hang = -1)

library(dplyr)

# Tabel Hasil
cut_point_1 = cutree(hc_single, k = 3) #Memilih sebanyak 7 klaster
databind_1 <- cbind(data_unsu, Cluster = cut_point_1)  # Bind data
head(databind_1)

# Mengitung Shilouette Score
jarak = as.matrix(d)
score <- mean(silhouette(databind_1$Cluster,dmatrix=jarak)[,3])
print(paste("silhouette score = ", round(score, 3)))

# Menghitung Karakteristik
data_unsu %>%
  group_by(databind_1$Cluster) %>%
  summarise_all(funs(mean = mean, median = median)) %>%
  as.data.frame()
