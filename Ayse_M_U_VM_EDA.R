#Ayşe Mustafaoğlu Uysal 

#EDA Sentetik Veri Üreterek
# Yeni bir örnek veri seti oluşturma
set.seed(456)
n <- 150
x1 <- rnorm(n, mean = 8, sd = 2) # Yeni bağımsız değişken 1
x2 <- rnorm(n, mean = 6, sd = 1.5) # Yeni bağımsız değişken 2
x3 <- rnorm(n, mean = 4, sd = 1) # Yeni bağımsız değişken 3
y <- 5 + 1.5*x1 - 0.8*x2 + 0.3*x3 + rnorm(n, mean = 0, sd = 1.8) # Yeni bağımlı değişken

# Rasgele eksik değerler oluşturma
missing_rate <- 0.1 # Eksiklik oranı (örneğin, yüzde 10)
x1_missing_indices <- sample(1:n, size = floor(missing_rate * n)) # x1'deki eksik verilerin indeksleri
x2_missing_indices <- sample(1:n, size = floor(missing_rate * n)) # x2'de eksik verilerin indeksleri
x3_missing_indices <- sample(1:n, size = floor(missing_rate * n)) # x3'te eksik verilerin indeksleri

# Eksik verileri atanabilir hale getirme
x1[x1_missing_indices] <- NA
x2[x2_missing_indices] <- NA
x3[x3_missing_indices] <- NA

# Veri çerçevesine dönüştürme
new_data_missing <- data.frame(y, x1, x2, x3)

# Veri setini gösterme
head(new_data_missing)

# Boyutların kontrolü
print(dim(new_data_missing))

# Bir veri çerçevesinin yapısını özetleme
str(new_data_missing)

# Yinelenen satırların kontrolü
print(sum(duplicated(new_data_missing)))

# Eksik değerlerin sayısının bulunması ve sonuçların yazdırılması
missing_values <- colSums(is.na(new_data_missing))
print(missing_values)

# Veri çerçevesinin tanımlayıcı istatistiklerini özetleme
summary(new_data_missing)

# Bir sütunun ortalama değerini hesaplama
mean(new_data_missing$y)

# Eksik değerleri sütunun ortalaması ile doldurma
mean_x1 <- mean(new_data_missing$x1, na.rm = TRUE)  # Sütunun ortalamasını hesapla
new_data_missing$x1 <- ifelse(is.na(new_data_missing$x1), mean_x1, new_data_missing$x1)  # Eksik değerleri ortalamayla doldur
# Eksik değerleri sütunun ortancası ile doldurma
median_x1 <- median(new_data_missing$x2, na.rm = TRUE)  # Sütunun medyanını hesapla
new_data_missing$x2 <- ifelse(is.na(new_data_missing$x1), median_x2, new_data_missing$x2)  # Eksik değerleri medyanla doldur
# Eksik değerleri sütunun ortalaması ile doldurma
mean_x2 <- mean(new_data_missing$x2, na.rm = TRUE)  # Sütunun ortalamasını hesapla
new_data_missing$x2 <- ifelse(is.na(new_data_missing$x2), mean_x2, new_data_missing$x2)  # Eksik değerleri ortalamayla doldur

# 'impute' paketini yükleme
#install.packages("impute")
#library(impute)

#Diğer yöntemler
# KNN ile eksik değerleri doldurma
#filled_data <- impute.knn(new_data_missing)

# 'zoo' paketini yükleme
#install.packages("zoo")
#library(zoo)
# LOCF ile eksik değerleri doldurma
#new_data_missing$x2 <- na.locf(new_data_missing$x2, na.rm = FALSE)  # NA değerlerini doldur

# Lineer interpolasyon ile eksik değerleri doldurma
#new_data_missing$x2 <- na.approx(new_data_missing$x2)

# Bir sütunun ortalama değerini hesaplama
mean(new_data_missing$x1)
# Bir sütunun ortalama değerini hesaplama
mean(new_data_missing$x2)

# Aykırı değer tespiti için kutu grafiği oluşturma
boxplot(new_data_missing$x1)

# Alt ve üst sınırları hesaplama
Q1 <- quantile(new_data_missing$x1, 0.25)
Q3 <- quantile(new_data_missing$x1, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- new_data_missing$x1 < lower_bound | new_data_missing$x1 > upper_bound

# Histogram oluşturma
hist(new_data_missing$x1, main = "Histogram", xlab = "Değerler", ylab = "Frekans")

# Kutu grafiği oluşturma
boxplot(new_data_missing$x1, main = "Kutu Grafiği", ylab = "Değerler")

# Q-Q plot oluşturma
qqnorm(new_data_missing$x1)
qqline(new_data_missing$x1)

qqnorm(new_data_missing$x2)
qqline(new_data_missing$x2)

# Kantillerden yararlanma
summary(new_data_missing$x1)

# Korelasyon matrisini hesaplama
correlation_matrix <- cor(new_data_missing)

# Korelasyon matrisini görüntüleme
print(correlation_matrix)
# NA değerlerini ortalama ile doldurma
filled_correlation_matrix <- apply(correlation_matrix, 1, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Korelasyon matrisini görüntüleme
print(filled_correlation_matrix)

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)

#Veri Ölçekleme
# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(y_standardized)

#Son Değişiklik
#Test-Train
# caret paketini yükleme
install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
# Veriyi eğitim ve test alt kümelerine bölmek için indeksleri oluşturma
set.seed(456)
train_index <- createDataPartition(new_data_missing$y, p = 0.7, list = FALSE)

# Eğitim ve test alt kümelerini oluşturma
train_data <- new_data_missing[train_index, ]
test_data <- new_data_missing[-train_index, ]

#son kontroller
# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)

# Eğitim verisinin ilk birkaç satırını görüntüleme
head(train_data)

# Test verisinin ilk birkaç satırını görüntüleme
head(test_data)



#EDA Veri Çekerek
# Gerekli kütüphaneleri yükleme
library(readr) # Veriyi okumak için
library(dplyr) # Veri işleme için
library(ggplot2) # Görselleştirme için

#1.Adım Veri Kümesini Tanımlama

# CSV dosyasını URL kullanarak okuma ve boş değerleri NA ile doldurma
netflix_data <- read.csv("https://raw.githubusercontent.com/ygterl/EDA-Netflix-2020-in-R/master/netflix_titles.csv", na.strings = c("", "NA"), stringsAsFactors =FALSE)

# Baştan ve sondan satırlar yazdırma
head(netflix_data)
tail(netflix_data)

# Veri kümesinin yapısını inceleme
str(netflix_data)

# Temel istatistiksel özet
summary(netflix_data)

# Boyutların kontrolü
print(dim(netflix_data))

# Yinelenen satırların kontrolü
print(sum(duplicated(netflix_data)))

# Eksik değerlerin sayısının bulunması ve sonuçların yazdırılması
missing_values <- colSums(is.na(netflix_data))
print(missing_values)

# Veriyi görselleştirme
# Örneğin, film ve TV şovlarının türlerini gösteren bir çubuk grafik oluşturma
ggplot(netflix_data, aes(x = type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Shows in Each Type", x = "Type", y = "Count") +
  theme_minimal()

#2.Adım: Veri Temizleme ve Düzenleme/Veri Kalitesi

# Yeni bir data frame oluşturarak eksik değerlerin yazdırılması
data.frame("Variable"=c(colnames(netflix_data)), "Missing Values"=sapply(netflix_data, function(x) sum(is.na(x))), row.names=NULL)

#Rating
#Rating değeri için eksiklerin mod ile doldurulması
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

netflix_data$rating[is.na(netflix_data$rating)] <- mode(netflix_data$rating)

data.frame("Variable"=c(colnames(netflix_data)), "Missing Values"=sapply(netflix_data, function(x) sum(is.na(x))), row.names=NULL)


#Direktor, cast, country, date_added 
#Direktor, cast, country, date_added eksik değerlerin veri setinden silinmesi (numerik olmayan özel değerler olduğu için silme yöntemi kullandım)

# Eksik değerleri kontrol etme
missing_values <- colSums(is.na(netflix_data))
print(missing_values)

# Eksik değerleri içeren satırları kaldırma
cleaned_data <- netflix_data[complete.cases(netflix_data), ]

# Eksik değerleri kontrol etme
missing_values <- colSums(is.na(cleaned_data))
print(missing_values)

#title, country, type and release_year
#Tekrarlayan değerlerin ayıklanması
cleaned_data=distinct(cleaned_data, title, country, type, release_year, .keep_all = TRUE)

# Aykırı değerlerin hangi ülkelerde olduğunun belirlenmesi (değere ulaşılamadı!)
outlier_countries <- netflix_data$country[which(netflix_data$duration %in% outliers)]

view(outlier_country_counts)

#3.Veri Görselleştirme
#Gerekli Kütüphaneler
library(tibble)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(crayon)
install.packages("plotly")
library(plotly)

#Şimdi veri temizliği öncesi ve sonrası kategorik analizler arasındaki farkları inceleyelim.

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla
df_cats <- as.data.frame(table(netflix_data$type))
names(df_cats) <- c("Cat", "Count")

# Görselleştirmeyi oluşturma
plt <- ggplot(df_cats, aes(x = Count, y = reorder(Cat, Count))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Category of Movies", x = "Count", y = "Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip()

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla
df_cats_2 <- as.data.frame(table(cleaned_data$type))
names(df_cats_2) <- c("Cat", "Count")

# Görselleştirmeyi oluşturma
plt_2 <- ggplot(df_cats_2, aes(x = Count, y = reorder(Cat, Count))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Category of Movies", x = "Count", y = "Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  coord_flip()

# Görselleştirmeyi görüntüleme
print (plt_2)

# Görselleştirmeyi görüntüleme
print(plt)

# İki görselleştirmeyi aynı sayfada görüntüleme
grid.arrange(plt, plt_2, ncol = 2)

#Oransal farkın arttığı açık bir şekilde gözlenmiştir.


# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla
df_cats <- as.data.frame(table(netflix_data$rating))
names(df_cats) <- c("Rating", "Count")

df_cats_2 <- as.data.frame(table(cleaned_data$rating))
names(df_cats_2) <- c("Rating", "Count")

# Görselleştirmeyi oluşturma
plt <- ggplot(df_cats, aes(x = Rating, y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Ratings", x = "Rating", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plt_2 <- ggplot(df_cats_2, aes(x = Rating, y = Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Ratings", x = "Rating", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Görselleştirmeyi görüntüleme
grid.arrange(plt, plt_2, ncol = 2)

#Rating kategorisinde büyük değişiklikler gözlenmemiştir.


# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla - Veri seti 1
df_listed_in <- as.data.frame(table(netflix_data$listed_in))
names(df_listed_in) <- c("Listed_in", "Count")

# Kategorileri frekanslarına göre sırala - Veri seti 1
df_listed_in <- df_listed_in[order(-df_listed_in$Count), ]

# Top 10 kategoriyi seç - Veri seti 1
top_10_listed_in <- head(df_listed_in, 10)

# Görselleştirmeyi oluşturma - Veri seti 1
plt <- ggplot(top_10_listed_in, aes(x = reorder(Listed_in, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Top 10 Listed_in Categories (Dataset 1)", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla - Veri seti 2
df_listed_in_2 <- as.data.frame(table(cleaned_data$listed_in))
names(df_listed_in_2) <- c("Listed_in", "Count")

# Kategorileri frekanslarına göre sırala - Veri seti 2
df_listed_in_2 <- df_listed_in_2[order(-df_listed_in_2$Count), ]

# Top 10 kategoriyi seç - Veri seti 2
top_10_listed_in_2 <- head(df_listed_in_2, 10)

# Görselleştirmeyi oluşturma - Veri seti 2
plt_2 <- ggplot(top_10_listed_in_2, aes(x = reorder(Listed_in, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Top 10 Listed_in Categories (Dataset 2)", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Görselleştirmeleri yan yana görüntüleme
grid.arrange(plt, plt_2, ncol = 2)


# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla - Veri seti 1
df_country <- as.data.frame(table(netflix_data$country))
names(df_country) <- c("Country", "Count")
df_director <- as.data.frame(table(netflix_data$director))
names(df_director) <- c("Director", "Count")

# Kategorileri frekanslarına göre sırala - Veri seti 1
df_country <- df_country[order(-df_country$Count), ]
df_director <- df_director[order(-df_director$Count), ]

# Top 5 kategoriyi seç - Veri seti 1
top_5_country <- head(df_country, 5)
top_5_director <- head(df_director, 5)

# Görselleştirmeyi oluşturma - Veri seti 1
plt_country <- ggplot(top_5_country, aes(x = reorder(Country, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 5 Countries (Dataset 1)", x = "Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plt_director <- ggplot(top_5_director, aes(x = reorder(Director, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 5 Directors (Dataset 1)", x = "Director", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla - Veri seti 2
df_country_2 <- as.data.frame(table(cleaned_data$country))
names(df_country_2) <- c("Country", "Count")
df_director_2 <- as.data.frame(table(cleaned_data$director))
names(df_director_2) <- c("Director", "Count")

# Kategorileri frekanslarına göre sırala - Veri seti 2
df_country_2 <- df_country_2[order(-df_country_2$Count), ]
df_director_2 <- df_director_2[order(-df_director_2$Count), ]

# Top 5 kategoriyi seç - Veri seti 2
top_5_country_2 <- head(df_country_2, 5)
top_5_director_2 <- head(df_director_2, 5)

# Görselleştirmeyi oluşturma - Veri seti 2
plt_country_2 <- ggplot(top_5_country_2, aes(x = reorder(Country, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Countries (Dataset 2)", x = "Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plt_director_2 <- ggplot(top_5_director_2, aes(x = reorder(Director, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Directors (Dataset 2)", x = "Director", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Görselleştirmeleri yan yana görüntüleme
grid.arrange(plt_country, plt_director, plt_country_2, plt_director_2, ncol = 2)

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla
df_country <- as.data.frame(table(netflix_data$country))
names(df_country) <- c("Country", "Count")

# Kategorileri frekanslarına göre sırala
df_country <- df_country[order(-df_country$Count), ]

# Top 10 kategoriyi seç
top_10_country <- head(df_country, 10)

# Görselleştirmeyi oluşturma
barplot(top_10_country$Count, names.arg = top_10_country$Country, col = "orange",
        main = "Top 10 Countries by Netflix Content",
        xlab = "Country", ylab = "Count",
        cex.names = 0.8, las = 2)

# Veri setindeki kategorik değişkenleri ve frekanslarını hesapla
df_country_2 <- as.data.frame(table(cleaned_data$country))
names(df_country_2) <- c("Country", "Count")

# Kategorileri frekanslarına göre sırala
df_country_2 <- df_country_2[order(-df_country$Count), ]

# Top 10 kategoriyi seç
top_10_country <- head(df_country_2, 10)

# Görselleştirmeyi oluşturma
barplot(top_10_country$Count, names.arg = top_10_country$Country, col = "skyblue",
        main = "Top 10 Countries by Netflix Content",
        xlab = "Country", ylab = "Count",
        cex.names = 0.8, las = 2)

#İstatiksel değişiklikler gözlenmiştir.


