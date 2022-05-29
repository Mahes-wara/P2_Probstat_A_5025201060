Nama | NRP
---------- | ----------
Maheswara Danendra Satriananda | 5025201060

# No 1
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap
kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat
kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali
kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas A

![image](https://user-images.githubusercontent.com/73051874/170871152-874a6f3f-490c-4027-8d23-7fccb574b192.png)

```r
orang = c(seq(1:9))
sebelum = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
sesudah = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
data = data.frame(orang, sebelum, sesudah)
n = 9
```

Data Penelitian

# 1A
Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel
diatas

```r
dif = data$sesudah - data$sebelum
cat("Standart Deviasi selisih pengamatan = ")
dev = sd(dif)
dev
```

# 1B
carilah nilai t (p-value)

```r
p = 0
xbar = mean(dif)
stat = ((xbar - p)/(dev/sqrt(n)))
value = 2 * pt(-abs(stat), df = n-1)
value
```

# 1C 
tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada
pengaruh yang signifikan secara statistika dalam hal kadar saturasi
oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”

```r
var.test(sebelum, sesudah)
t.test(sebelum, sesudah, mu = 0, alternative = "two.sided"
       , var.equal = TRUE)
```
Berikut adalah hasil run dari script
![image](https://user-images.githubusercontent.com/73051874/170871719-531e836d-8d27-4d1c-ab70-b939cec2a03a.png)


# No 2
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
23.500 kilometer dan standar deviasi 3900 kilometer.

```r
zsum.test(mean.x = 23500, sigma.x = 3900, n.x = 100,
          alternative = "greater", mu = 20000,
          conf.level = 0.95)
```

# 2A
Setuju, karena kesimpulan dari uji z menolak H0, sehingga mobil dikemudikan rata-rata lebih dari 20000 kilometer per tahun

# 2B
Output dari z test adalah, hipotesis alternatif alternative hypothesis: true mean is greater than 20000 atau H1 diterima sehingga klaim benar. 

# 2C
P-value dari uji tes z adalah 2.2e-16 atau mendekati 0, dari hasil p-value tersebut hipotesis awal dapat ditolak dan H1 diterima.


# No 3
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.

![image](https://user-images.githubusercontent.com/73051874/170872559-81ef5956-328a-4de2-af65-979e1eecca80.png)

# 3A
H0 dan H1
H0 = (miu1 = miu2)
H1 = (miu1 != miu2) 

# 3B
Hitung Sampel Statistik

```r
tsum.test(mean.x = 3.63, s.x = 1.67, n.x = 19,
          mean.y = 2.79, s.y = 1.32, n.y = 27,
          alternative = "greater", var.equal = TRUE)
```

Hasil run script

![image](https://user-images.githubusercontent.com/73051874/170873135-04a54139-9646-405f-ba0b-663eefc0b4ed.png)

# 3C
Lakukan Uji Statistik (df =2)

```r
plotDist(dist = 't', df = 2, col = "red")
```

Hasil run script

![image](https://user-images.githubusercontent.com/73051874/170873396-3c061c1b-fe34-4d67-b8f7-6a5b17a7d7b0.png)

# 3D
Nilai Kritikal

```r
qt(p = 0.05, df = 2, lower.tail = FALSE)
```

Hasil run script
![image](https://user-images.githubusercontent.com/73051874/170873569-1a68dd3c-1076-4687-aa3a-6910d31441f7.png)

# 3E
Keputusan
Karena p-value < a maka Hipotesi ditolak

# 3F
Kesimpulan
Tidak ada perbedaan pada rata-rata jumlah saham perusahaan di dua kota tersebut


# No 4
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika :
diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya
sama

```r
myFile <- read.delim(file.choose())

myFile$Group <- as.factor(myFile$Group)
myFile$Group = factor(myFile$Group,labels = c("grup1", "grup2", "grup3"))

class(myFile$Group)
```

#4A
Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
lihat apakah ada outlier utama dalam homogenitas varians

```r
grup1 <- subset(myFile, Group == "grup1")
grup2 <- subset(myFile, Group == "grup2")
grup3 <- subset(myFile, Group == "grup3")

qqnorm(grup1$Length)
qqline(grup1$Length)

qqnorm(grup2$Length)
qqline(grup2$Length)

qqnorm(grup3$Length)
qqline(grup3$Length)
```

berdasarkan plot kuantil normal di atas, tidak ditemukan outlier utama pada homogenitas varians
![image](https://user-images.githubusercontent.com/73051874/170880059-1ec5efb4-d991-4510-9575-65f5dface82e.png)


# 4B
carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

```r
bartlett.test(Length~Group, data = myFile)
```

![image](https://user-images.githubusercontent.com/73051874/170880170-0ecd2411-d607-4748-978e-4fc5829dbf9e.png)


# 4C 
Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
Grup dan beri nama model tersebut model 1.

```r
model1 = lm(Length~Group, data = myFile)
anova(model1)
```

![image](https://user-images.githubusercontent.com/73051874/170880222-1ff79266-35a2-48fd-85a7-97fff371444c.png)

# 4D
nilai p adalah 0.0013 dimana kurang dari 0.005, sehingga h0 ditolak

# 4E
Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0? Setelah di jalankan maka nilai p-value = 0.8054.

# 4F
Visualisasikan data dengan ggplot2

```r
library("ggplot2")

ggplot(myFile, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "green", colour = "yellow") +
  scale_x_discrete() + xlab("Group") + ylab("Length")
```

![image](https://user-images.githubusercontent.com/73051874/170880245-c70b704a-9228-4220-b25d-330d42b6354a.png)


# No 5
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen. Dengan data tersebut:

```r
library(readr)
library(multcompView)
library(dplyr)
library(ggplot2)

gtl <- read.csv(file.choose())
```

# 5A
Buatlah plot sederhana untuk visualisasi data

```r
qplot(x = Temp, y = Light, geom = "point", data = gtl) + 
  facet_grid(.~Glass, labeller = label_both)
```

![image](https://user-images.githubusercontent.com/73051874/170880686-5d91a474-4bcb-4901-ba2b-8e39982f4ff4.png)

# 5B
Lakukan uji ANOVA dua arah

```r
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gt)

anova <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(anova)
```

![image](https://user-images.githubusercontent.com/73051874/170880880-06ec0971-4d25-4307-b801-27e313b0b796.png)

# 5C
Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

```r
data_sumarry <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_sumarry)
```

![image](https://user-images.githubusercontent.com/73051874/170881114-7b476f49-6e2e-46c2-8ba2-0ce142268e90.png)

# 5D
Lakukan uji Tukey

```r
print("tukey test : ")
tukey <- TukeyHSD(anova)
print(tukey)
```

![image](https://user-images.githubusercontent.com/73051874/170881268-a9b95701-c735-4395-b98b-1e234cb55f47.png)

![image](https://user-images.githubusercontent.com/73051874/170881286-61a8af21-e595-4a5c-b95c-b257112efd9a.png)

# 5E
Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

```r
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)
```

![image](https://user-images.githubusercontent.com/73051874/170881384-1351ba69-00c4-4455-a053-44562286140d.png)
