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
myFile <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
dim(myFile)
head(myFile)
attach(myFile)
myFile$Group <- as.factor(myFile$Group)
myFile$Group = factor(myFile$Group, labels = c("grup1", "grup2", "grup3"))

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

# 4B
carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

```r
barlett.test(Length~Group, data = myFile)
```

# 4C 
Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
Grup dan beri nama model tersebut model 1.

```r
model1 = lm(Length~Group, data = myFile)
anova(model1)
```

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


# No 5



