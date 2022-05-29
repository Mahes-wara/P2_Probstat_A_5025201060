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



