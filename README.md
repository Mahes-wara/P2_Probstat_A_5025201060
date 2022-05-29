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
