#Data aktivitas
orang = c(seq(1:9))
sebelum = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
sesudah = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
data = data.frame(orang, sebelum, sesudah)
n = 9

#a
dif = data$sesudah - data$sebelum
cat("Standart Deviasi selisih pengamatan = ")
dev = sd(dif)
dev

#b
p = 0 
xbar = mean(dif)
stat = ((xbar - p)/(dev/sqrt(n)))
value = 2 * pt(-abs(stat), df = n-1)
value

#c
var.test(sebelum, sesudah)
t.test(sebelum, sesudah, mu = 0, alternative = "two.sided"
       , var.equal = TRUE)

