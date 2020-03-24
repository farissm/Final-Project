library(ggplot2)
library(readxl)
library(xlsx)
library(rsq)
library(ggthemes)
library(Metrics)
library(caTools)

data.parkir <- read_excel("D:\\KULIAH\\Mata Kuliah\\TA\\Data\\Data Parkiran.xlsx", sheet = "Sheet1")
entire.workbook <- lapply(excel_sheets('D:\\KULIAH\\Mata Kuliah\\TA\\Data\\Data Parkiran.xlsx'), read_excel, path = 'D:\\KULIAH\\Mata Kuliah\\TA\\Data\\Data Parkiran.xlsx')

data.waktu <- data.parkir$Waktu
data.senin <- data.parkir$Senin
data.selasa <- data.parkir$Selasa
data.rabu <- data.parkir$Rabu
data.kamis <- data.parkir$Kamis
data.jumat <- data.parkir$Jumat
data.sabtu <- data.parkir$Sabtu
data.minggu <- data.parkir$Minggu

sample.senin <- sample.split(data.senin, SplitRatio = 0.8)
sample.selasa <- sample.split(data.selasa, SplitRatio = 0.8)
sample.rabu <- sample.split(data.rabu, SplitRatio = 0.8)
sample.kamis <- sample.split(data.kamis, SplitRatio = 0.8)
sample.jumat <- sample.split(data.jumat, SplitRatio = 0.8)
sample.sabtu <- sample.split(data.sabtu, SplitRatio = 0.8)
sample.minggu <- sample.split(data.minggu, SplitRatio = 0.8)

train.senin <- subset(data.senin, sample.senin == T, na.rm = T)
train.selasa <- subset(data.selasa, sample.selasa == T, na.rm = T)
train.rabu <- subset(data.rabu, sample.rabu == T, na.rm = T)
train.kamis <- subset(data.kamis, sample.kamis == T, na.rm = T)
train.jumat <- subset(data.jumat, sample.jumat == T, na.rm = T)
train.sabtu <- subset(data.sabtu, sample.sabtu == T, na.rm = T)
train.minggu <- subset(data.minggu, sample.minggu == T, na.rm = T)

test.senin <- subset(data.senin, sample == F)
test.selasa <- subset(data.selasa, sample == F)
test.rabu <- subset(data.rabu, sample == F)
test.kamis <- subset(data.kamis, sample == F)
test.jumat <- subset(data.jumat, sample == F)
test.sabtu <- subset(data.sabtu, sample == F)
test.minggu <- subset(data.minggu, sample == F)

grafik.senin <- ggplot(data.parkir, aes(x = Waktu, y = Senin )) + ggtitle("Senin") + xlab("Waktu") + ylab("Mobil") + geom_point()
grafik.selasa <- ggplot(data.parkir, aes(x = Waktu, y = Selasa )) + ggtitle("Selasa") + xlab("Waktu") + ylab("Mobil") + geom_point() 
grafik.rabu <- ggplot(data.parkir, aes(x = Waktu, y = Rabu)) + ggtitle("Rabu") + xlab("Waktu") + ylab("Mobil") + geom_point() 
grafik.kamis <- ggplot(data.parkir, aes(x = Waktu, y = Kamis )) + ggtitle("Kamis") + xlab("Waktu") + ylab("Mobil") + geom_point() 
grafik.jumat <- ggplot(data.parkir, aes(x = Waktu, y = Jumat )) + ggtitle("Jumat") + xlab("Waktu") + ylab("Mobil") + geom_point()
grafik.sabtu <- ggplot(data.parkir, aes(x = Waktu, y = Sabtu )) + ggtitle("Sabtu") + xlab("Waktu") + ylab("Mobil") + geom_point()
grafik.minggu <- ggplot(data.parkir, aes(x = Waktu, y = Minggu )) + ggtitle("Minggu") + xlab("Waktu") + ylab("Mobil") + geom_point()
m <- length(data.waktu)

x <- 0
# hari
kapasitas.func.senin <- function(){
  a <- 1
  
  for(i in 1:length(train.senin)){
    x[a] <- train.senin[a] 
    a <- a+1
  }
  return(x)
}
# kapasitas.func.senin()

kapasitas.func.selasa <- function(){
  a <- 1
  
  for(i in 1:length(train.selasa)){
    x[a] <- train.selasa[a] 
    a <- a+1
  }
  return(x)
}
# kapasitas.func.selasa()

kapasitas.func.rabu <- function(){
  a <- 1
  
  for(i in 1:length(train.rabu)){
    x[a] <- train.rabu[a] 
    a <- a+1
  }
  return(x)
}
kapasitas.func.rabu()

kapasitas.func.kamis <- function(){
  a <- 1
  
  for(i in 1:length(train.kamis)){
    x[a] <- train.kamis[a] 
    a <- a+1
  }
  return(x)
}
kapasitas.func.kamis()

kapasitas.func.jumat <- function(){
  a <- 1
  
  for(i in 1:length(train.jumat)){
    x[a] <- train.jumat[a] 
    a <- a+1
  }
  return(x)
}
kapasitas.func.jumat()

kapasitas.func.sabtu <- function(){
  a <- 1
  
  for(i in 1:length(train.sabtu)){
    x[a] <- train.sabtu[a] 
    a <- a+1
  }
  return(x)
}
kapasitas.func.sabtu()

kapasitas.func.minggu <- function(){
  a <- 1
  
  for(i in 1:length(train.minggu)){
    x[a] <- train.minggu[a] 
    a <- a+1
  }
  return(x)
}
kapasitas.func.minggu()

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

waktu.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]
    a <- a+1
  }
  return(x)
}


waktu2.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}


waktu3.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}


waktu4.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}


waktu5.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}

waktu6.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}

waktu7.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}

waktu8.func <- function(){
  a <- 1
  for(i in 1:length(data.waktu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]
    a <- a+1
  }
  return(x)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------

KapasitasWaktu.func.senin <- function(){
  a <- 1
  for(i in 1:length(train.senin)) {
    x[a] <- data.waktu[a]*train.senin[a] 
    a <- a+1
  }
  return(x)
}
# KapasitasWaktu.func()

KapasitasWaktu.func.selasa <- function(){
  a <- 1
  for(i in 1:length(train.selasa)) {
    x[a] <- data.waktu[a]*train.selasa[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu.func.rabu <- function(){
  a <- 1
  for(i in 1:length(train.rabu)) {
    x[a] <- data.waktu[a]*train.rabu[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu.func.kamis <- function(){
  a <- 1
  for(i in 1:length(train.kamis)) {
    x[a] <- data.waktu[a]*train.kamis[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu.func.jumat <- function(){
  a <- 1
  for(i in 1:length(train.jumat)) {
    x[a] <- data.waktu[a]*train.jumat[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu.func.sabtu <- function(){
  a <- 1
  for(i in 1:length(train.sabtu)) {
    x[a] <- data.waktu[a]*train.sabtu[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu.func.minggu <- function(){
  a <- 1
  for(i in 1:length(train.minggu)) {
    x[a] <- data.waktu[a]*train.minggu[a] 
    a <- a+1
  }
  return(x)
}

# ----------------------------------------------------------------------------------------------------------------------------------------------------

KapasitasWaktu2.func.senin <- function(){
  a <- 1
  for(i in 1:length(train.senin)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.senin[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.selasa <- function(){
  a <- 1
  for(i in 1:length(train.selasa)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.selasa[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.rabu <- function(){
  a <- 1
  for(i in 1:length(train.rabu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.rabu[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.kamis <- function(){
  a <- 1
  for(i in 1:length(train.kamis)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.kamis[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.jumat <- function(){
  a <- 1
  for(i in 1:length(train.jumat)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.jumat[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.sabtu <- function(){
  a <- 1
  for(i in 1:length(train.sabtu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.sabtu[a] 
    a <- a+1
  }
  return(x)
}

KapasitasWaktu2.func.minggu <- function(){
  a <- 1
  for(i in 1:length(train.minggu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*train.minggu[a] 
    a <- a+1
  }
  return(x)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------

KapasitasWaktu3.func.senin <- function(){
  a <- 1
  for(i in 1:length(train.senin)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.senin[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.selasa <- function(){
  a <- 1
  for(i in 1:length(train.selasa)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.selasa[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.rabu <- function(){
  a <- 1
  for(i in 1:length(train.rabu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.rabu[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.kamis <- function(){
  a <- 1
  for(i in 1:length(train.kamis)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.kamis[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.jumat <- function(){
  a <- 1
  for(i in 1:length(train.jumat)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.jumat[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.sabtu <- function(){
  a <- 1
  for(i in 1:length(train.sabtu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.sabtu[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu3.func.minggu <- function(){
  a <- 1
  for(i in 1:length(train.minggu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*train.minggu[a] #set harinya
    a <- a+1
  }
  return(x)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------------

KapasitasWaktu4.func.senin <- function(){
  a <- 1
  for(i in 1:length(train.senin)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.senin[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.selasa <- function(){
  a <- 1
  for(i in 1:length(train.selasa)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.selasa[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.rabu <- function(){
  a <- 1
  for(i in 1:length(train.rabu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.rabu[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.kamis <- function(){
  a <- 1
  for(i in 1:length(train.kamis)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.kamis[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.jumat <- function(){
  a <- 1
  for(i in 1:length(train.jumat)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.jumat[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.sabtu <- function(){
  a <- 1
  for(i in 1:length(train.sabtu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.sabtu[a] #set harinya
    a <- a+1
  }
  return(x)
}

KapasitasWaktu4.func.minggu <- function(){
  a <- 1
  for(i in 1:length(train.minggu)) {
    x[a] <- data.waktu[a]*data.waktu[a]*data.waktu[a]*data.waktu[a]*train.minggu[a] #set harinya
    a <- a+1
  }
  return(x)
}

# ----------------------------------------------------------------------------Linear Least Square---------------------------------------------------------------------------------------

a0.senin <- ((sum(waktu2.func()) * sum(kapasitas.func.senin())) - (sum(KapasitasWaktu.func.senin()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.senin <- ((m * sum(KapasitasWaktu.func.senin())) - (sum(waktu.func()) * sum(kapasitas.func.senin()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.selasa <- ((sum(waktu2.func()) * sum(kapasitas.func.selasa())) - (sum(KapasitasWaktu.func.selasa()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.selasa <- ((m * sum(KapasitasWaktu.func.selasa())) - (sum(waktu.func()) * sum(kapasitas.func.selasa()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.rabu <- ((sum(waktu2.func()) * sum(kapasitas.func.rabu())) - (sum(KapasitasWaktu.func.rabu()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.rabu <- ((m * sum(KapasitasWaktu.func.rabu())) - (sum(waktu.func()) * sum(kapasitas.func.rabu()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.kamis <- ((sum(waktu2.func()) * sum(kapasitas.func.kamis())) - (sum(KapasitasWaktu.func.kamis()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.kamis <- ((m * sum(KapasitasWaktu.func.kamis())) - (sum(waktu.func()) * sum(kapasitas.func.kamis()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.jumat <- ((sum(waktu2.func()) * sum(kapasitas.func.jumat())) - (sum(KapasitasWaktu.func.jumat()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.jumat <- ((m * sum(KapasitasWaktu.func.jumat())) - (sum(waktu.func()) * sum(kapasitas.func.jumat()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.sabtu <- ((sum(waktu2.func()) * sum(kapasitas.func.sabtu())) - (sum(KapasitasWaktu.func.sabtu()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.sabtu <- ((m * sum(KapasitasWaktu.func.sabtu())) - (sum(waktu.func()) * sum(kapasitas.func.sabtu()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

a0.minggu <- ((sum(waktu2.func()) * sum(kapasitas.func.minggu())) - (sum(KapasitasWaktu.func.minggu()) * sum(waktu.func()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))
a1.minggu <- ((m * sum(KapasitasWaktu.func.minggu())) - (sum(waktu.func()) * sum(kapasitas.func.minggu()))) / ((m * (sum(waktu2.func()))) - (sum(waktu.func()) * sum(waktu.func())))

LLS.senin <- function(x){
  yi <- a1.senin * x + a0.senin
  return(yi)
}

LLS.selasa <- function(x){
  yi <- a1.selasa * x + a0.selasa
  return(yi)
}

LLS.rabu<- function(x){
  yi <- a1.rabu * x + a0.rabu
  return(yi)
}

LLS.kamis <- function(x){
  yi <- a1.kamis * x + a0.kamis
  return(yi)
}

LLS.jumat <- function(x){
  yi <- a1.jumat * x + a0.jumat
  return(yi)
}

LLS.sabtu <- function(x){
  yi <- a1.sabtu * x + a0.sabtu
  return(yi)
}

LLS.minggu <- function(x){
  yi <- a1.minggu * x + a0.minggu
  return(yi)
}
# ----------------------------------------------------------------------------Linear Least Square---------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------Polynomial Least Square orde 2----------------------------------------------------------------------------

v1 <- c(m, sum(waktu.func()), sum(waktu2.func()))
v2 <- c(sum(waktu.func()), sum(waktu2.func()), sum(waktu3.func()))
v3 <- c(sum(waktu2.func()), sum(waktu3.func()), sum(waktu4.func()))

vr2.sn <- c(sum(kapasitas.func.senin()), sum(KapasitasWaktu.func.senin()), sum(KapasitasWaktu2.func.senin()))
vr2.sl <- c(sum(kapasitas.func.selasa()), sum(KapasitasWaktu.func.selasa()), sum(KapasitasWaktu2.func.selasa()))
vr2.rb <- c(sum(kapasitas.func.rabu()), sum(KapasitasWaktu.func.rabu()), sum(KapasitasWaktu2.func.rabu()))
vr2.km <- c(sum(kapasitas.func.kamis()), sum(KapasitasWaktu.func.kamis()), sum(KapasitasWaktu2.func.kamis()))
vr2.jm <- c(sum(kapasitas.func.jumat()), sum(KapasitasWaktu.func.jumat()), sum(KapasitasWaktu2.func.jumat()))
vr2.sb <- c(sum(kapasitas.func.sabtu()), sum(KapasitasWaktu.func.sabtu()), sum(KapasitasWaktu2.func.sabtu()))
vr2.mg <- c(sum(kapasitas.func.minggu()), sum(KapasitasWaktu.func.minggu()), sum(KapasitasWaktu2.func.minggu()))

v.orde2 <- c(v1, v2, v3)
vmat2 <- matrix(v.orde2, byrow = T, nrow = 3)

vrmat2.sn <- matrix(vr2.sn, byrow = T, nrow = 3)
vrmat2.sl <- matrix(vr2.sl, byrow = T, nrow = 3)
vrmat2.rb <- matrix(vr2.rb, byrow = T, nrow = 3)
vrmat2.km <- matrix(vr2.km, byrow = T, nrow = 3)
vrmat2.jm <- matrix(vr2.jm, byrow = T, nrow = 3)
vrmat2.sb <- matrix(vr2.sb, byrow = T, nrow = 3)
vrmat2.mg <- matrix(vr2.mg, byrow = T, nrow = 3)

vmat.inv2 <- solve(vmat2)

matrix.res2.sn <- vmat.inv2 %*% vrmat2.sn
matrix.res2.sl <- vmat.inv2 %*% vrmat2.sl
matrix.res2.rb <- vmat.inv2 %*% vrmat2.rb
matrix.res2.km <- vmat.inv2 %*% vrmat2.km
matrix.res2.jm <- vmat.inv2 %*% vrmat2.jm
matrix.res2.sb <- vmat.inv2 %*% vrmat2.sb
matrix.res2.mg <- vmat.inv2 %*% vrmat2.mg

result2.sn <- as.vector(matrix.res2.sn) 
result2.sl <- as.vector(matrix.res2.sl) 
result2.rb <- as.vector(matrix.res2.rb) 
result2.km <- as.vector(matrix.res2.km) 
result2.jm <- as.vector(matrix.res2.jm) 
result2.sb <- as.vector(matrix.res2.sb) 
result2.mg <- as.vector(matrix.res2.mg) 

PLS2.sn <- function(x){
  yi <- result2.sn[3] * (x^2) + result2.sn[2] * x + result2.sn[1]
  return(yi)
}
PLS2.sl <- function(x){
  yi <- result2.sl[3] * (x^2) + result2.sl[2] * x + result2.sl[1]
  return(yi)
}
PLS2.rb <- function(x){
  yi <- result2.rb[3] * (x^2) + result2.rb[2] * x + result2.rb[1]
  return(yi)
}
PLS2.km <- function(x){
  yi <- result2.km[3] * (x^2) + result2.km[2] * x + result2.km[1]
  return(yi)
}
PLS2.jm <- function(x){
  yi <- result2.jm[3] * (x^2) + result2.jm[2] * x + result2.jm[1]
  return(yi)
}
PLS2.sb <- function(x){
  yi <- result2.sb[3] * (x^2) + result2.sb[2] * x + result2.sb[1]
  return(yi)
}
PLS2.mg <- function(x){
  yi <- result2.mg[3] * (x^2) + result2.mg[2] * x + result2.mg[1]
  return(yi)
}

# ----------------------------------------------------------------------------Polynomial Least Square orde 2----------------------------------------------------------------------------


# ----------------------------------------------------------------------------Polynomial Least Square orde 4----------------------------------------------------------------------------

v4 <- c(m, sum(waktu.func()), sum(waktu2.func()), sum(waktu3.func()), sum(waktu4.func()))
v5 <- c(sum(waktu.func()), sum(waktu2.func()), sum(waktu3.func()), sum(waktu4.func()), sum(waktu5.func()))
v6 <- c(sum(waktu2.func()), sum(waktu3.func()), sum(waktu4.func()), sum(waktu5.func()), sum(waktu6.func()))
v7 <- c(sum(waktu3.func()), sum(waktu4.func()), sum(waktu5.func()), sum(waktu6.func()), sum(waktu7.func()))
v8 <- c(sum(waktu4.func()), sum(waktu5.func()), sum(waktu6.func()), sum(waktu7.func()), sum(waktu8.func()))

vr3.sn <- c(sum(kapasitas.func.senin()), sum(KapasitasWaktu.func.senin()), sum(KapasitasWaktu2.func.senin()), sum(KapasitasWaktu3.func.senin()), sum(KapasitasWaktu4.func.senin()))
vr3.sl <- c(sum(kapasitas.func.selasa()), sum(KapasitasWaktu.func.selasa()), sum(KapasitasWaktu2.func.selasa()), sum(KapasitasWaktu3.func.selasa()), sum(KapasitasWaktu4.func.selasa()))
vr3.rb <- c(sum(kapasitas.func.rabu()), sum(KapasitasWaktu.func.rabu()), sum(KapasitasWaktu2.func.rabu()), sum(KapasitasWaktu3.func.rabu()), sum(KapasitasWaktu4.func.rabu()))
vr3.km <- c(sum(kapasitas.func.kamis()), sum(KapasitasWaktu.func.kamis()), sum(KapasitasWaktu2.func.kamis()), sum(KapasitasWaktu3.func.kamis()), sum(KapasitasWaktu4.func.kamis()))
vr3.jm <- c(sum(kapasitas.func.jumat()), sum(KapasitasWaktu.func.jumat()), sum(KapasitasWaktu2.func.jumat()), sum(KapasitasWaktu3.func.jumat()), sum(KapasitasWaktu4.func.jumat()))
vr3.sb <- c(sum(kapasitas.func.sabtu()), sum(KapasitasWaktu.func.sabtu()), sum(KapasitasWaktu2.func.sabtu()), sum(KapasitasWaktu3.func.sabtu()), sum(KapasitasWaktu4.func.sabtu()))
vr3.mg <- c(sum(kapasitas.func.minggu()), sum(KapasitasWaktu.func.minggu()), sum(KapasitasWaktu2.func.minggu()), sum(KapasitasWaktu3.func.minggu()), sum(KapasitasWaktu4.func.minggu()))

v.orde3 <- c(v4, v5, v6, v7, v8)
vmat3 <- matrix(v.orde3, byrow = T, nrow = 5)

vrmat3.sn <- matrix(vr3.sn, byrow = T, nrow = 5)
vrmat3.sl <- matrix(vr3.sl, byrow = T, nrow = 5)
vrmat3.rb <- matrix(vr3.rb, byrow = T, nrow = 5)
vrmat3.km <- matrix(vr3.km, byrow = T, nrow = 5)
vrmat3.jm <- matrix(vr3.jm, byrow = T, nrow = 5)
vrmat3.sb <- matrix(vr3.sb, byrow = T, nrow = 5)
vrmat3.mg <- matrix(vr3.mg, byrow = T, nrow = 5)

vmat.inv3 <- solve(vmat3)

matrix.res3.sn <- vmat.inv3 %*% vrmat3.sn
matrix.res3.sl <- vmat.inv3 %*% vrmat3.sl
matrix.res3.rb <- vmat.inv3 %*% vrmat3.rb
matrix.res3.km <- vmat.inv3 %*% vrmat3.km
matrix.res3.jm <- vmat.inv3 %*% vrmat3.jm
matrix.res3.sb <- vmat.inv3 %*% vrmat3.sb
matrix.res3.mg <- vmat.inv3 %*% vrmat3.mg

result3.sn <- as.vector(matrix.res3.sn)
result3.sl <- as.vector(matrix.res3.sl) 
result3.rb <- as.vector(matrix.res3.rb) 
result3.km <- as.vector(matrix.res3.km) 
result3.jm <- as.vector(matrix.res3.jm) 
result3.sb <- as.vector(matrix.res3.sb) 
result3.mg <- as.vector(matrix.res3.mg) 

result3.sn
result3.sl
result3.rb
result3.km
result3.jm
result3.sb
result3.mg

PLS3.sn <- function(x){
  yi <- result3.sn[5] * (x^4) + result3.sn[4] * (x^3) + result3.sn[3] * (x^2) + result3.sn[2] * x + result3.sn[1]
  return(yi)
}
PLS3.sl <- function(x){
  yi <- result3.sl[5] * (x^4) + result3.sl[4] * (x^3) + result3.sl[3] * (x^2) + result3.sl[2] * x + result3.sl[1]
  return(yi)
}
PLS3.rb <- function(x){
  yi <- result3.rb[5] * (x^4) + result3.rb[4] * (x^3) + result3.rb[3] * (x^2) + result3.rb[2] * x + result3.rb[1]
  return(yi)
}
PLS3.km <- function(x){
  yi <- result3.km[5] * (x^4) + result3.km[4] * (x^3) + result3.km[3] * (x^2) + result3.km[2] * x + result3.km[1]
  return(yi)
}
PLS3.jm <- function(x){
  yi <- result3.jm[5] * (x^4) + result3.jm[4] * (x^3) + result3.jm[3] * (x^2) + result3.jm[2] * x + result3.jm[1]
  return(yi)
}
PLS3.sb <- function(x){
  yi <- result3.sb[5] * (x^4) + result3.sb[4] * (x^3) + result3.sb[3] * (x^2) + result3.sb[2] * x + result3.sb[1]
  return(yi)
}
PLS3.mg <- function(x){
  yi <- result3.mg[5] * (x^4) + result3.mg[4] * (x^3) + result3.mg[3] * (x^2) + result3.mg[2] * x + result3.mg[1]
  return(yi)
}

# ----------------------------------------------------------------------------Polynomial Least Square orde 3----------------------------------------------------------------------------

tes.LLS.sn <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.senin(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.sl <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.selasa(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.rb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.rabu(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.km <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.kamis(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.jm <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.jumat(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.sb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.sabtu(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.LLS.mg <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- LLS.minggu(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tes.PLS2.sn <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.sn(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.sl <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.sl(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.rb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.rb(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.km <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.km(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.jm <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.jm(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.sb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.sb(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS2.mg <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS2.mg(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tes.PLS3.sn <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.sn(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.sl <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.sl(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.rb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.rb(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.km <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.km(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.jm <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.jm(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.sb <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.sb(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
tes.PLS3.mg <- function(){
  a <- 1
  y <- 8
  for (i in 1:29) {
    x[a] <- PLS3.mg(y)
    a <- a + 1
    y <- y + 0.5
  }
  return(x)
}
# --------------------------------------------------------------------------------- print grafik ------------------------------------------------------------------------------------

pred.LLS.sn <- tes.LLS.sn()
pred.LLS.sl <- tes.LLS.sl()
pred.LLS.rb <- tes.LLS.rb()
pred.LLS.km <- tes.LLS.km()
pred.LLS.jm <- tes.LLS.jm()
pred.LLS.sb <- tes.LLS.sb()
pred.LLS.mg <- tes.LLS.mg()

pred.PLS2.sn <- tes.PLS2.sn()
pred.PLS2.sl <- tes.PLS2.sl()
pred.PLS2.rb <- tes.PLS2.rb()
pred.PLS2.km <- tes.PLS2.km()
pred.PLS2.jm <- tes.PLS2.jm()
pred.PLS2.sb <- tes.PLS2.sb()
pred.PLS2.mg <- tes.PLS2.mg()

pred.PLS3.sn <- tes.PLS3.sn()
pred.PLS3.sl <- tes.PLS3.sl()
pred.PLS3.rb <- tes.PLS3.rb()
pred.PLS3.km <- tes.PLS3.km()
pred.PLS3.jm <- tes.PLS3.jm()
pred.PLS3.sb <- tes.PLS3.sb()
pred.PLS3.mg <- tes.PLS3.mg()

print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Senin")  +geom_point(aes(y = data.senin, color = "Senin")) + geom_line(aes(y = pred.PLS3.sn, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.sn, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.sn, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Selasa")  +geom_point(aes(y = data.selasa, color = "Selasa")) + geom_line(aes(y = pred.PLS3.sl, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.sl, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.sl, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Rabu")  +geom_point(aes(y = data.rabu, color = "Rabu")) + geom_line(aes(y = pred.PLS3.rb, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.rb, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.rb, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Kamis")  +geom_point(aes(y = data.kamis, color = "Kamis")) + geom_line(aes(y = pred.PLS3.km, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.km, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.km, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Jumat")  +geom_point(aes(y = data.jumat, color = "Jumat")) + geom_line(aes(y = pred.PLS3.jm, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.jm, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.jm, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Sabtu")  +geom_point(aes(y = data.sabtu, color = "Sabtu")) + geom_line(aes(y = pred.PLS3.sb, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.sb, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.sb, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))
print(grafik.pred <- ggplot(data.parkir, aes(x = Waktu)) +  ggtitle("Minggu")  +geom_point(aes(y = data.minggu, color = "Minggu")) + geom_line(aes(y = pred.PLS3.mg, color = "Polynomial Least Square (4)")) + geom_line(aes(y = pred.PLS2.mg, color = "Polynomial Least Square (2)")) + geom_line(aes(y = pred.LLS.mg, color = "Linear Least Square")) + ylab("Cars") + xlab("Time") + theme_gray() + theme(legend.position = "bottom", legend.text = element_text(size = 9, face = "bold")))

# ------------------------------------------------------------------------------------- ERROR LLS ----------------------------------------------------------------------------------------

errLLS.sn <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.senin[a] - LLS.senin(y)) * (data.senin[a] - LLS.senin(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.sl <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.selasa[a] - LLS.selasa(y)) * (data.selasa[a] - LLS.selasa(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.rb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.rabu[a] - LLS.rabu(y)) * (data.rabu[a] - LLS.rabu(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.km <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.kamis[a] - LLS.kamis(y)) * (data.kamis[a] - LLS.kamis(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.jm <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.jumat[a] - LLS.jumat(y)) * (data.jumat[a] - LLS.jumat(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.sb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.sabtu[a] - LLS.sabtu(y)) * (data.sabtu[a] - LLS.sabtu(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errLLS.mg <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.minggu[a] - LLS.minggu(y)) * (data.minggu[a] - LLS.minggu(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}

# ------------------------------------------------------------------------------------- ERROR LLS ----------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------- ERROR PLS2 ---------------------------------------------------------------------------------------

errPLS2.sn <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.senin[a] - PLS2.sn(y)) * (data.senin[a] - PLS2.sn(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.sl <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.selasa[a] - PLS2.sl(y)) * (data.selasa[a] - PLS2.sl(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.rb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.rabu[a] - PLS2.rb(y)) * (data.rabu[a] - PLS2.rb(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.km <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.kamis[a] - PLS2.km(y)) * (data.kamis[a] - PLS2.km(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.jm <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.jumat[a] - PLS2.jm(y)) * (data.jumat[a] - PLS2.jm(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.sb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.sabtu[a] - PLS2.sb(y)) * (data.sabtu[a] - PLS2.sb(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS2.mg <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.minggu[a] - PLS2.mg(y)) * (data.minggu[a] - PLS2.mg(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}

# ------------------------------------------------------------------------------------- ERROR PLS2 ---------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------- ERROR PLS3 ---------------------------------------------------------------------------------------

errPLS3.sn <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.senin[a] - PLS3.sl(y)) * (data.senin[a] - PLS3.sn(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.sl <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.selasa[a] - PLS3.sl(y)) * (data.selasa[a] - PLS3.sl(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.rb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.rabu[a] - PLS3.rb(y)) * (data.rabu[a] - PLS3.rb(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.km <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.kamis[a] - PLS3.km(y)) * (data.kamis[a] - PLS3.km(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.jm <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.jumat[a] - PLS3.jm(y)) * (data.jumat[a] - PLS3.jm(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.sb <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.sabtu[a] - PLS3.sb(y)) * (data.sabtu[a] - PLS3.sb(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
errPLS3.mg <- function(){
  a <- 1
  y <- 8
  for(i in 1:29) {
    err[a] <- ((data.minggu[a] - PLS3.mg(y)) * (data.minggu[a] - PLS3.mg(y)))
    a <- a+1
    y <- y + 0.5
  }
  return(err)
}
# ------------------------------------------------------------------------------------- ERROR PLS3 ---------------------------------------------------------------------------------------


print(errorLLS <- c(sum(errLLS.sn()), sum(errLLS.sl()), sum(errLLS.rb()), sum(errLLS.km()), sum(errLLS.jm()), sum(errLLS.sb()), sum(errLLS.mg())))
print(errorPLS2 <- c(sum(errPLS2.sn()), sum(errPLS2.sl()), sum(errPLS2.rb()), sum(errPLS2.km()), sum(errPLS2.jm()), sum(errPLS2.sb()), sum(errPLS2.mg())))
print(errorPLS3 <- c(sum(errPLS3.sn()), sum(errPLS3.sl()), sum(errPLS3.rb()), sum(errPLS3.km()), sum(errPLS3.jm()), sum(errPLS3.sb()), sum(errPLS3.mg())))

rmse.lls.sn <- rmse(data.senin, tes.LLS.sn())
rmse.lls.sl <- rmse(data.selasa, tes.LLS.sl())
rmse.lls.rb <- rmse(data.rabu, tes.LLS.rb())
rmse.lls.km <- rmse(data.kamis, tes.LLS.km())
rmse.lls.jm <- rmse(data.jumat, tes.LLS.jm())
rmse.lls.sb <- rmse(data.sabtu, tes.LLS.sb())
rmse.lls.mg <- rmse(data.minggu, tes.LLS.mg())

rmse.pls2.sn <- rmse(data.senin, tes.PLS2.sn())
rmse.pls2.sl <- rmse(data.selasa, tes.PLS2.sl())
rmse.pls2.rb <- rmse(data.rabu, tes.PLS2.rb())
rmse.pls2.km <- rmse(data.kamis, tes.PLS2.km())
rmse.pls2.jm <- rmse(data.jumat, tes.PLS2.jm())
rmse.pls2.sb <- rmse(data.sabtu, tes.PLS2.sb())
rmse.pls2.mg <- rmse(data.minggu, tes.PLS2.mg())

rmse.pls3.sn <- rmse(data.senin, tes.PLS3.sn())
rmse.pls3.sl <- rmse(data.selasa, tes.PLS3.sl())
rmse.pls3.rb <- rmse(data.rabu, tes.PLS3.rb())
rmse.pls3.km <- rmse(data.kamis, tes.PLS3.km())
rmse.pls3.jm <- rmse(data.jumat, tes.PLS3.jm())
rmse.pls3.sb <- rmse(data.sabtu, tes.PLS3.sb())
rmse.pls3.mg <- rmse(data.minggu, tes.PLS3.mg())

rmse.sn <- c(rmse.lls.sn, rmse.pls2.sn, rmse.pls3.sn)
rmse.sl <- c(rmse.lls.sl, rmse.pls2.sl, rmse.pls3.sl)
rmse.rb <- c(rmse.lls.rb, rmse.pls2.rb, rmse.pls3.rb)
rmse.km <- c(rmse.lls.km, rmse.pls2.km, rmse.pls3.km)
rmse.jm <- c(rmse.lls.jm, rmse.pls2.jm, rmse.pls3.jm)
rmse.sb <- c(rmse.lls.sb, rmse.pls2.sb, rmse.pls3.sb)
rmse.mg <- c(rmse.lls.mg, rmse.pls2.mg, rmse.pls3.mg)

mean(rmse.lls.sn, rmse.lls.sl, rmse.lls.rb, rmse.lls.km, rmse.lls.jm, rmse.lls.sb, rmse.lls.mg)
mean(rmse.pls2.sn, rmse.pls2.sl, rmse.pls2.rb, rmse.pls2.km, rmse.pls2.jm, rmse.pls2.sb, rmse.pls2.mg)
mean(rmse.pls3.sn, rmse.pls3.sl, rmse.pls3.rb, rmse.pls3.km, rmse.pls3.jm, rmse.pls3.sb, rmse.pls3.mg)

hasil.lls <- data.frame(tes.LLS.sn(), tes.LLS.sl(), tes.LLS.rb(), tes.LLS.km(), tes.LLS.jm(), tes.LLS.sb(), tes.LLS.mg())
hasil.pls2 <- data.frame(tes.PLS2.sn(), tes.PLS2.sl(), tes.PLS2.rb(), tes.PLS2.km(), tes.PLS2.jm(), tes.PLS2.sb(), tes.PLS2.mg())
hasil.pls3 <- data.frame(tes.PLS3.sn(), tes.PLS3.sl(), tes.PLS3.rb(), tes.PLS3.km(), tes.PLS3.jm(), tes.PLS3.sb(), tes.PLS3.mg())

# --------------------------------------------------------------------------------END------------------------------------------------------------------------------------------
