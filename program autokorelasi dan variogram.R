long <- seq(-5,5, by=0.8)
lat <- seq(-5,5, by=0.8)
hasil = NULL
for (i in long){
  for (j in lat){
    y = (i**2+j**2) + rnorm(1,0,1)
    hasil = rbind(hasil,c(i, j, y))
  }
}

plot(hasil[,1], hasil[,2])

z <- matrix(hasil[,3], length(long), length(lat), byrow=TRUE)

persp(long, lat, z, phi=45, theta=-10)
contour(long, lat, z)

pasangan = NULL
for (i in 1:(nrow(hasil)-1)){
  for (j in (i+1):nrow(hasil)){
    jarak = sqrt((hasil[i,1]-hasil[j,1])^2 
                 + (hasil[i,2]-hasil[j,2])^2)
    pasangan = rbind(pasangan, c(i, j, jarak, hasil[i,3],hasil[j,3]))
  }
}

pasangan.h1 <- pasangan[which(pasangan[,3]<=1),]
cor(pasangan.h1[,4:5])
pasangan.h2 <- pasangan[which(pasangan[,3]<=2 & pasangan[,3]>1),]
cor(pasangan.h2[,4:5])


pasangan.h1 <- pasangan[which(pasangan[,3]>1 & pasangan[,3]<2),]
cor(pasangan.h1[,4:5])
pasangan.h3 <- pasangan[which(pasangan[,3]==3),]
cor(pasangan.h3[,4:5])

korelasi <- NULL
for (i in 1:20/2){
  pasangan.h <- pasangan[which(pasangan[,3]>=(i-0.5) & pasangan[,3]<i),]
  korelasi <- rbind(korelasi, cor(pasangan.h[,4:5])[2])
}
plot(1:20/2, korelasi, type='b', xlab="jarak")

xbar <- mean(hasil[,3])
a <- sum((pasangan[,4]-xbar)*(pasangan[,5]-xbar)/pasangan[,3])
b <- sum(1/pasangan[,3])
c <- nrow(hasil)
d <- sum((hasil[,3]-xbar)**2)
moran <- (c*a)/(b*d)
moran

library(ape)
jarak <- as.matrix(dist(cbind(hasil[,1], hasil[,2])))
inv.jarak <- 1/jarak
diag(inv.jarak) <- 0
Moran.I(hasil[,3], inv.jarak)


nilai.moran <- NULL
for (ulangan in 1:100){
  acak <- sample(1:nrow(hasil), 30)
  sampel <- hasil[acak,]
  jarak <- as.matrix(dist(cbind(sampel[,1], sampel[,2])))
  inv.jarak <- 1/jarak
  diag(inv.jarak) <- 0
  indeks.moran <- Moran.I(sampel[,3], inv.jarak)[[1]]
  nilai.moran[ulangan] <- indeks.moran
}
boxplot(nilai.moran)


nilai.moran <- NULL
for (ukuran in c(20, 30, 40, 50, 60)){
  for (ulangan in 1:100){
    acak <- sample(1:nrow(hasil), ukuran)
    sampel <- hasil[acak,]
    jarak <- as.matrix(dist(cbind(sampel[,1], sampel[,2])))
    inv.jarak <- 1/jarak
    diag(inv.jarak) <- 0
    indeks.moran <- Moran.I(sampel[,3], inv.jarak)[[1]]
    nilai.moran <- rbind(nilai.moran, cbind(ukuran, indeks.moran))
  }
}
moran <- data.frame(nilai.moran)
boxplot(indeks.moran ~ ukuran, data=moran, 
        xlab="ukuran contoh", ylab='indeks moran')

variogram <- NULL
for (i in 1:20/2){
  pasangan.h <- pasangan[which(pasangan[,3]>=(i-0.5) & pasangan[,3]<i),]
  vvv <- sum((pasangan.h[,4]-pasangan.h[,5])**2)/(2*nrow(pasangan.h))
  variogram <- rbind(variogram, vvv)
}
plot(1:20/2, variogram, type='b', xlab="jarak")

library(geoR)
data.x <- data.frame(hasil)
data.x2 <- as.geodata(data.x, coord.col=1:2, data.col=3)
variog <- variog(data.x2,max.dist=10)
plot(variog, type='b')
variog.ols.exp <- variofit(variog, cov.model="exponential",wei="equal")
plot(variog)
lines(variog.ols.exp)
variog.ols.sph <- variofit(variog, cov.model="spherical",wei="equal")
lines(variog.ols.sph, col="red")
