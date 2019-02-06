   #===============================
   # Principal Components - simple 2-D example
   #===============================
library(stats)
A <- cbind(1:5,1:5)
(A.pc <- prcomp(A))

summary(A.pc)

(A.new <- A%*%A.pc$rotation[,1])
   
A.new%*%A.pc$rotation[,1]


   # Principal Components - 2-D example
numb <- 500            # data set with 500 points
a <- 10                # semi-major axis
b <- 5                 # semi-minor axis
x <- runif(numb,-a,a)  # x is random uniform in [-a, a] i.e. [-10,10]
y <- matrix(0,1,numb)
for (i in (1:numb)) {
   aa <- b*(1 - (x[i]/a)^2)^(1/2)
   y[i] <- runif(1, -aa, aa)    # a random number in [-aa, aa]
}

y <- as.vector(y)
plot(x, y, pch = 20, main = "Ellipse? - default scaling", cex = 1.5)
bringToTop(which = dev.cur())

plot(x,y, pch = 20, asp = 1, main = "Ellipse with correct scaling", cex = 1.5)
bringToTop(which = dev.cur())

   #=======================
drive <- "I:"
code.dir <- paste(drive, "DataMining/Code", sep = "/")
data.dir <- paste(drive, "DataMining/Data", sep = "/")
   #
(R <- cbind(c(cos(pi/3), sin(pi/3)),c(sin(pi/3), -cos(pi/3))))
Z <- cbind(x,y)
XX <- Z%*%R
# write.table(XX, row.names = F, col.names = F, quote = F, file = paste(data.dir, "pc_xx.dat", sep = ""))
XX <- read.table(paste(data.dir, "pc_xx.dat", sep = "/"))
Z <- as.matrix(XX)%*%R
plot(Z, pch = 20, asp = 1, main = "Rotated ellipse (red) with original")
points(XX, pch = 20, col = "red")
bringToTop(which = dev.cur())

   # Find the principal components from the SVD of the covariance matrix
   # The Singular Value Decomposition is
(pc.1 <- svd(cov(XX)))
   # The standard deviations are
sqrt(pc.1$d)

# Find the principal components from the eigenvectors of the covariance matrix
(pc.2 <- eigen(cov(XX)))

   # or from a routine (gives other info)
(pc.3 <- prcomp(XX))

plot(pc.3, main = "Scree plot for ellipse", col = c("red","yellow"))
bringToTop(which = dev.cur())

   #
summary(pc.3)

   # Display with principal axes
plot(XX, pch = 20, asp = 1, main = "Ellipse with principal axes", cex = 1.5)
A <- diag(c(1, 1))
PCaxes <- A%*%pc.3$rotation
abline(0,pc.3$rotation[2,1]/pc.3$rotation[1,1], col = "red")
abline(0,pc.3$rotation[2,2]/pc.3$rotation[1,2], col = "blue")
bringToTop(which = dev.cur())

m.1 <- pc.3$rotation[2,1]/pc.3$rotation[1,1]
m.2 <- pc.3$rotation[2,2]/pc.3$rotation[1,2]

for (i in sample(1:500,10)) {
   x.1 <- XX[i,1]
   y.1 <- XX[i,2]
   points(x.1, y.1, pch = 15, col = "blue")
   x.2 <- (y.1 - m.2*x.1)/(m.1 - m.2)
   y.2 <- m.1*x.2
   lines(c(x.1,x.2), c(y.1,y.2), col = "blue")
}
for (i in sample(1:500,5)) {
   x.1 <- XX[i,1]
   y.1 <- XX[i,2]
   points(x.1, y.1, pch=15, col = "red")
   x.2 <- (y.1 - m.1*x.1)/(m.2 - m.1)
   y.2 <- m.2*x.2
   lines(c(x.1,x.2), c(y.1,y.2), col = "red")
}
bringToTop(which = dev.cur())

   #
   # Rotate so the principal axes are horiz/vert
XXR <- as.matrix(XX)%*%PCaxes
plot(XX, pch = 20, asp = 1, main = "Ellipse rotated by PCA rotation", cex = 1.5)
points(XXR, col = "red", pch = "+", cex = 1.5)
bringToTop(which = dev.cur())

   #====== Compare rotation =================
   # Compare original rotation with PC rotation
   # The ellipse was rotated with the matrix R
   # The Principal Components rotation is PCaxes
R

PCaxes

   #=========== Flea Beetles =================

source(paste(code.dir, "ReadFleas.r", sep = "/"))

   #========= Flea Beetles
(pc.flea <- prcomp(d.flea))
plot(pc.flea, col = heat.colors(6))
bringToTop(which = dev.cur())

 

   # Handwritten digits
d.file <- {}
d.digits <- c({}, {}, {})
for (i in 0:9) {
   d.file[i+1] <- paste(data.dir, "/train_", i, ".dat", sep = "")
   d.digits[[i+1]] <- matrix(scan(d.file[i+1], sep = ","), ncol = 256, byrow = T)
}

(num.cases <- unlist(lapply(d.digits, dim))[seq(1,20,2)])

plot.digits <- function(digits) {
  x11(width = 6, height = 6)    # Open a graphics window of given size  
      # Create a plot matrix with 144 subplots - plot in row-wise order
  layout(matrix(1:144, 12, 12, byrow = TRUE))
      # No margin (see ?par)
  oldpar <- par(mar = c(0, 0, 0, 0))
  for (i in 1:144) {
         # xaxt = "n", yaxt = "n" - no axes
    image(matrix(digits[i,],16,16)[,16:1], xaxt = "n", yaxt = "n",
          col = gray((255:0)/255))
  }
  par(oldpar)
}

plot.digits(d.digits[[2+1]])
plot.digits(d.digits[[3+1]])
plot.digits(d.digits[[5+1]])
plot.digits(d.digits[[8+1]])


#=====
library(stats)
graphics.off()
pc.digits <- {}
for (i in 0:9) {
  pc.digits[[i+1]] <- prcomp(d.digits[[i+1]])
  plot(pc.digits[[i+1]], col = heat.colors(10), main = i)
  print(summary(pc.digits[[i+1]]))
  readline("Press return..")
}

#
pc.digits[[3+1]]


   #
   # For each number create the first 4 principal components
pc <- array(dim = c(4 , 256, 10), dimnames = list(c(1:4),1:256,c(0:9)))
for (i in 0:9) {
   pc[1,,i+1] <- pc.digits[[i+1]]$rotation[,1]
   pc[2,,i+1] <- pc.digits[[i+1]]$rotation[,2]
   pc[3,,i+1] <- pc.digits[[i+1]]$rotation[,3]
   pc[4,,i+1] <- pc.digits[[i+1]]$rotation[,4]
}

   #

graphics.off()
x11(width = 4, height = 2.4)
layout(matrix(1:50, 5, 10, byrow = TRUE))
oldpar <- par(mar = c(0, 0, 0, 0))
for (i in 0:9) {
   mean <- apply(d.digits[[i+1]], 2, mean)
   image(matrix(mean,16,16)[,16:1], xaxt = "n", yaxt = "n",
               col = gray((255:0)/255))
   image(matrix(pc[1,,i+1],16,16)[,16:1], xaxt = "n", yaxt = "n",
               col = gray((255:0)/255))
   image(matrix(pc[2,,i+1],16,16)[,16:1], xaxt = "n", yaxt = "n",
               col = gray((255:0)/255))
   image(matrix(pc[3,,i+1],16,16)[,16:1], xaxt = "n", yaxt = "n",
               col = gray((255:0)/255))
   image(matrix(pc[4,,i+1],16,16)[,16:1], xaxt = "n", yaxt = "n",
               col = gray((255:0)/255))
}
par(oldpar)

   #

display.mean.pc <- function(pc, digits) {
   mean <- apply(digits, 2, mean)
   for (i in 1:15) {
      image(matrix(mean+(i-8)*pc, 16,16)[,16:1], xaxt = "n", yaxt = "n", col = gray((255:0)/255))
   }
}

display.pcs <- function (pcnum) {
   x11(width = 7, height = 5)
   oldpar <- par(mar = c(1, 0, 0, 0))
   layout(matrix(1:150, 10, 15, byrow = TRUE))
   for (i in 0:9) {
      display.mean.pc(pc[pcnum,,i+1], d.digits[[i+1]])
   }
   bringToTop(which = dev.cur())
   par(oldpar)
}

display.pcs(1)
display.pcs(2)
display.pcs(3)
display.pcs(4)

   # Reconstitution
d.digits.pc <- {}
for (i in 0:9) {
   d.digits.pc[[i+1]] <- d.digits[[i+1]]%*%pc.digits[[i+1]]$rotation
}

   # As before
plot.digits(d.digits[[3+1]])

   #
recreate <- function(pc.range, digit) {
   tmp <- matrix(0, num.cases[digit+1], 256)
   tmp <- d.digits.pc[[digit+1]][,pc.range]%*%t(pc.digits[[digit+1]]$rotation[,pc.range])
   tmp <- tmp/max(abs(range(tmp)))   # We want to scale the data to lie in [-1, 1]
   tmp
}


display.recreate <- function(digit) {
   plot.digits(d.digits[[digit+1]])
   pc.1.20 <- recreate(1:20, digit)
   plot.digits(pc.1.20)
   pc.1.100 <- recreate(1:100, digit)
   plot.digits(pc.1.100)
   pc.21.100 <- recreate(21:100, digit)
   plot.digits(pc.21.100)
   pc.101.256 <- recreate(101:256, digit)
   plot.digits(pc.101.256)
}

display.recreate(1)
display.recreate(3)
display.recreate(8)



   # MDS

#======= cmdscale

require(stats)
require(MASS)

(test <- matrix(c(1,1,0, 1,-1,0, -1,1,0, -1,-1,0, 0,0,1), ncol = 3, byrow = T))
(test.dist <- dist(test))

(test.mds <- cmdscale(test.dist))
dist(test.mds)

plot(test[,-3], xlim = c(-1.5,1.5), ylim = c(-1.5,1.5), xlab = "", ylab = "",
     type = "n", main = "Classical vs Projection - apex at 1")
text(test[,1],test[,2],1:5, col = "red")
text(test.mds[,1],test.mds[,2],1:5, col = "blue")

tmp <- matrix(0, 5, 5)
row(tmp) > col(tmp)           # to show what 'row(tmp) > col(tmp)' does

tmp[row(tmp) > col(tmp)] <- test.dist^2
tmp

(S <- tmp + t(tmp))

(grand.mean <- mean(S))
(col.mean <- apply(S, 2, mean))
(row.mean <- apply(S, 1, mean))

(S <- t(t(S) - col.mean))
(S <- S - row.mean)
(S <- S + grand.mean)

apply(S,1,mean)
apply(S,2,mean)

k <- 2                      # Dimension of the target space
(eig <- eigen(-S/2, symmetric = T))
eig$values
(E <- eig$vectors[,1:k])
(D <- diag(sqrt(eig$values[1:k])))

(cmds <- E%*%D)
dist(cmds)

plot(test[,-3], xlim = c(-1.5,1.5), ylim = c(-1.5,1.5), xlab = "", ylab = "",
     type = "n", main = "cmdscale vs eigen - apex at 1")
text(cmds[,1],cmds[,2],1:5, col = "red")
text(test.mds[,1],test.mds[,2],1:5, col = "blue")

   # Redo

source(paste(code.dir, "ClassicMDS.r", sep = "/"))

Classic.MDS(2.236)

Classic.MDS(2.237)

   #============================
test <- matrix(c(1,1,0, 1,-1,0, -1,1,0, -1,-1,0, 0,0,2.237), ncol = 3, byrow = T)
test.dist <- dist(test)
(test.mds <- sammon(test.dist))

plot(test[,-3], xlim = c(-1.5,1.8), ylim = c(-1.5,1.5), xlab = "", ylab = "",
     type = "n", main = "Projected vs Sammon - apex at 2.237")
text(test[,1],test[,2],1:5, col = "blue")
text(test.mds$points[,1],test.mds$points[,2],1:5, col = "red")
li <- c(1,2,5,4,3,5,1,3,4,2)
lines(test[li,1],test[li,2], col = "blue", lwd = 2)
lines(test.mds$points[li,1],test.mds$points[li,2],col = "red", lwd = 2)

(dist.sam <- dist(test.mds$points))

(test.mds <- cmdscale(test.dist))

(total <- sum(test.dist))
(diff <- test.dist-dist(cmdscale(test.dist)))
(err <- sum((diff^2)/test.dist))
err/total

(diffs <- test.dist-dist.sam)
(errs <- sum((diffs^2)/test.dist))
errs/total               # stress


   #
install.packages("pixmap")
(data(eurodist))
eurodist

library(pixmap)   
require(MASS)
d.file <- paste(data.dir, "Europe.pgm", sep = "/")

#
image <- read.pnm(d.file)

plot(image, main = "Classical MDS")

loc.cmd <- cmdscale(eurodist)
x <- (loc.cmd[,1]-min(loc.cmd[,1]))*.14 + 50
y <- -(loc.cmd[,2]-max(loc.cmd[,2]))*.12 + 80
text(x, y, labels(eurodist), cex = 1, col = "red")

plot(image, main = "Sammon")


   #
loc <- sammon(eurodist)
locPt <- loc$points
x <- (loc$points[,1]-min(loc$points[,1]))*.14 + 50
y <- -(loc$points[,2]-max(loc$points[,2]))*.12 + 80
text(x, y, labels(eurodist), cex = 1, col = "red")

plot(image, main = "isoMDS")

   #
loc <- isoMDS(eurodist)
x <-  (locPt[,1]-min(locPt[,1]))*.14 + 50
y <- -(locPt[,2]-max(locPt[,2]))*.15 + 80
text(x, y, labels(eurodist), cex = 1, col = "red")


source(paste(code.dir, "3Drotations.r", sep = "/"))
plot(image, main = "isoMDS with rotation")

   #
loc <- isoMDS(eurodist)
locPt <- R.2D(loc$points,-.25)
x <-  (locPt[,1]-min(locPt[,1]))*.14 + 50
y <- -(locPt[,2]-max(locPt[,2]))*.15 + 80
text(x, y, labels(eurodist), cex = 1, col = "red")



   # Taste test  http://www.york.ac.uk/depts/maths/teaching/pml/mva/md/data.3

comp <- c("Diet Pepsi","RC Cola", "Yukon","Dr. Pepper","Shasta", "Coca-Cola",
          "Diet Dr. Pepper","Tab","Pepsi-Cola","Diet-Rite")
p <- length(comp)
dat <- scan(paste(data.dir,"softdrinks.dat",sep = "/"))

oldpar <- par(mfrow = c(2,3), mar = c(1,0,1,0), xaxt = "n", yaxt = "n")
k <- 1
   # Repeat 9 times to get 9 students
for (kk in 1:9) {
   Dis <- matrix(0, p, p)
   Dis[col(Dis) >= row(Dis)] <- dat[k:(k+54)]
   Dis <- t(Dis)+Dis
   diag(Dis) <- diag(Dis/2)
   # Dis <- 100-Dis
   k <- k + 55
   dimnames(Dis) <- list(comp, comp)
   coords <- cmdscale(Dis)
   coord1 <- -coords[,1]
   coord2 <- -coords[,2]
   plot(coord2,coord1, xlab = "",main = "cmdscale")
   text(coord2,coord1, comp)
   coords <- isoMDS(Dis)
   coord1 <- -coords$points[,1]
   coord2 <- -coords$points[,2]
   plot(coord2,coord1, xlab = "", main = paste("isoMDS ", kk))
   text(coord2,coord1, comp)
   coords <- sammon(Dis)
   coord1 <- -coords$points[,1]
   coord2 <- -coords$points[,2]
   plot(coord2,coord1, xlab = "", main = "sammon")
   text(coord2,coord1, comp)
   readline("Press any key")
}
par(oldpar)


source(paste(code.dir, "ReadFleas.r", sep = "/"))
(flea.dist <- dist(d.flea))

   #
flea.mds <- isoMDS(flea.dist)
plot(flea.mds$points, type = "n",  main = "isoMDS for Flea Beetles")
text(flea.mds$points, labels = as.character(1:nrow(d.flea)), col = species+1)
bringToTop(which = dev.cur())

   #
flea.sam <- sammon(dist(flea.dist))
plot(flea.sam$points, type = "n",  main = "Sammon for Flea Beetles")
text(flea.sam$points, labels = as.character(1:nrow(d.flea)), col = species+1)
bringToTop(which = dev.cur())

plot(-d.flea[,c(1,6)], col = species+1, pch = 19)

