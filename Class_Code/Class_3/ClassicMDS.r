Classic.MDS <- function(apex) {
  test <- matrix(c(1,1,0, 1,-1,0, -1,1,0, -1,-1,0, 0,0,apex),ncol=3,byrow=T)
  print("test");   print(test)
  test.dist <- dist(test)
  print("test.dist");   print(test.dist)

  test.mds <- cmdscale(test.dist)
  print("test.mds");   print(test.mds)
  dist(test.mds)
  print("dist(test.mds)");   print(dist(test.mds))

  plot(test[,-3], xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), xlab="", ylab="",
       type="n", main="Classical vs Projection - apex at 1")
  text(test[,1],test[,2],1:5, col = "red")
  text(test.mds[,1],test.mds[,2],1:5, col = "blue")

  tmp <- matrix(0, 5, 5)
  print("row(tmp) > col(tmp)")
  print(row(tmp) > col(tmp))         # to show what 'row(tmp) > col(tmp)' does

  tmp[row(tmp) > col(tmp)] <- test.dist^2
  print("tmp");   print(tmp)

  tmp <- tmp + t(tmp)
  print("tmp");   print(tmp)

  (grand.mean <- mean(tmp))
  print("grand.mean");   print(grand.mean)
  (col.mean <- apply(tmp, 2, mean))
  print("col.mean");   print(col.mean)
  row.mean <- apply(tmp, 1, mean)
  print("row.mean");   print(row.mean)

  S <- tmp - row.mean
  print("row mean removed");   print(S)
  S <- t(t(S) - col.mean)
  print("col mean removed");   print(S)
  S <- S + grand.mean
  print("grand mean added");   print(S)
  
  print("new row mean");   print(apply(S,1,mean))
  
  print("new col mean");   print(apply(S,2,mean))

  k <- 2                      # Dimension of the target space
  eig <- eigen(-S/2, symmetric=T)
  print("eig");   print(eig)
  E <- eig$vectors[,1:k]
  print("E");   print(E)
  D <- diag(sqrt(eig$values[1:k]))
  print("D");   print(D)

  cmds <- E%*%D
  print("cmds");   print(cmds)
  dist(cmds)
  print("dist(cmds)");   print(dist(cmds))

  plot(test[,-3], xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), xlab="", ylab="",
       type="n", main=paste("cmdscale vs eigen - apex at", apex))
  text(cmds[,1],cmds[,2],1:5, col = "red")
  text(test.mds[,1],test.mds[,2],1:5, col = "blue")
}

cat ("Usage: Classic.MDS(apex)\n")
