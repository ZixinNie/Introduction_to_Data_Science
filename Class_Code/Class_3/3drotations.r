# The following are some graphics routines to rotate and translate 3D objects

# Tensor product: outer(v,w)
# crossprod(v,w)

# Translation
translate <- function(x, w) {
    # move 3d data x to new centre (w1,w2,w3)
   I <- diag(rep(1,3))
   T <- rbind(cbind(I,c(0,0,0)),c(w,1))
   (cbind(x,1)%*%T)[,1:3]
}

# Rotation
R.x <- function(data, Ang) {
   T <- diag(rep(1,4))
   c <- cos(Ang); s <- sin(Ang)
   T[2,2] <- c;  T[3,3] <- c
   T[2,3] <- -s; T[3,2] <- s
   (cbind(data,1)%*%T)[,1:3]
}

R.y <- function(data, Ang) {
   T <- diag(rep(1,4))
   c <- cos(Ang); s <- sin(Ang)
   T[1,1] <- c;  T[3,3] <- c
   T[3,1] <- s; T[1,3] <- -s
   (cbind(data,1)%*%T)[,1:3]
}

R.z <- function(data, Ang) {
   T <- diag(rep(1,4))
   c <- cos(Ang); s <- sin(Ang)
   T[1,1] <- c;  T[2,2] <- c
   T[2,1] <- -s; T[1,2] <- s
   (cbind(data,1)%*%T)[,1:3]
}

Rot.translate <- function(data, xr, yr, zr, xt=0, yt=0, zt=0) {
   rot <- R.x(data, xr)
   rot <- R.y(rot, yr)
   rot <- R.z(rot, zr)
   translate(rot, c(xt, yt, zt))
}

#===============================
# 2D rotation
#===============================
R.2D <- function(data, Ang) {
    T <- diag(rep(1, 2))    # 2 x 2 identity matrix
    c <- cos(Ang)           
    s <- sin(Ang)
    T[1, 1] <- c            #  
    T[2, 2] <- c            #  cos     sin
    T[2, 1] <- -s           # -sin     cos
    T[1, 2] <- s            #
    data %*% T
}
