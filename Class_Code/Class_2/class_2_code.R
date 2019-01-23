n.baskets <- 2733
numb <- 5                            # Number of items in baskets
# Simulation of baskets
set.seed(12345, "default")           # Try to make replication possible
# Create a matrix to represent the basket (holds 5 items) 
baskets <- matrix(0, n.baskets, numb)
heading <- c("Milk", "Peanut.Butter", "Bread", "Cereal", "Jelly")
dimnames(baskets) <- list (NULL, heading)
# Put "Peanut.Butter", "Bread","Jelly" in baskets 1-82
baskets[1:82, c("Peanut.Butter", "Bread","Jelly")] <- 1
# and "Peanut.Butter","Jelly" in 83 to 100
baskets[83:100,c("Peanut.Butter","Jelly")] <- 1
# (P)eanut Bu??arulestter & (J)elly can only appear together
#        in the first 100 baskets
# n.@, b.@, e.@ represent the number of the item @,
#    the starting and ending baskets
# sample(100,n.@) produces n.@ random integers from 1-100
# Scatter some (M)ilk and (C)ereal in the first 100 baskets
n.M <- 60

n.C <- 780; b.C <- 600; e.C <- 1600
baskets[sample(e.C - b.C, n.C) + b.C, c("Cereal")] <- 1
n.B <- 1200; b.B <- 100;  e.B <- 2733
baskets[sample(e.B - b.B, n.B) + b.B, c("Bread")] <- 1
# End of simulation
# write.table(baskets, quote=F, file="I:/DataMining/Data/Baskets.dat")
# 
baskets <- read.table("I:/DataMining/Data/Baskets.dat")
baskets[sample(100, n.M), c("Milk")] <- 1
n.C <- 55
baskets[sample(100, n.C), c("Cereal")] <- 1
# sample(e.@ - b.@, n.@) + b.@
# e.@ - b.@ gives the range [1, e.@ - b.@] from which to choose n.J
# adding b.@ makes the ramge [b.@+1, e.@ - b.@ + b.@] 
n.J <- 800; b.J <- 100;  e.J <- 1503
baskets[sample(e.J - b.J, n.J) + b.J, c("Jelly")] <- 1

n.P <- 700; b.P <- 1514;  e.P <- 2733
baskets[sample(e.P - b.P, n.P) + b.P, c("Peanut.Butter")] <- 1
# Now put more Milk, Cereal, and Bread in some baskets
n.M <- 500; b.M <- 100;  e.M <- 1103
baskets[sample(e.M - b.M, n.M) + b.M, c("Milk")] <- 1

n.M <- 450; b.M <- 200;  e.M <- 2733
baskets[sample(e.M - b.M, n.M) + b.M, c("Milk")] <- 1


(item.in.basket <- apply (baskets, 2, sum))
(percent.in.basket <- round(item.in.basket/n.baskets*100, 2))

# All unique pairs
n.comb.2 <- choose(numb,2)
double <- matrix(0, n.comb.2, 2)
ind <- 1
for (i in 1:numb) {
  j <- i+1
  while (j <= numb) {  # for can run 6:5 ...
    double[ind,] <- c(i,j)
    ind <- ind + 1
    j <- j + 1
  }
}

# double
matrix(heading[double], n.comb.2, 2)

double.counts <- matrix(0, n.comb.2, 3)
for (i in 1:dim(double)[1]){
  double.counts[i,] <-c(double[i,],sum(floor(apply(baskets[,double[i,]],1,sum)/2)))
}
matrix(c(heading[double.counts[,1:2]], double.counts[,3]), n.comb.2, 3)

# All unique triples
n.comb.3 <- choose(numb,3)
triple <- matrix(0, n.comb.3, 3)
ind <- 1
for (i in 1:numb) {
  j <- i+1
  while (j <= numb) {  # for can run 6:5 ...
    k <- j + 1
    while (k <= numb) { 
      triple[ind, ] <- c(i,j,k)
      ind <- ind + 1
      k <- k + 1
    }
    j <- j + 1
  }
}
# triple
matrix(heading[triple], n.comb.3, 3)

triple.counts <- matrix(0, n.comb.3, 4)
for (i in 1:dim(triple)[1]){
  triple.counts[i,] <-c(triple[i,],sum(floor(apply(baskets[,triple[i,]],1,sum)/3)))
}
matrix(c(heading[triple.counts[,1:3]],triple.counts[,4]), n.comb.3, 4)


# quads
matrix(heading[quad], n.comb.4, 5)

quad.counts <- matrix(0, n.comb.4, 5)
for (i in 1:dim(quad)[1]){
  qu
  # All unique triples
  n.comb.3 <- choose(numb,3)
  triple <- matrix(0, n.comb.3, 3)
  ind <- 1
  for (i in 1:numb) {
    j <- i+1
    while (j <= numb) {  # for can run 6:5 ...
      k <- j + 1
      while (k <= numb) { 
        triple[ind, ] <- c(i,j,k)
        ind <- ind + 1
        k <- k + 1
      }
      j <- j + 1
    }
  }
  # triple
  matrix(heading[triple], n.comb.3, 3)
  
  triple.counts <- matrix(0, n.comb.3, 4)
  for (i in 1:dim(triple)[1]){
    triple.counts[i,] <-c(triple[i,],sum(floor(apply(baskets[,triple[i,]],1,sum)/3)))
  }
  matrix(c(heading[triple.counts[,1:3]],triple.counts[,4]), n.comb.3, 4)
  
  # All unique quads
  n.comb.4 <- choose(numb,4)
  quad <- matrix(0, n.comb.4, 4)
  ind <- 1
  for (i in 1:numb) {
    j <- i+1
    while (j <= numb) {  # for can run 6:5 ...
      k <- j + 1
      while (k <= numb) {
        l <- k + 1
        while (l <= numb) { 
          quad[ind, ] <- c(i,j,k,l)
          ind <- ind + 1
          l <- l + 1
        }
        k <- k + 1
      }
      j <- j + 1
    }
  }
  quad.counts[i,] <-c(quad[i,],sum(floor(apply(baskets[,quad[i,]],1,sum)/4)))
}

matrix(c(heading[quad.counts[,1:4]],quad.counts[,5]), n.comb.4, 5)

# Look for influences
for (i in 1:length(item.in.basket)){    # Run through all the singles
  for (j in 1:nrow(triple)){      # For each single, look at each double
    one.in <- i == double[j,]
    if (any(one.in)) {               # Test to see if single in current double
      s.c <- item.in.basket[i]     # It is, so we get the appropriate counts.
      d.c <- double.counts[j, 3]
      item <- double[j, which(!one.in)]
      other <- heading[i]
      cat("When ", other,
          " is purchased (", s.c, "/", n.baskets, " = ",
          round(100*s.c/n.baskets, 2),"%) Support(", other, ")\n   ",
          heading[item], " was purchased (", d.c, "/", s.c,
          " = ", round(100*d.c/s.c, 2),"%)  Confidence\n", sep = "")
      cat("   Overall ", heading[item], " purchase rate is ", percent.in.basket[item],
          "%)  Support(", heading[item], "),\n", sep = "")
      cat("   Lift ", round(10000*d.c/s.c, 0)/percent.in.basket[item], "%\n", sep = "")
    }
  }
}

for (i in 1:nrow(double)){    # Run through all the doubles
  for (j in 1:nrow(triple)){ # For each double, look at each triple
    one.in <- double[i,1] == triple[j,]
    two.in <- double[i,2] == triple[j,]
    if (sum(two.in*1 + one.in*1) == 2) { # Test to see if double in current triple
      d.c <- double.counts[i, 3]     # It is, so we get the appropriate counts.
      t.c <- triple.counts[j, 4]
      item <- triple[j, which(!(two.in|one.in))]
      others <- paste(heading[double[i,1]],heading[double[i,2]], sep = "/")
      cat("When ", others,
          " are purchased (", d.c, "/", n.baskets, " = ",
          round(100*d.c/n.baskets, 2),"%) Support(", others, ")\n   ",
          heading[item], " was purchased (", t.c,"/",d.c,
          " = ", round(100*t.c/d.c, 2),"%)  Confidence\n", sep = "")
      cat("   Overall ", heading[item], " purchase rate is ", percent.in.basket[item],
          "%)  Support(", heading[item], "),\n", sep = "")
      cat("   Lift ", round(10000*t.c/d.c, 0)/percent.in.basket[item], "%\n", sep = "")
    }
  }
}

for (i in 1:nrow(triple)){    # Run through all the triples
  for (j in 1:nrow(quad)){ # For each triple, look at each quad
    one.in <- triple[i, 1] == quad[j,]
    two.in <- triple[i, 2] == quad[j,]
    three.in <- triple[i, 3] == quad[j,]
    if (sum(two.in*1 + one.in*1 + three.in*1) == 3) { # Test to see if triple in current quad
      t.c <- triple.counts[i, 4]     # It is, so we get the appropriate counts.
      q.c <- quad.counts[j, 5]
      item <- quad[j, which(!(three.in|two.in|one.in))]
      others <- paste(heading[triple[i,]], collapse = "/")
      cat("When ", others,
          " are purchased (", t.c, "/", n.baskets, " = ",
          round(100*t.c/n.baskets, 2),"%) Support(", others, ")\n   ",
          heading[item], " was purchased (", q.c,"/", t.c,
          " = ", round(100*q.c/t.c, 2),"%)  Confidence\n", sep = "")
      cat("   Overall ", heading[item], " purchase rate is ", percent.in.basket[item],
          "%)  Support(", heading[item], "),\n", sep = "")
      cat("   Lift ", round(10000*q.c/t.c, 0)/percent.in.basket[item], "%\n", sep = "")
    }
  }
}
install.packages("arules")

library(arules)
rules <- apriori(apply(baskets, 2, as.numeric), parameter = list(supp = 0.01, conf = 0.01, target = "rules"))

summary(rules)
inspect(rules)



# For use by apriori
Heading <- c("Milk", "Peanut.Butter", "Bread", "Cereal", "Jelly")
write(Heading[which(baskets[1,]==1)],
      ncolumns = 5,
      file = "I:/DataMining/Data/MB.tab",                     
      append = FALSE)
for(i in 2:n.baskets) {
  if (sum(baskets[i,]==1)>0){
    write(Heading[which(baskets[i,]==1)],
          ncolumns = 5,
          file = "I:/DaaMining/Data/MB.tab",
          append = TRUE)
  } else {  # Put meat in any empty baskets
    write("Meat",
          ncolumns = 5,
          file = "I:/DataMining/Data/MB.tab",
          append = TRUE)
  }
}