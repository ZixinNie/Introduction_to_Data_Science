# Simple examples of R code
a <- 5
a
(a <- 5)
(b <- c(1, 3, 2, 6, 5, 3, 2))
(c <- rep(5, 7))
(c.1 <- rep(c(1, 3, 2), 4))
(d <- 1:6)
(e <- 6:1)
(f <- 1:10/10)           
(g <- seq(10, 9, -.2))
a - b
a*b
a%*%b
a%*%t(b)
sum(a%*%t(b))
(m.1 <- matrix(0, 3, 2))
(m.2 <- matrix(1:12, nrow=3))
(I.3 <- diag(1,3))
(h <- matrix(b, nrow=1))
h*h
h%*%t(h)
(h.m <- t(h)%*%h)
h.m[5, 2]
h.m[1:3, 5:4]
h.m[c(3,5,1), c(6,2,7)]
(h.m[c(3,5,1), c(6,2,7)] <- -10)
sum(h.m)
mean(h.m)
apply(h.m, 1, sum)
apply(h.m, 1, mean)
apply(h.m, 2, sum)
apply(h.m, 2, mean)
matrix(letters[1:6], ncol=3)
(A <- matrix(c(3, 2, 5, 4, 1, 9, -1, 6, 8), 3, 3))
(bT <- c(-1, 3, 2))
cbind(A, bT)
(Mixed <- matrix(c("Height", "Width", 25, 30), 2, 2))
Mixed[1,2]*Mixed[2,2]
as.numeric(Mixed[1,2])*as.numeric(Mixed[2,2])
(L.1 <- list(first.name="John", last.name="Smith", sn=345678, mark="A-"))
L.1[2]
L.1[[2]]
L.1$last.name
L.2 <- {}
L.2 <- c(L.2, list(1))
L.2 <- c(L.2, list("x"))
L.2 <- c(L.2, list("x^2/2!"))
(L.2 <- c(L.2, list("x^3/3!")))
unlist(L.2)

paste("John", "Smith", 345678)
(str.1 <- paste("John", "Smith", 345678, sep=","))
(str.2 <- paste("Jane", "Jones", 234567, sep=","))
(str.3 <- paste("D:","DATA","Data Mining R-code",sep="/"))
(str.4 <- paste(unlist(L.2), collapse=" + "))

strsplit(str.1, ",")
strsplit(rbind(str.1, str.2), ",")
strsplit(str.4, " + ")
strsplit(str.4, " \\+ ")

a <- 1
b <- 2
if (a < b) print("a < b")

if (a == b)
   print("a < b")
else
   print("a >= b")

if (a == b) {
   print("a < b")
} else {
   print("a >= b")
}

for (i in 1:5) {
   print (i)
}

n <- 1
f <- 1
while (n < 5) {
   f <- f * n
   n <- n + 1
   print(f)
}

# Functions
(num <- runif(20, -1, 1)) # 20 random numbers from a uniform distribution

my.mean <- function (x) {
   len <- length(x)
   sum(x)/len
}
my.mean

my.mean(num)

fact <- function (m) {
   if (m > 1) {
      f <- fact(m - 1) * m
   } else {
      return(1)
   }
   f
}

# debug(fact)
# f <- 0
# fact(5)
# 
# m;f;n

   # Visualization

code.dir <- "C:/Users/lab/Documents/R/Zixin_Nie"
data.dir <- "C:/Users/lab/Documents/R/Zixin_Nie/Data"

   # Set the files to be read
(d.file <- paste(data.dir, "flea.dat", sep="/"))
(d.column <- paste(data.dir, "flea.col", sep="/")) 

(headers <- scan(d.column))
(headers <- scan(d.column, "" ))
(n.var <- length(headers))   # The length of the vector gives the number of variables

d.flea.s <- matrix(scan(d.file), ncol=n.var, byrow=T)
d.flea.s[1:5,]

d.flea.str <- matrix(scan(d.file, ""), ncol=n.var, byrow=T)
d.flea.str[1:5,]

as.numeric(d.flea.str[1,1])

d.flea.s <- matrix(as.numeric(d.flea.str), ncol=n.var)
d.flea.s[1:5,]

d.flea <- read.table(d.file)
d.flea[1:5,]

d.flea.s[c(5,10,15,20), c(2,4)]

d.flea[c(5,10,15,20), c(2,4)]

dim(d.flea.s)

dimnames(d.flea.s) <- list(1:dim(d.flea.s)[1], headers)
d.flea.s[c(5,10,15,20), c(2,4)]

colnames(d.flea) <- headers
d.flea[c(5,10,15,20), c(2,4)]

flea.species <- c(rep("C",21),rep("Hp",22),rep("Hk",31))
species <- c(rep(1,21),rep(2,22),rep(3,31))

(d.row <- paste(data.dir, "flea.row", sep="/")) 
(row.headers <- scan(d.row, ""))
(row.headers <- noquote(scan(d.row, "")))

is.data.frame(d.flea)

df.flea <- data.frame(d.flea.s)

source(paste(code.dir, "DispStr.r", sep = "/"))   
source(paste(code.dir, "pairs_ext.r", sep = "/"))
source(paste(code.dir, "makestereo.r", sep = "/"))

# We can look at a scatterplot matrix of our data values
graphics.off()
pairs(d.flea)

# This can be made more informative by displaying,
# the histograms of the variables and the correlations,
# between them.
# Note that the size of the correlation determines
# the size of the number.
graphics.off()
pairs(d.flea, upper.panel = panel.cor, diag.panel = panel.hist)

debug(panel.cor)
graphics.off()
pairs(d.flea, upper.panel = panel.cor, diag.panel = panel.hist)
undebug(panel.cor)

graphics.off()
pairs(d.flea, upper.panel = panel.cor, diag.panel = panel.hist)

graphics.off()
pairs(d.flea, col = species + 1)

graphics.off()
coplot(aede3 ~ tars1 | aede1, data = df.flea)
graphics.off()
coplot(aede3 ~ tars1 | aede1, data = df.flea, overlap = 0.1)
graphics.off()
coplot(aede3 ~ tars1 | aede1, data = df.flea, overlap = 0.1, col = species + 1, pch = 16)

library(lattice)

equal.space <- function(data, count) {
      # range(data) gives the max and min of the variable data.
      # diff takes the difference between the two values so
      # diffs gives the width of each interval.
   diffs <- diff(range(data))/count
      # min(data)+diffs*(0:(count-1)) gives the starting values
      #    for the intervals.
      # min(data)+diffs*(1:count) gives the ending values
      #    for the intervals.
      # cbind treats two(or more) vectors as column vectors
      #    and binds them as columns of a matrix.
   intervals <- cbind(min(data)+diffs*(0:(count-1)),
                      min(data)+diffs*(1:count))
      # shingle takes the interval structure and the data
      #    and breaks the data into the appropriate groups.
   return (shingle(data, intervals))
}   

C1 <- equal.count(df.flea$aede1, number = 6, overlap = 0.1)
xyplot(aede3 ~ tars1 | C1, data = df.flea, pch = 19)

C2 <- equal.space(df.flea$aede1, 6)
xyplot(aede3 ~ tars1 | C2, data = df.flea, pch = 19)

source(paste(d.R <- code.dir, "ellipseoutline.r", sep="/"))
ec.t1 <- {}
for (t in -20:20) {
   ec.t1 <- rbind(ec.t1, cbind(ellipse.outline(20, 20, 10, 5, t, 0, (200-t^2)/10), t))
}
ec.t1 <- data.frame(ec.t1[sample(dim(ec.t1)[1], dim(ec.t1)[1]),])
pairs(ec.t1, upper.panel = panel.cor, diag.panel = panel.hist)

X <- equal.space(ec.t1$x, 25)
Y <- equal.space(ec.t1$y, 25)
Z <- equal.space(ec.t1$z, 25)
T <- equal.space(ec.t1$t, 25)

xyplot(z ~ x | Y, data = ec.t1, pch=".", main ="z ~ x | Y",
                  aspect = diff(range(ec.t1$z))/diff(range(ec.t1$x)))

x11()
xyplot(y ~ x | Z, data = ec.t1, pch=".", main ="y ~ x | Z",
                  aspect = diff(range(ec.t1$y))/diff(range(ec.t1$x)))

x11()
xyplot(z ~ y | X, data = ec.t1, pch=".", main ="z ~ y | X",
                  aspect = diff(range(ec.t1$z))/diff(range(ec.t1$y)))

x11()
xyplot(z ~ x | T, data = ec.t1, pch=".", main ="z ~ x | T", 
                  aspect = diff(range(ec.t1$z))/diff(range(ec.t1$x)))

Z5 <- equal.space(ec.t1$z, 5)
T5 <- equal.space(ec.t1$t, 5)
x11()
xyplot(z ~ x | T5*Z5, data = ec.t1, main ="z ~ x | T5*Z5", pch=".",
                   aspect = diff(range(ec.t1$z))/diff(range(ec.t1$x)))


r <- 1
c <- 1
for (i in -20:15) {               # Loop through i from -20 to 15
   ind <- ec.t1$t==i              # Get the cases for which the t value == i
   X <- ec.t1$x[ind]              # And the corresponding x,y,z values
   Y <- ec.t1$y[ind]
   Z <- ec.t1$z[ind]
      # In the following - ( ?cloud)
      # print - displays the
      # cloud - a function that creates a cloud of points,
      #         with xlim, ylim, zlim (the range of values on the axes)
      #         set to the maximum range (x) to give proper scaling.
      # subpanel - the function use to plot the points.
      # groups - allows classes to be identified.
      # screen - sets the viewpoint.
      # split  - c(col, row, cols, rows)
      # more   -
   print(cloud(Z ~ X*Y, xlim = range(ec.t1$x),
                ylim = range(ec.t1$x),zlim = range(ec.t1$x),
                subpanel = panel.superpose, groups = rep(1,dim(ec.t1)[1]),
                screen = list(z = 10, x = -80, y = 0), data = ec.t1),
                split = c(c, r, 6, 6), more = TRUE)
   c <- c+1
   if (c%%6 == 1) {   # Remainder mod 6
      c <- 1
      r <- r+1
   }
}

   # Ggobi

source(paste(code.dir, "ReadFleas.r", sep="/"))

library(rggobi)
g <- ggobi(d.flea)

   # This opens with a scatterplot of the projections on
   # one plane - i.e. a part of the scatterplot matrix.
   # We can get a scatterplot matrix in Ggobi
display(g[1], "Scatterplot Matrix")
   # or - [Display][New scatterplot matrix]
   # from the Ggobi console.
   # (This may not display all pairs by default
   #  - you will need to select the other variables.
   # A view that gives a better look at the data is selected by
   #  - [ViewMode][2D Tour]

display(g[1], "2D Tour")
   # This gives a 2D "tour" of the 6 dimensional data.
   # The portion of each variable in the view is shown by the
   # representation of the axis in the bottom corner
   # (and on the console).
   # As the tour runs 3 clusters will appear.
   # When they do, you can click [Pause] and apply brushing
   # - [Interaction][Brush]

   # As we move the brush, the data points change colour as we
   # pass over them but return to normal when we move away.
   # To change this behaviour, select
   # [Transient] and change to [Persistent]

           
(old.col <- glyph_colour(g[1]))

   # It turns out that we know the species of the
   # flea beetles so we can compare the clustering that
   # we observed with the true classification.
(noquote(rbind(flea.species, old.col)))

   # Set the points to a single colour and style.
glyph_colour(g[1]) <-rep(2, 74)
glyph_type(g[1]) <-rep(4, 74)
   # Another use of brushing is in linked plots,
   # so we can open other displays.
display(g[1], "Parallel Coordinates Display")
   # Parallel coordinates are good for looking at
   # high dimensional data.
   # With the tour made active, select a [Transient]
   # brush and a color.
   # Notice how all the displays respond to the brushing.
   # This enables us to identify corresponding values.

   # In order to proceed with other aspects of linking
   # we will use the colours based on the species.
glyph_colour(g[1]) <- c(rep(6,21),rep(4,22),rep(9,31))

cols <- rep(6, 74)
cols[which(d.flea[,6] < 95)] <- 9
cols[which(d.flea[,1] < 160)] <- 4
glyph_colour(g[1]) <- cols

close(g)

source(paste(code.dir, "MakeStereo.r", sep="/"))

   #Stereo
   # Use the 3 variables that seemed useful
make.Stereo(d.flea[,c(1,5,6)], species, Main="Flea beetles", asp="F",
           Xlab="tars1", Ylab="aede2", Zlab="aede3")

make.Stereo(d.flea[,c(6, 5, 1)], species, Main="Flea beetles", asp="F",
           Zlab="tars1", Ylab="aede2", Xlab="aede3")

   # rgl
library(rgl)

plot3d(d.flea[,1], d.flea[,5], d.flea[,6], xlab="tars1",ylab="aede2",zlab="aede3",
       col=species+1, size=0.5, type="s")

for (j in seq(0, 90, 10)) {
   for(i in 0:360) {
      rgl.viewpoint(i, j);
   }
}


   # Randu
   # The Randu random number data set.
   # First look at the pairs data.

d.file <- paste(data.dir, "randu.dat", sep = "/")
d.randu <- read.table(d.file)
pairs(d.randu, upper.panel=panel.cor, diag.panel=panel.hist)

library(rggobi)
g <- ggobi(d.randu)
   # Use a [View][2D tour]
close(g)

   #====== Prim7 ============
prim <- read.table(paste(data.dir, "prim7.dat",sep="/"))
g <- ggobi(prim)

new.col <- rep(1, 500)
col.2 <- c(2,3,4,14,15,16,17,18,21,23,30,34,37,41,43,46,49,50,53,54,55,
           57,58,63,65,66,69,70,72,73,74,75,77,78,79,85,86,88,90,91,92,
           93,94,95,99,100,102,104,105,106,107,109,110,113,114,116,120,
           121,124,125,126,127,129,130,133,139,140,141,143,145,147,150,
           152,153,157,158,159,160,161,164,166,169,172,175,176,177,178,
           180,185,194,195,198,200,203,204,209,210,211,212,218,219,220,
           222,223,226,228,229,233,234,236,238,240,242,244,245,246,248,
           249,252,253,257,259,263,264,265,266,267,269,270,273,277,278,
           280,281,282,283,284,286,292,294,296,297,300,305,310,311,314,
           315,317,323,331,332,333,334,335,341,342,343,346,351,356,359,
           360,361,362,365,370,372,374,375,377,378,379,380,383,386,388,
           389,390,391,393,397,398,400,402,403,405,407,408,413,414,415,
           417,418,419,420,425,427,428,429,430,432,433,434,436,437,438,
           440,444,445,447,448,452,453,454,455,456,463,465,467,470,471,
           473,476,477,478,480,481,482,484,485,487,488,489,490,491,494,
           497)
col.3 <- c(11,20,27,33,47,51,60,61,62,98,115,118,119,132,155,186,191,
           193,202,205,207,208,213,225,230,231,232,235,239,243,250,251,
           268,272,295,312,316,338,339,345,349,354,358,364,366,376,381,
           395,401,421,422,446,460,496)
col.5 <- c(5,8,13,19,26,32,39,48,56,71,81,96,111,136,137,144,149,156,
           162,165,188,199,201,216,255,262,274,279,289,291,301,320,322,
           326,327,329,344,348,353,363,367,369,384,399,404,406,411,423,
           441,442,443,469,474,479,483,495,499,500)
col.8 <- c(7,29,31,36,89,101,117,131,138,154,173,187,190,192,196,197,
           206,247,254,256,258,287,290,298,299,309,324,325,385,387,464)
col.9 <- c(1,12,22,24,25,44,45,52,64,83,103,108,122,123,134,135,146,151,
           167,168,170,174,179,181,184,221,224,237,261,271,285,293,304,
           306,307,308,319,328,337,352,355,357,368,396,410,424,426,435,
           439,449,451,458,461,462,466,472,475,493)
           

new.col[col.2] <-  2
glyph_colour(g[1]) <-  new.col

new.col[col.3] <-  3
glyph_colour(g[1]) <-  new.col

new.col[col.5] <-  5
glyph_colour(g[1]) <-  new.col

new.col[col.8] <-  8
glyph_colour(g[1]) <-  new.col

new.col[col.9] <-  9
glyph_colour(g[1]) <-  new.col

prim.lin <- read.table(paste(data.dir, "prim7.lines",sep="/"))
edges(g[1]) <- prim.lin

close(g)

g <- ggobi(paste(data.dir, "cube6.xml",sep="/"))
close(g)