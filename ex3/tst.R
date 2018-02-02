x <- 1:100
dat <- data.frame(x,y=x^3+x^2+x+5)
f <- function(x) x^3+x^2+x+5
ggplot(dat, aes(x,y)) + 
  geom_point()+
  stat_function(fun=f, colour="red")

m = matrix(runif(100),10,10)
image(m)

image(data3$X)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)

layout.show(n = 1)
par(mfrow = c(1,1))

layout(matrix(1:25, 5, 5, byrow = TRUE))

par(mfrow = c(5, 5))
sample<-sample(1:nrow(data3$X), 25, replace=F)
for(i in sample){
  t<-matrix(data3$X[i,],20,20,byrow=T)
  t1<-t[,20:1]
  image(t1)
}
t<-matrix(data3$X[3719,],20,20,byrow=T)
t1<-t[,20:1]
# image(matrix(t,20,20,byrow=T))
image(t1)
# heatmap(matrix(t,20,20),Rowv=NA,Colv=NA,col=paste("gray",1:99,sep=""))


vec<-as.vector(rnorm(1:924))
heatmap(matrix(vec,33,28),Rowv=NA,Colv=NA,col=paste("gray",1:99,sep=""))

library(nnet)
data.df<-cbind(as.factor(data3$y),as.data.frame(data3$X))
colnames(data.df)[1]<-"y"
data.df<-tbl_df(data.df)
test <- multinom(y ~ ., data = data.df)
tst <- nnet(data3$X, data3$y, size = 25,MaxNWts=20000)
y<-array(0,dim=c(nrow(data3$y),10))
for (i in 1:nrow(data3$y)){
  val<-data3$y[i]
  y[i,val]<-1
}
tst <- nnet(data3$X, y, size = 25, rang = 0.5,MaxNWts=20000 ,decay = 5e-4, maxit = 100)
test.cl <- function(true, pred) {
  true <- (true)
  cres <- (pred)
  table(true, cres)
}
pred<-predict(tst, data3$X)
ypred<-max.col(pred)
tst_tbl<-test.cl(data3$y, ypred)
mean(ypred == data3$y) * 100



net$n <- c(dim(data3$X)[2L], 25, dim(y)[2L])
net$nunits <- as.integer(1L + sum(net$n))
net$nconn <- rep(0, net$nunits+1L)
net$conn <- numeric(0L)
net <- norm.net(net)


options(contrasts = c("contr.treatment", "contr.poly"))
library(MASS)
example(birthwt)
(bwt.mu <- multinom(low ~ ., bwt))

library(mlogit)
set.seed(75)
amat <- matrix( sample(10, size=60, replace=T), nrow=6)
apply(amat,1,max)

data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")



a<-c(1,2,3,4,5,6)
b<-c(3,2,3,4,3,6)
a==b
mean(a==b)

# nnet
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))
#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
#plot each model
plot.nnet(ir1)
plot.nnet(tst)



m <- ggplot(economics, aes(unemploy/pop, psavert))
m + geom_path()

movepath<-as.data.frame(centroids)
p <- p + geom_path(data=movepath,aes(x=V1, y=V2))
p


require(grDevices) # for colours
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1,
        xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple",
        main = "20 levels; \"simple\" labelling method")
par(op)


df<-data.frame(X[,1],X[,2],p)
colnames(df)<-c("x1","x2","p")
df2=df%>%arrange(x1,x2)
plot(X)
contour(df2$x1,df2$x2,df2$p)

x1<-seq(1, 25, length.out=300 )
x2<-seq(1, 25, length.out=300 )

# SIMULATING MULTIVARIATE DATA
# https://stat.ethz.ch/pipermail/r-help/2003-September/038314.html
# lets first simulate a bivariate normal sample
library(MASS)
# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package
# R offers several ways of visualizing the distribution. These next two lines of code overlay a contour plot on a "heat Map" that maps the density of points to a gradient of colors.

# Contour plot overlayed on heat map image of results
image(bivn.kde)       # from base graphics package
contour(bivn.kde,levels = c(.5,.10,.05,.01,.001,.0001), add = TRUE)     # from base graphics package

mu
sigma2 = matrix(c(   1.8326, 0,0, 1.7097),2,byrow=T)
Sigma = sigma2^.5
covx<-cov(X)
bivn <- mvrnorm(5000, mu = mu, Sigma = covx )  # from Mass package

bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)  
plot(X,col = "blue")



attach(geyser)
plot(duration, waiting, xlim = c(0.5,6), ylim = c(40,100))
f1 <- kde2d(duration, waiting, n = 50, lims = c(0.5, 6, 40, 100))
image(f1, zlim = c(0, 0.05))
f2 <- kde2d(duration, waiting, n = 50, lims = c(0.5, 6, 40, 100),
            h = c(width.SJ(duration), width.SJ(waiting)) )
image(f2, zlim = c(0, 0.05))
persp(f2, phi = 30, theta = 20, d = 5)


persp(bivn.kde, phi = 30, theta = 20, d = 5)


x <- seq(-4,4,length=100)*Sigma + mu
hx <- dnorm(x,mu,Sigma)

plot(x, hx)


require(graphics)

dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))
x=ex8data1$X[,1]
## Using "log = TRUE" for an extended range :
par(mfrow = c(2,1))
plot(function(x) dnorm(x), -10, 10,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)

plot(function(x) pnorm(x), -10, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("pnorm(x, log=TRUE)", adj = 0)
mtext("log(pnorm(x))", col = "red", adj = 1)

## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)

library(dplyr)
library(lazyeval)

not.uniq.per.group <- function(df, grp.var, uniq.var) {
  df %>%
    group_by_(grp.var) %>%
    summarise_( n_uniq=interp(~n_distinct(v), v=as.name(uniq.var)) ) %>%
    filter(n_uniq > 1)
}

not.uniq.per.group(iris, "Sepal.Length", "Sepal.Width")
                                                       
df1 <- tibble(x = 1:3)
a <- 10
mutate(df1, y = a + x)       
mutate_y <- function(df) {
  mutate(df, y = a + x)
}
df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5), 
  b = sample(5)
)
mutate_y(df1)
my_mutate <- function(df, expr) {
  expr <- enquo(expr)
  mean_name <- paste0("mean_", quo_name(expr))
  sum_name <- paste0("sum_", quo_name(expr))
  
  mutate(df, 
         !!mean_name := mean(!!expr), 
         !!sum_name := sum(!!expr)
  )
}

my_mutate(df, a)
