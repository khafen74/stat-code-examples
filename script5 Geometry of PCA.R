#
# script5.R
#
# Stat 5600
# Geometry of Principal Components
#
# the first part of the code is similar to script 4
# we generate data from a bivariate normal and plot
# the estimated density ellipse, but this time we make the 
# plot square
#
# the second part of the code shows how an 
# eigendecomposition of the estimated covariance matrix 
# relates to the estimated density ellipse
#
library(MASS)
library(ellipse)

# generate data from a bivariate normal distribution
mu = c(3,3)
rho = matrix( c(1,.9,.9,1), nrow=2, ncol=2)
sds = c(1,1)

# mvrnorm takes mu and Sigma, the covariance matrix:

Sigma = diag(sds)%*%rho%*%diag(sds)

x = mvrnorm(1000, mu, Sigma )

# this time, do a square plot
par(pty="s")
xlim = extendrange(x[,1:2], f = 0.15)
ylim = xlim
plot(x,xlab="",ylab="",xlim=xlim,ylim=ylim,col="red")

# estimate the parameters
muhat = apply(x,2,mean)
SDhat = apply(x,2,sd)
Sighat = cov(x)
rhohat = cor(x)

# estimated density contour:
lines(ellipse(rhohat,scale=SDhat,centre=muhat,level=.95),type="l",col="blue")

# eigendecomposition of the estimated covariance matrix
# v gives the eigenvectors (one in each column)
# lambda gives the eigenvalues
e = eigen(Sighat)
v1 = e$vectors[,1]
lambda1 = e$values[1]
v2 = e$vectors[,2]
lambda2 = e$values[2]

# the major axis:
slope = v1[2]/v1[1] 
intercept = muhat[2] - slope*muhat[1]
abline(intercept,slope)

# the minor axis:
slope = v2[2]/v2[1] 
intercept = muhat[2] - slope*muhat[1]
abline(intercept,slope)

plot(x,xlab="",ylab="",xlim=xlim,ylim=ylim,type="n")
lines(ellipse(rhohat,scale=SDhat,centre=muhat,level=.95),type="l",col="blue")

chi95 = qchisq(p=.95,df=2)
# the major axis:
x0 = muhat - sqrt(lambda1*chi95)*v1
x1 = muhat + sqrt(lambda1*chi95)*v1
segments(x0[1],x0[2],x1[1],x1[2])
# the minor axis
x0 = muhat - sqrt(lambda2*chi95)*v2
x1 = muhat + sqrt(lambda2*chi95)*v2
segments(x0[1],x0[2],x1[1],x1[2])
