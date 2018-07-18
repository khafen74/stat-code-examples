
# Load packages -----------------------------------------------------------

library(MASS)
library(ellipse)


# Generate data from a bivariate normal dist ------------------------------

#mu = vector of means, size 2x1 with values 3, 3
mu = c(3,3)
#rho = R = correlation matrix, 2x2 matrix with values, 1, 0.9, 0.9, 1
rho = matrix(c(1, 0.9, 0.9, 1), nrow=2, ncol=2)
#vector of standard deviations, size 2x1 with values 1, 1
sds = c(1,1)

#mvrnorm takes mu and sigma (the covaraition matrix)
#sigma is the covariance matrix
sigma = diag(sds) %*% rho %*% diag(sds) #diag creates matrix with values from vector "sds" down the diagonal
x = mvrnorm(1000, mu, sigma) #matrix x has 1000 rows and 2 columns (1000x2), this is the data matrix

#square plot
par(pty="s")
xlim = extendrange(x[,1:2], f=0.15)
ylim = xlim
plot(x, xlab="", ylab="", xlim=xlim, ylim=ylim, col="red")

#estimate the parameters
muhat = apply(x,2,mean) #sample means
SDhat = apply(x,2,sd) #sample standard deviations
sighat = cov(x) #sample covariance matrix
rhohat = cor(x) #sample correlation matrix

#estimated density contour
lines(ellipse(rhohat,scale=SDhat, centre=muhat, level=0.95), type = "l", col = "blue")

#eigendecomposition of the estimate covariance matrix
#v gives the eigenvectors (one in each column)
#lambda gives the eigenvalues
e = eigen(sighat)
v1 = e$vectors[,1]
lambda1 = e$values[1]
v2 = e$vectors[,2]
lambda2 = e$values[2]

#the major axis
slope = v1[2]/v1[1]
intercept = muhat[2] - slope*muhat[1]
abline(intercept, slope)

#the minor axis
slope = v2[2]/v2[1]
intercept = muhat[2] - slope*muhat[1]
abline(intercept, slope)

plot(x, xlab="", ylab="", xlim=xlim, type = "n")
lines(ellipse(rhohat, scale=SDhat, centre=muhat,level=0.95), type="l")
