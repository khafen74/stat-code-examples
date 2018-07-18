
# Set working directory and read data -------------------------------------

setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "turtles.csv"
dataAll = read.csv(fn)
female = dataAll[dataAll$Gender == 'female', 1:3]
male = dataAll[dataAll$Gender == 'male', 1:3]

# Example code ------------------------------------------------------------

#covariance matrix
cov.f = cov(female)
cov.m = cov(male)

#corelation matrix
cor.f = cor(female)
cor.m = cor(male)


# Principle components analysis -------------------------------------------
males.eigen = eigen(cov.m) #two parts to this object, values and vectors
males.evals = males.eigen$values

#variability accounted for by each principle component
100 * males.evals/sum(males.evals) # 97% of variability accounted for by first principle component (data are ~1 dimensional)

males.v1 = males.eigen$vectors[,1] #fist eigen vector, vector corresponding to first eigen value
sum(males.v1^2) #check if it is of unit length, should equal 1

#covariance multiplied by eigenvector should equal eigenvalue multiplied by eigenvector
cov.m%*%males.v1
males.evals[1]*males.v1

#direction of maximum variability i.e. the eigenvalue
t(males.v1)%*%cov.m%*%males.v1

##PCA on correlation matrix, for when values are not similar -> i.e. there is a differnce in scale and/or variability of values
cor.eigen = eigen(cor.m)
100*eigen(cor.m)$values/sum(eigen(cor.m)$values)
