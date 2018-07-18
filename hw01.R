#STAT 5810 Homework 1

# Load libraries ----------------------------------------------------------

library(ggplot2)
library(reshape2)
library(gridExtra)
library(MASS)

# Set Working Directory and Read Data -------------------------------------

setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "turtles.csv"
dataAll = read.csv(fn)
dataF = dataAll[dataAll$Gender == 'female', 1:3]
dataM = dataAll[dataAll$Gender == 'male', 1:3]
stack = melt(dataAll)
stackMale = melt(dataM)
stackFemale = melt(dataF)

###########################################################################
################ QUESTION 1: PLOTS ########################################
###########################################################################

# Box Plots ---------------------------------------------------------------

bp1 = ggplot(data = melt(stack), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("All Turtles") +
  ylab("") +
  xlab("")

bp2 = ggplot(data = melt(stackMale), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("Male Turtles") +
  ylab("Millimeters (mm)") +
  xlab("")

bp3 = ggplot(data = melt(stackFemale), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("Female Turtles")+
  ylab("") +
  xlab("")

grid.arrange(bp1, bp2, bp3)

# Histograms --------------------------------------------------------------

nBins = 15
h1 = qplot(dataAll$Length, geom = "histogram", xlab = "", ylab = "", main = "All Turtles", bins = nBins)
h2 = qplot(male$Length, geom = "histogram", xlab = "Length (mm)", ylab = "", main = "Male Turtles", bins = nBins)
h3 = qplot(female$Length, geom = "histogram", xlab = "", ylab = "", main = "Female Turtles", bins = nBins)

h4 = qplot(dataAll$Width, geom = "histogram", xlab = "", ylab = "Count", bins = nBins)
h5 = qplot(male$Width, geom = "histogram", xlab = "Width (mm)", , ylab = "", bins = nBins)
h6 = qplot(female$Width, geom = "histogram", xlab = "", ylab = "", bins = nBins)

h7 = qplot(dataAll$Height, geom = "histogram", xlab = "", ylab = "", bins = nBins)
h8 = qplot(male$Height, geom = "histogram", xlab = "Height (mm)", ylab = "", bins = nBins)
h9 = qplot(female$Height, geom = "histogram", xlab = "", ylab = "", bins = nBins)

grid.arrange(h1, h2, h3, h4, h5, h6, h7, h8, h9)

# Normal Quantile Plots ---------------------------------------------------

y <- quantile(dataAll$Length[!is.na(dataAll$Length)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q1 = ggplot(dataAll, aes(sample = Length)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ggtitle("All Turtles") +
  ylab("Length (mm)") +
  xlab("")

y <- quantile(male$Length[!is.na(male$Length)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q2 = ggplot(male, aes(sample = Length)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ggtitle("Male Turtles") +
  ylab("") +
  xlab("")

y <- quantile(female$Length[!is.na(female$Length)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q3 = ggplot(female, aes(sample = Length)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ggtitle("Female Turtles") +
  ylab("") +
  xlab("")

y <- quantile(dataAll$Width[!is.na(dataAll$Width)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q4 = ggplot(dataAll, aes(sample = Width)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("Width (mm)") +
  xlab("")

y <- quantile(male$Width[!is.na(male$Width)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q5 = ggplot(male, aes(sample = Width)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("") +
  xlab("")

y <- quantile(female$Width[!is.na(female$Width)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q6 = ggplot(female, aes(sample = Width)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("") +
  xlab("")

y <- quantile(dataAll$Height[!is.na(dataAll$Height)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q7 = ggplot(dataAll, aes(sample = Height)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("Height (mm)") +
  xlab("")

y <- quantile(male$Height[!is.na(male$Height)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q8 = ggplot(male, aes(sample = Height)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("") +
  xlab("Theoretical")

y <- quantile(female$Height[!is.na(female$Height)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
q9 = ggplot(female, aes(sample = Height)) + stat_qq() + geom_abline(slope = slope, intercept = int) +
  ylab("") +
  xlab("")


grid.arrange(q1, q2, q3, q4, q5, q6, q7, q8, q9)

###########################################################################
################## QUESTION 2: COV AND COR MATRICES #######################
###########################################################################

# Compute covariance matrices ---------------------------------------------

#covariance matrices
cov.m = cov(dataM)
cov.f = cov(dataF)

#corelation matrices
cor.m = cor(dataM)
cor.f = cor(dataF)

###########################################################################
################## QUESTION 3: SSCP MATRIX ################################
###########################################################################

# Compute SSCP matrix -----------------------------------------------------

#n = number of rows
n = length(dataM$Length)

#Convert to matrix
X = data.matrix(dataM)

#Create vector of ones
ones = rep(1, n)

#Compute x bar matrix
xbar = (1/n) * t(X) %*% ones

#Compute SSCP
sscp = (t(X) %*% X) - (n * xbar %*% t(xbar))

#Compute T
T = (1/(n-1)) * sscp

###########################################################################
################### QUESTION 4: EIGENVALUES AND EIGENVECTORS ##############
###########################################################################

#Compute the inverse of cov matrix for male turtles and eigenvectors and values
covm.inv = ginv(cov.m)
eigen.m.inv = eigen(covm.inv)

i = diag(3,3)
covm.inv = solve(cov.m)
eigen.m.inv = eigen(covm.inv)

#Compute eigenvectors and values for cov matrix of male turtles
eigen.m = eigen(cov.m)

#Percent variation of eigenvalues
100 * eigen.m.inv$values / sum(eigen.m.inv$values)

###########################################################################
########### QUESTION 5: RELATIONSHIP BTWN EIGENVALUES AND EIGENVECTORS ####
###########################################################################

#eigenvalue
eigen.m$values[1]

#cov matrix multiplied by eigenvector
cov.m %*% eigen.m$vectors[,1]

#eigenvalue multiplied by eigenvector
eigen.m$values[1] %*% eigen.m$vectors[,1]

t(eigen.m$vectors[,1]) %*% cov.m %*% eigen.m$vectors[,1]

###########################################################################
##### QUESTION 7: EIGENVALUES AND EIGENVECTORS FOR COR MATRIX##############
###########################################################################

eigen.m.cor = eigen(cor.m)
eigen.m.cor$values
eigen.m.cor$vectors

100 * eigen.m.cor$values / sum(eigen.m.cor$values)
100 * eigen.m$values / sum(eigen.m$values)
