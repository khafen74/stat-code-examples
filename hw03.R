# Set working directory and read data -------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "Iris.csv"
iris = read.csv(fn)


# Libraries ---------------------------------------------------------------

library(MASS)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(biotools)
library(klaR)

# Manipulate data ---------------------------------------------------------

set = iris[iris$Species == "setosa", 1:4]
ver = iris[iris$Species == "versicolor", 1:4]
vir = iris[iris$Species == "virginica", 1:4]
set.m = melt(set)
ver.m = melt(ver)
vir.m = melt(vir)


# Boxplots for each species -----------------------------------------------

bp1 = ggplot(data = melt(set.m), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("setosa") +
  ylab("") +
  xlab("")

bp2 = ggplot(data = melt(ver.m), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("versicolor") +
  ylab("Millimeters (mm)") +
  xlab("")

bp3 = ggplot(data = melt(vir.m), aes(x=variable, y=value)) + 
  geom_boxplot()+
  ggtitle("virginica")+
  ylab("") +
  xlab("")

grid.arrange(bp1, bp2, bp3)


# Histograms --------------------------------------------------------------

ggplot(data = melt(set.m), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

ggplot(data = melt(ver.m), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

ggplot(data = melt(vir.m), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')


# Covariance matrices -----------------------------------------------------

set.cov = cov(set)
ver.cov = cov(ver)
vir.cov = cov(vir)

boxM(iris[, -5], iris[, 5])


# Test variables ----------------------------------------------------------

n = nrow(iris)
iris.qda.cv = qda(Species ~ SepalLength+SepalWidth+PetalLength+PetalWidth, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ SepalLength+SepalWidth+PetalLength, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ SepalLength+SepalWidth+PetalWidth, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ SepalLength+PetalLength+PetalWidth, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ SepalWidth+PetalLength+PetalWidth, CV = TRUE, data = iris)

iris.qda.cv = qda(Species ~ SepalLength+PetalLength, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ SepalLength+PetalWidth, CV = TRUE, data = iris)
iris.qda.cv = qda(Species ~ PetalLength+PetalWidth, CV = TRUE, data = iris)

table(iris$Species, iris.qda.cv$class)
100-100*sum(diag(table(iris$Species, iris.qda.cv$class)))/n

###########################################################################
#################### Nest Dataset #########################################
###########################################################################

# Set working directory and read data -------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "Nest.csv"
nest = read.csv(fn)


# Subset data -------------------------------------------------------------

flick = subset(nest, nest$Species == "Flicker" | nest$Species == "Non-nest")
chick = subset(nest, nest$Species == "Chickadee" | nest$Species == "Non-nest")
sap = subset(nest, nest$Species == "Sapsucker" | nest$Species == "Non-nest")
drops = c("Species")
nest = nest[ , !(names(nest) %in% drops)] # drop species column
chick = chick[ , !(names(chick) %in% drops)] # drop species column
flick = flick[ , !(names(flick) %in% drops)] # drop species column
sap = sap[ , !(names(sap) %in% drops)] # drop species column


# LR, LDA, and QDA for all nest data with CV ------------------------------

nest.lr = glm(Nest ~ ., family=binomial, data = nest)
nest.lda = lda(Nest ~ ., data = nest)
nest.qda = qda(Nest ~ ., data = nest)


# 10-fold CV for LR -------------------------------------------------------

#with 10-fold crossvalidation
nest.lr.xval=rep(0,nrow(nest))
xvs=rep(1:10,length=nrow(nest))
xvs=sample(xvs)
for(i in 1:10){
    train=nest[xvs!=i,]
    test=nest[xvs==i,]
    glub=glm(Nest~ ., family=binomial,data=train)
    nest.lr.xval[xvs==i]=predict(glub,test,type="response")
}
100 - 100 * sum(diag(table(nest$Nest,round(nest.lr.xval))))/nrow(nest)
class.sum(nest$Nest,nest.lr.xval)

# 10-fold CV for LDA ------------------------------------------------------

nest.lda.xval = rep(0, length=nrow(nest)) #length = length of dataset, can use nrow(dataset)
x=rep(1:10, length=nrow(nest))

x=sample(x) #random permutation of x

for(i in 1:10)
{
  train = nest[x!=i,]
  test = nest[x==i,]
  glub = lda(Nest~ ., data=train)
  nest.lda.xval[x==i]=predict(glub,test)$class
}
100 - 100 * sum(diag(table(nest$Nest, nest.lda.xval)))/nrow(nest)


# 10-fold CV for QDA ------------------------------------------------------

nest.qda.xval = rep(0, length=nrow(nest)) #length = length of dataset, can use nrow(dataset)
x=rep(1:10, length=nrow(nest))

x=sample(x) #random permutation of x

for(i in 1:10)
{
  train = nest[x!=i,]
  test = nest[x==i,]
  glub = qda(Nest~ ., data=train)
  nest.qda.xval[x==i]=predict(glub,test)$class
}
100 - 100 * sum(diag(table(nest$Nest,nest.qda.xval)))/nrow(nest)


# Identify important variables from LR ------------------------------------

nest.lr.step = step(nest.lr)
table(nest$Nest,round(predict(nest.lr.step,type="response")))
100-100*sum(diag(table(nest$Nest,round(predict(nest.lr.step,type="response")))))/nrow(nest)
class.sum(nest$Nest,predict(nest.lr.step,type="response"))


# Identify important variables from LDA -----------------------------------

stepclass(Nest ~ ., data=nest, method="lda", direction="backward", performance.method = "CF", improvement=0.001, trace=FALSE)


# Test above stepwise LDA to see if it seems right ------------------------

#test with 10-fold CV (to see if the model produced above gives the same error estimates)
nest.lda.xval = rep(0, length=nrow(nest)) #length = length of dataset, can use nrow(dataset)
x=rep(1:10, length=nrow(nest))

x=sample(x) #random permutation of x

for(i in 1:10)
{
  train = nest[x!=i,]
  test = nest[x==i,]
  glub = lda(Nest ~ NumTreelt1in + NumTree1to3in + NumTree3to6in + NumTree6to9in + 
    NumTree9to15in + NumSnags + PctShrubCover + NumConifer + 
    StandType, data=train)
  nest.lda.xval[x==i]=predict(glub,test)$class
}
100 - 100 * sum(diag(table(nest$Nest, nest.lda.xval)))/nrow(nest)

# LR for each species separately ------------------------------------------

flick.lr = glm(Nest ~ ., data = flick, family = binomial)
flick.lr.step = step(flick.lr)
100-100*sum(diag(table(flick$Nest,round(predict(flick.lr.step,type="response")))))/nrow(flick)
class.sum(flick$Nest,predict(flick.lr.step,type="response"))

chick.lr = glm(Nest ~ ., data = chick, family = binomial)
chick.lr.step = step(chick.lr)
100-100*sum(diag(table(chick$Nest,round(predict(chick.lr.step,type="response")))))/nrow(chick)
class.sum(chick$Nest,predict(chick.lr.step,type="response"))

sap.lr = glm(Nest ~ ., data = sap, family = binomial)
sap.lr.step = step(sap.lr)
100-100*sum(diag(table(sap$Nest,round(predict(sap.lr.step,type="response")))))/nrow(sap)
class.sum(sap$Nest,predict(sap.lr.step,type="response"))

# #LDA for each species separately ----------------------------------------

stepclass(Nest ~ ., data = flick, method = "lda", direction = "backward", improvement=0.001)
stepclass(Nest ~ ., data = chick, method = "lda", direction = "backward", improvement=0.001)
stepclass(Nest ~ ., data = sap, method = "lda", direction = "backward", improvement=0.001)






