# Set working directory and read data -------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "turtles.csv"
turtles = read.csv(fn)


# Libraries ---------------------------------------------------------------

library(MASS)


# LDA ---------------------------------------------------------------------

#resubstitution
#this is the model that should be used, we're just evaluating with resubstitution
turtles.lda = lda(Gender ~ ., data = turtles) #predict gender as a function of all other variables
table(turtles$Gender, predict(turtles.lda)$class) #confusion matrix, rows observed genders, columns predicted genders

#tsting above movel using delete one crossvalidation
turtles.lda.cv = lda(Gender ~ ., CV = TRUE, data = turtles)
table(turtles$Gender, turtles.lda.cv$class)


# LDA with 10-fold cross validation ---------------------------------------

#testing above model using 10-fold crossvalidation
turtles.lda.xval = rep(0, length=48) #length = length of dataset, can use nrow(dataset)
x=rep(1:10, length=48)

x=sample(x) #random permutation of x

for(i in 1:10)
{
  train = turtles[x!=i,]
  test = turtles[x==i,]
  glub = lda(Gender~Length+Width+Height, data=train)
  turtles.lda.xval[x==i]=predict(glub,test)$class
}
table(turtles$Gender,turtles.lda.xval)

# QDA ---------------------------------------------------------------------

turtles.qda = qda(Gender ~ ., data = turtles)
table(turtles$Gender, predict(turtles.qda)$class)

turtles.qda.cv = qda(Gender ~ ., CV = TRUE, data = turtles)
table(turtles$Gender, turtles.lda.cv$class)

# QDA with 10-fold cross validation ---------------------------------------

#testing above model using 10-fold crossvalidation
turtles.qda.xval = rep(0, length=48) #length = length of dataset, can use nrow(dataset)
x=rep(1:10, length=48)

x=sample(x) #random permutation of x

for(i in 1:10)
{
  train = turtles[x!=i,]
  test = turtles[x==i,]
  glub = qda(Gender~Length+Width+Height, data=train)
  turtles.qda.xval[x==i]=predict(glub,test)$class
}
table(turtles$Gender,turtles.qda.xval)
