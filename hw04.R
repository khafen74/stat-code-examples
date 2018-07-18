##############################################################################
################ Some probability theory calculations ########################
##############################################################################

# Problem 1, bootstrapping theory -----------------------------------------

n = 50
x = seq(1, n, by=1)
p = 1/n
pn = (1/n)^n
pnx = (1/x)^x
pnn = (1-1/n)^n
pnnx = (1-1/x)^x

plot(x, pnnx, xlab="n", ylab="P")
lines(x, pnnx)
points(x, pnx, col="red")
lines(x, pnx, col="red")
legend("right", legend = c("Probability of an observation not being selected", 
                           "Probability of all unique observations being selected"),
       lwd=c(1,1),
       lty=c(1,1),
       col=c("black","red"))

max = 1000000000
(1-1/max)^max


for (i in 1:9){

  print ((1-(1/(10^i)))^(10^i))
}


# SE plots ----------------------------------------------------------------

n = 1000
p=0.37
x = seq(1,n,by=1)
se.x=sqrt(p*(1-p)/x)

plot(x, se.x, ylab= "SE (x)", xlab = "n")
lines(x, se.x)

plot(x, se.x*x, ylab= "SE (n)", xlab = "n")
lines(x, se.x*x)


##############################################################################
################ Problem 2 Birds #############################################
##############################################################################

# Problem 2 - Birds -------------------------------------------------------

# Set working directory and read data -------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "Nest.csv"
nest = read.csv(fn)

occ = nest[,-c(2)]
spp = nest[,-c(1)]
spp$Species = ifelse(spp$Species=="Non-nest","a_Non_nest")

# Subset data -------------------------------------------------------------

flick = subset(nest, nest$Species == "Flicker" | nest$Species == "Non-nest")
chick = subset(nest, nest$Species == "Chickadee" | nest$Species == "Non-nest")
sap = subset(nest, nest$Species == "Sapsucker" | nest$Species == "Non-nest")
drops = c("Species")
nest = nest[ , !(names(nest) %in% drops)] # drop species column
chick = chick[ , !(names(chick) %in% drops)] # drop species column
flick = flick[ , !(names(flick) %in% drops)] # drop species column
sap = sap[ , !(names(sap) %in% drops)] # drop species column


# Fit full classification tree --------------------------------------------

occ.full=rpart(Nest~., method="class", data=occ, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plot(occ.full)
plotcp(occ.full)
#try cp 0f 0.035, 0.054, and somewhere in between


# Refit tree --------------------------------------------------------------

#This one is best
occ.035=rpart(Nest~., method="class", data=occ, control=rpart.control(cp=0.035, minsplit=2)) #if method not specified regression tree is default
plot(occ.035, margin=0.1)
text(occ.035, use.n=TRUE)

table(occ$Nest,predict(occ.035,type="class"))
class.sum(occ$Nest,predict(occ.035,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(occ))
xvs=sample(xvs)
occ.035.xval=rep(0,length(nrow(occ)))
for(i in 1:10){
  train=occ[xvs!=i,]
  test=occ[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.035))
  occ.035.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(occ$Nest,round(occ.035.xval))
class.sum(occ$Nest,occ.035.xval)


occ.054=rpart(Nest~., method="class", data=occ, control=rpart.control(cp=0.054, minsplit=2)) #if method not specified regression tree is default
plot(occ.054, margin=0.1)
text(occ.054, use.n=TRUE)

table(occ$Nest,predict(occ.054,type="class"))
class.sum(occ$Nest,predict(occ.054,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(occ))
xvs=sample(xvs)
occ.054.xval=rep(0,length(nrow(occ)))
for(i in 1:10){
  train=occ[xvs!=i,]
  test=occ[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.054))
  occ.054.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(occ$Nest,round(occ.054.xval))
class.sum(occ$Nest,occ.054.xval)

#random forest
data=occ
xval.class=rep(0,length=nrow(data))
xval.prob=rep(0,length=nrow(data))
xvs=rep(1:10,length=nrow(data))
xvs=sample(xvs)
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  glub=randomForest(as.factor(Nest)~ . , data=train)
  xval.class[xvs==i]=predict(glub,test,type="response")
  xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
}
table(data$Nest,xval.class)
class.sum(data$Nest,xval.prob)



# Fit tree for each species separately ------------------------------------

chick.full=rpart(Nest~., method="class", data=chick, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plotcp(chick.full)

flick.full=rpart(Nest~., method="class", data=flick, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plotcp(flick.full)

sap.full=rpart(Nest~., method="class", data=sap, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plotcp(sap.full)

#cp for species: chick = 0.068, flick = 0.1, sap = 0.1 or 0.23

#fit pruned tree for chick
chick.068=rpart(Nest~., method="class", data=chick, control=rpart.control(cp=0.068, minsplit=2)) #if method not specified regression tree is default
plot(chick.068, margin=0.1)
text(chick.068, use.n=TRUE)

table(chick$Nest,predict(chick.068,type="class"))
class.sum(chick$Nest,predict(chick.068,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(chick))
xvs=sample(xvs)
chick.068.xval=rep(0,length(nrow(chick)))
for(i in 1:10){
  train=chick[xvs!=i,]
  test=chick[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.068))
  chick.068.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(chick$Nest,round(chick.068.xval))
class.sum(chick$Nest,chick.068.xval)

#random forest for chick

data=chick
xval.class=rep(0,length=nrow(data))
xval.prob=rep(0,length=nrow(data))
xvs=rep(1:10,length=nrow(data))
xvs=sample(xvs)
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  glub=randomForest(as.factor(Nest)~ . , data=train)
  xval.class[xvs==i]=predict(glub,test,type="response")
  xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
}
table(data$Nest,xval.class)
class.sum(data$Nest,xval.prob)

#fit pruned tree for flick
flick.1=rpart(Nest~., method="class", data=flick, control=rpart.control(cp=0.1, minsplit=2)) #if method not specified regression tree is default
plot(flick.1, margin=0.1)
text(flick.1, use.n=TRUE)

table(flick$Nest,predict(flick.1,type="class"))
class.sum(flick$Nest,predict(flick.1,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(flick))
xvs=sample(xvs)
flick.1.xval=rep(0,length(nrow(flick)))
for(i in 1:10){
  train=flick[xvs!=i,]
  test=flick[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.1))
  flick.1.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(flick$Nest,round(flick.1.xval))
class.sum(flick$Nest,flick.1.xval)

#random forest for flick
data=flick
xval.class=rep(0,length=nrow(data))
xval.prob=rep(0,length=nrow(data))
xvs=rep(1:10,length=nrow(data))
xvs=sample(xvs)
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  glub=randomForest(as.factor(Nest)~ . , data=train)
  xval.class[xvs==i]=predict(glub,test,type="response")
  xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
}

table(data$Nest,round(flick.1.xval))
class.sum(data$Nest,flick.1.xval)

#fit pruned tree for sap cp=0.23
sap.23=rpart(Nest~., method="class", data=sap, control=rpart.control(cp=0.23, minsplit=2)) #if method not specified regression tree is default
plot(sap.23, margin=0.1)
text(sap.23, use.n=TRUE)

table(sap$Nest,predict(sap.23,type="class"))
class.sum(sap$Nest,predict(sap.23,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(sap))
xvs=sample(xvs)
sap.23.xval=rep(0,length(nrow(sap)))
for(i in 1:10){
  train=sap[xvs!=i,]
  test=sap[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.23))
  sap.23.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(sap$Nest,round(sap.23.xval))
class.sum(sap$Nest,sap.23.xval)

#fit pruned tree for sap cp=0.1
sap.1=rpart(Nest~., method="class", data=sap, control=rpart.control(cp=0.1, minsplit=2)) #if method not specified regression tree is default
plot(sap.1, margin=0.1)
text(sap.1, use.n=TRUE)

table(sap$Nest,predict(sap.1,type="class"))
class.sum(sap$Nest,predict(sap.1,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(sap))
xvs=sample(xvs)
sap.1.xval=rep(0,length(nrow(sap)))
for(i in 1:10){
  train=sap[xvs!=i,]
  test=sap[xvs==i,]
  rp=rpart(Nest~ . ,method="class",data=train,control=rpart.control(cp=0.1))
  sap.1.xval[xvs==i]=predict(rp,test,type="prob")[,2]
}

table(sap$Nest,round(sap.1.xval))
class.sum(sap$Nest,sap.1.xval)

#random forests
sap.rf=randomForest(as.factor(Nest)~., method="class", data=sap, imporance=T)
sap.rf$confusion
sap.rf.confusion = table(sap$Nest, predict(sap.rf, type="response"))
100 * sum(diag(sap.rf.confusion))/sum(sap.rf.confusion)
class.sum(lichenLO$LobaOreg,predict(Loreg.rf,type="prob")[,2])

data=sap
xval.class=rep(0,length=nrow(data))
xval.prob=rep(0,length=nrow(data))
xvs=rep(1:10,length=nrow(data))
xvs=sample(xvs)
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  glub=randomForest(as.factor(Nest)~ . , data=train)
  xval.class[xvs==i]=predict(glub,test,type="response")
  xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
}

table(data$Nest,xval.class)
class.sum(data$Nest,xval.prob)

# Use species as the response variable ------------------------------------

spp.rf=randomForest(as.factor(Species)~ . ,data=spp,importance=TRUE)

spp.rf$confusion

spp.rf.confusion=table(spp2$Species,predict(spp.rf,type="response"))
100*sum(diag(spp.rf.confusion))/sum(spp.rf.confusion)

varImpPlot(spp.rf,scale=FALSE)