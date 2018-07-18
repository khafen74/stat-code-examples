
# Libraries ---------------------------------------------------------------

library(rpart)
library(randomForest)


# Read data ---------------------------------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "Glass.csv"
glass = read.csv(fn)


# Fit classification tree using 1 SE rule ---------------------------------

glass.full=rpart(as.factor(GlassType)~., method="class", data=glass, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plot(glass.full)
plotcp(glass.full)


# Refit tree --------------------------------------------------------------

# cp = 0.018, maybe try 0.027
glass.018=rpart(as.factor(GlassType)~., method="class", data=glass, control=rpart.control(cp=0.018, minsplit=2)) #if method not specified regression tree is default
plot(glass.018, margin=0.1)
text(glass.018, use.n=TRUE)

glass.027=rpart(as.factor(GlassType)~., method="class", data=glass, control=rpart.control(cp=0.027, minsplit=2)) #if method not specified regression tree is default
plot(glass.027, margin=0.1)
text(glass.027, use.n=TRUE)

data=glass
cp = 0.018
xvs=rep(c(1:10),length=nrow(data))
xvs=sample(xvs)
xval.prob=rep(0,length(nrow(data)))
xval.class=rep(0,length(nrow(data)))
for(i in 1:10){
  train=data[xvs!=i,]
  test=data[xvs==i,]
  rp=rpart(GlassType~ . ,method="class",data=train,control=rpart.control(cp=cp))
  xval.prob[xvs==i]=predict(rp,test,type="prob")[,2]
  xval.class[xvs==i]=predict(rp,test,type="class")
}

confusion = table(data$GlassType,round(xval.class))
confusion
100*sum(diag(confusion))/sum(confusion)
class.sum(data$GlassType,xval)


# Apply random forests to glass data --------------------------------------

glass.rf=randomForest(as.factor(GlassType)~ . ,data=glass,importance=TRUE)

glass.rf$confusion

glass.rf.confusion=table(glass$GlassType,predict(glass.rf,type="response"))
100*sum(diag(glass.rf.confusion))/sum(glass.rf.confusion)

varImpPlot(glass.rf,scale=FALSE)

glass.sub.rf = randomForest(as.factor(GlassType)~Magnesium+Refindex+Aluminum+Calcium+Barium,
                            data=glass, importance = T)
glass.sub.rf$confusion

glass.sub.rf.confusion=table(glass$GlassType,predict(glass.sub.rf,type="response"))
100*sum(diag(glass.sub.rf.confusion))/sum(glass.sub.rf.confusion)
