library(rpart)
library(verification)

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "SNRA.csv"
snra = read.csv(fn)

#fit full tree first
snra.rpartfull=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
  control=rpart.control(cp=0.0, minsplit=2),data=snra)
plotcp(snra.rpartfull)

#se rule would give cp about 0.0026, but levels off around 0.014, so we're going use that
snra.cp014=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
  control=rpart.control(cp=0.014, minsplit=2),data=snra)
plot(snra.cp014,margin=0.1)
text(snra.cp014)

#resubstitution correction rate
snra.cp014confuse=table(snra$Type,predict(snra.cp014,type="class"))
snra.cp014confuse
100*sum(diag(snra.cp014confuse))/699

#10-fold cross validation error rate
snra.cp014.xval=rep(0,nrow(snra))
xvs=rep(1:10,length=nrow(snra))
xvs=sample(xvs)
for(i in 1:10){
    test=snra[xvs==i,]
    train=snra[xvs!=i,]
    glub=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
      control=rpart.control(cp=0.014, minsplit=2),data=train)
    snra.cp014.xval[xvs==i]=predict(glub,test,type="class")
}

snra.cp014confuse.xval=table(snra$Type,snra.cp014.xval)
snra.cp014confuse.xval
100*sum(diag(snra.cp014confuse.xval))/699


#change cp (cost-complexity) value to produce a differnt tree (mininum cp at 0.0013)
snra.cp0045=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
  control=rpart.control(cp=0.0045, minsplit=2),data=snra)
plot(snra.cp0045,margin=0.1)
text(snra.cp0045)

#resubstitution error rate
snra.cp0045confuse=table(snra$Type,predict(snra.cp0045,type="class"))
snra.cp0045confuse
100*sum(diag(snra.cp0045confuse))/699

#10-fold cross validated error rate
snra.cp0045.xval=rep(0,nrow(snra))
xvs=rep(1:10,length=nrow(snra))
xvs=sample(xvs)
for(i in 1:10){
    test=snra[xvs==i,]
    train=snra[xvs!=i,]
    glub=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
control=rpart.control(cp=0.0045, minsplit=2),data=train)
    snra.cp0045.xval[xvs==i]=predict(glub,test,type="class")
}
snra.cp0045confuse.xval=table(snra$Type,snra.cp0045.xval)
snra.cp0045confuse.xval
100*sum(diag(snra.cp0045confuse.xval))/699



#fit another tree with the lowest cp value
snra.cp0013=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
  control=rpart.control(cp=0.0013, minsplit=2),data=snra)
plot(snra.cp0013)

#resubstitution error rate
snra.cp0013confuse=table(snra$Type,predict(snra.cp0013,type="class")
snra.cp0013confuse
100*sum(diag(snra.cp0013confuse))/699

#cross validated error rate (turns out similar to the smaller tree above at cp=0.0045)
snra.cp0013.xval=rep(0,nrow(snra))
xvs=rep(1:10,length=nrow(snra))
xvs=sample(xvs)
for(i in 1:10){
    test=snra[xvs==i,]
    train=snra[xvs!=i,]
    glub=rpart(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI+SoilBrightness+Greenness+Yellowness+NoneSuch,
control=rpart.control(cp=0.0013, minsplit=2),data=train)
    snra.cp0013.xval[xvs==i]=predict(glub,test,type="class")
}
snra.cp0013confuse.xval=table(snra$Type,snra.cp0013.xval)
snra.cp0013confuse.xval
100*sum(diag(snra.cp0013confuse.xval))/699
