
library(MASS)
library(verification)

###############
####  LDA  ####
###############

lichenLO=subset(laqi,select=c(LobaOreg,TransAspect,Elevation,Slope,ACONIF,PctConifCov,DegreeDays,
                EvapoTransAve,EvapoTransDiff,MoistIndexAve,MoistIndexDiff,PrecipAve,PrecipDiff,
                RelHumidAve,RelHumidDiff,TempAve,TempDiff,VapPressAve,VapPressDiff,PotGlobRadAve,
                PotGlobRadDiff))

Loreg.lda=lda(LobaOreg~ . ,data=lichenLO)
table(lichenLO$LobaOreg,predict(Loreg.lda)$class)
class.sum(lichenLO$LobaOreg,predict(Loreg.lda)$posterior[,2])

Loreg.cvlda=lda(LobaOreg~ . ,CV=TRUE,data=lichenLO)
table(lichenLO$LobaOreg,Loreg.cvlda$class)
class.sum(lichenLO$LobaOreg,Loreg.cvlda$posterior[,2])


Loreg.lda.xval.class=rep(0,nrow(lichenLO))
Loreg.lda.xval.posterior=rep(0,nrow(lichenLO))

xvs=rep(1:10,length=nrow(lichenLO))
xvs=sample(xvs)
for(i in 1:10){
    train=lichenLO[xvs!=i,]
    test=lichenLO[xvs==i,]
    glub=lda(LobaOreg~ . ,data=train)
    Loreg.lda.xval.posterior[xvs==i]=predict(glub,test)$posterior[,2]
    Loreg.lda.xval.class[xvs==i]=predict(glub,test)$class
}
table(lichenLO$LobaOreg,Loreg.lda.xval.class)
class.sum(lichenLO$LobaOreg,Loreg.lda.xval.posterior)



###############
####  QDA  ####
###############

Loreg.qda=qda(LobaOreg~ . ,data=lichenLO)
table(lichenLO$LobaOreg,predict(Loreg.qda)$class)
class.sum(lichenLO$LobaOreg,predict(Loreg.qda)$posterior[,2])

Loreg.cvqda=qda(LobaOreg~ . ,CV=TRUE,data=lichenLO)
table(lichenLO$LobaOreg,Loreg.cvqda$class)
class.sum(lichenLO$LobaOreg,Loreg.cvqda$posterior[,2])


Loreg.qda.xval.class=rep(0,nrow(lichenLO))
Loreg.qda.xval.posterior=rep(0,nrow(lichenLO))

xvs=rep(1:10,length=nrow(lichenLO))
xvs=sample(xvs)
for(i in 1:10){
    train=lichenLO[xvs!=i,]
    test=lichenLO[xvs==i,]
    glub=qda(LobaOreg~ . ,data=train)
    Loreg.qda.xval.posterior[xvs==i]=predict(glub,test)$posterior[,2]
    Loreg.qda.xval.class[xvs==i]=predict(glub,test)$class
}
table(lichenLO$LobaOreg,Loreg.qda.xval.class)
class.sum(lichenLO$LobaOreg,Loreg.qda.xval.posterior)


###############################
####  Logistic Regression  ####
###############################

#with resubstitution
Loreg.lr = glm(LobaOreg~ . ,family=binomial,data=lichenLO)
table(lichenLO$LobaOreg,round(predict(Loreg.lr,type="response")))
class.sum(lichenLO$LobaOreg,predict(Loreg.lr,type="response"))

#with 10-fold crossvalidation
Loreg.lr.xval=rep(0,nrow(lichenLO))
xvs=rep(1:10,length=nrow(lichenLO))
xvs=sample(xvs)
for(i in 1:10){
    train=lichenLO[xvs!=i,]
    test=lichenLO[xvs==i,]
    glub=glm(LobaOreg~ . ,family=binomial,data=train)
    Loreg.lr.xval[xvs==i]=predict(glub,test,type="response")
}
table(lichenLO$LobaOreg,round(Loreg.lr.xval))
class.sum(lichenLO$LobaOreg,Loreg.lr.xval)

#predict onto new dataset
Loreg.lr.pilotI = predict(Loreg.lr,pilotI,type="response")
table(pilotI$LobaOreg,round(Loreg.lr.pilotI))
class.sum(pilotI$LobaOreg,Loreg.lr.pilotI)


#stepwise? selection
Loreg.lr12=step(Loreg.lr)

table(lichenLO$LobaOreg,round(predict(Loreg.lr12,type="response")))
class.sum(lichenLO$LobaOreg,predict(Loreg.lr12,type="response"))


Loreg.lr12.xval=rep(0,nrow(lichenLO))
xvs=rep(1:10,length=nrow(lichenLO))
xvs=sample(xvs)
for(i in 1:10){
    train=lichenLO[xvs!=i,]
    test=lichenLO[xvs==i,]
    glub=glm(LobaOreg~ . ,family=binomial,data=train)
    Loreg.lr12.xval[xvs==i]=predict(glub,test,type="response")
}
table(lichenLO$LobaOreg,round(Loreg.lr12.xval))
class.sum(lichenLO$LobaOreg,Loreg.lr12.xval)


Loreg.lr12.pilotI = predict(Loreg.lr12,pilotI,type="response")
table(pilotI$LobaOreg,round(Loreg.lr12.pilotI))
class.sum(pilotI$LobaOreg,Loreg.lr12.pilotI)
