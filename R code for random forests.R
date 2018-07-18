library(randomForest)
library(verification)


#############################################
####  Lichen Analyses-- Lobaria oregana  ####
#############################################

Loreg.rf=randomForest(as.factor(LobaOreg)~ . ,data=lichenLO)

Loreg.rf$confusion

class.sum(lichenLO$LobaOreg,predict(Loreg.rf,type="prob")[,2])

Loreg.rf.xval.class=rep(0,length=nrow(lichenLO))
Loreg.rf.xval.prob=rep(0,length=nrow(lichenLO))
xvs=rep(1:10,length=nrow(lichenLO))
xvs=sample(xvs)
for(i in 1:10){
    train=lichenLO[xvs!=i,]
    test=lichenLO[xvs==i,]
    glub=randomForest(as.factor(LobaOreg)~ . , data=train)
    Loreg.rf.xval.class[xvs==i]=predict(glub,test,type="response")
    Loreg.rf.xval.prob[xvs==i]=predict(glub,test,type="prob")[,2]
    }

table(lichenLO$LobaOreg,Loreg.rf.xval.class)
class.sum(lichenLO$LobaOreg,Loreg.rf.xval.prob)

table(pilotI$LobaOreg,predict(Loreg.rf,pilotI,type="response"))
class.sum(pilotI$LobaOreg,predict(Loreg.rf,pilotI,type="prob")[,2])



#############################################
####  Now fitting a Classification Tree  ####
#############################################

plot(Loreg.cp031,margin=0.1)
text(Loreg.cp031,use.n=TRUE)

library(rpart)
Loreg.cp031=rpart(LobaOreg~ . ,data=lichenLO,method="class",control=rpart.control(cp=0.031,minsplit=2))

table(pilotI$LobaOreg,predict(Loreg.cp031,pilotI,type="class"))
class.sum(pilotI$LobaOreg,predict(Loreg.cp031,pilotI,type="prob")[,2])



#################################################
####  Variable Importance in Random Forests  ####
#################################################

Loreg.rf=randomForest(as.factor(LobaOreg)~ . ,importance=TRUE,data=lichenLO)
varImpPlot(Loreg.rf,scale=FALSE)



#####################################################
####  Parial Dependence Plots in Random Forests  ####
#####################################################

Loreg.rf=randomForest(as.factor(LobaOreg)~ . ,importance=TRUE,keep.forest=TRUE,data=lichenLO)
par(mfrow=c(2,2))
partialPlot(Loreg.rf,lichenLO,ACONIF,which.class=1)
partialPlot(Loreg.rf,lichenLO,Elevation,which.class=1)
partialPlot(Loreg.rf,lichenLO,TempAve,which.class=1)
partialPlot(Loreg.rf,lichenLO,PrecipAve,which.class=1)



#########################
####  SNRA Analyses  ####
#########################


snra2=subset(snra,select=c(Blue,Green,Red,NearInfrared,SoilBrightness,Greenness,Yellowness,NoneSuch,NDVI,Elevation,Type))

snra2.rf=randomForest(Type~ . ,data=snra2,importance=TRUE)

snra2.rf$confusion

snra2.rf.confusion=table(snra2$Type,predict(snra2.rf,type="response"))
100*sum(diag(snra2.rf.confusion))/sum(snra2.rf.confusion)

varImpPlot(snra2.rf,scale=FALSE)


snra2.rf2=randomForest(Type~ Elevation+NDVI+Yellowness+Greenness ,data=snra2)

snra2.rf2.confusion=table(snra2$Type,predict(snra2.rf2,type="response"))
100*sum(diag(snra2.rf2.confusion))/sum(snra2.rf2.confusion)



#################################################
####  Uintah Mountains Birds' Nest Analyses  ####
#################################################

nest456=read.csv("nest456.csv")
NoSpecies = subset(nest456,select = -Species)
NoNest = subset(nest456, select = -nest)

Sapsucker = droplevels(subset(nest456, Species=="Sapsucker" | Species=="Non-nest",select=-Species))
Chickadee = droplevels(subset(nest456, Species=="Chickadee" | Species=="Non-nest",select=-Species))
Flicker = droplevels(subset(nest456, Species=="Flicker" | Species=="Non-nest",select=-Species))

nest.rf=randomForest(as.factor(nest)~ . , importance=TRUE,keep.forest=TRUE,data=NoSpecies)
nest.rf$confusion
class.sum(NoSpecies$nest,predict(nest.rf,type="prob")[,2])
varImpPlot(nest.rf, scale=FALSE)

Chickadee.rf=randomForest(as.factor(nest)~. , importance=TRUE,data=Chickadee)
Chickadee.rf$confusion
class.sum(Chickadee$nest,predict(Chickadee.rf,type="prob")[,2])
varImpPlot(Chickadee.rf, scale=FALSE)

Flicker.rf=randomForest(as.factor(nest)~. , importance=TRUE,data=Flicker)
Flicker.rf$confusion
class.sum(Flicker$nest,predict(Flicker.rf,type="prob")[,2])
varImpPlot(Flicker.rf, scale=FALSE)

Sapsucker.rf=randomForest(as.factor(nest)~. , importance=TRUE,data=Sapsucker)
Sapsucker.rf$confusion
class.sum(Sapsucker$nest,predict(Sapsucker.rf,type="prob")[,2])
varImpPlot(Sapsucker.rf, scale=FALSE)
