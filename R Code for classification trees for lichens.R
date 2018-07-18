library(rpart)
library(verification)


Loreg.rpartfull=rpart(LobaOreg~ . ,method="class", control=rpart.control(cp=0.0,minsplit=2),data=lichenLO)
plot(Loreg.rpartfull)
plotcp(Loreg.rpartfull)

Loreg.rpartcp059=rpart(LobaOreg~ . ,method="class",data=lichenLO,control=rpart.control(cp=0.059))
plot(Loreg.rpartcp059,margin=0.1)
text(Loreg.rpartcp059,use.n=TRUE)
Loreg.rpartcp059

table(lichenLO$LobaOreg,predict(Loreg.rpartcp059,type="class"))
class.sum(lichenLO$LobaOreg,predict(Loreg.rpartcp059,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(lichenLO))
xvs=sample(xvs)
Loreg.rpartcp059.xval=rep(0,length(nrow(lichenLO)))
for(i in 1:10){
	train=lichenLO[xvs!=i,]
	test=lichenLO[xvs==i,]
	rp=rpart(LobaOreg~ . ,method="class",data=train,control=rpart.control(cp=0.059))
	Loreg.rpartcp059.xval[xvs==i]=predict(rp,test,type="prob")[,2]
	}

table(lichenLO$LobaOreg,round(Loreg.rpartcp059.xval))
class.sum(lichenLO$LobaOreg,Loreg.rpartcp059.xval)

table(pilotI$LobaOreg,predict(Loreg.rpartcp059,pilotI,type="class"))
class.sum(pilotI$LobaOreg,predict(Loreg.rpartcp059,pilotI,type="prob")[,2])




Loreg.rpartcp031=rpart(LobaOreg~ . ,method="class",data=lichenLO,control=rpart.control(cp=0.031))
plot(Loreg.rpartcp031,margin=0.1)
text(Loreg.rpartcp031,use.n=TRUE)


table(lichenLO$LobaOreg,predict(Loreg.rpartcp031,type="class"))
class.sum(lichenLO$LobaOreg,predict(Loreg.rpartcp031,type="prob")[,2])

xvs=rep(c(1:10),length=nrow(lichenLO))
xvs=sample(xvs)
Loreg.rpartcp031.xval=rep(0,length(nrow(lichenLO)))
for(i in 1:10){
	train=lichenLO[xvs!=i,]
	test=lichenLO[xvs==i,]
	rp=rpart(LobaOreg~ . ,method="class",data=train,control=rpart.control(cp=0.031))
	Loreg.rpartcp031.xval[xvs==i]=predict(rp,test,type="prob")[,2]
	}

table(lichenLO$LobaOreg,round(Loreg.rpartcp031.xval))
class.sum(lichenLO$LobaOreg,Loreg.rpartcp031.xval)

table(pilotI$LobaOreg,predict(Loreg.rpartcp031,pilotI,type="class"))
class.sum(pilotI$LobaOreg,predict(Loreg.rpartcp031,pilotI,type="prob")[,2])











