
library(MASS)

###############
####  LDA  ####
###############

snra.lda=lda(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI,data=snra)
table(snra$Type,predict(snra.lda)$class)
100-100*sum(diag(table(snra$Type,predict(snra.lda)$class)))/699


snra.lda.xval=rep(0,nrow(snra))
xvs=rep(1:10,length=nrow(snra))
xvs=sample(xvs)
for(i in 1:10){
    test=snra[xvs==i,]
    train=snra[xvs!=i,]
    glub=lda(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI,data=train)
    snra.lda.xval[xvs==i]=predict(glub,test)$class
}
table(snra$Type,snra.lda.xval)
100-100*sum(diag(table(snra$Type,snra.lda.xval)))/699




###############
####  QDA  ####
###############

snra.qda=qda(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI,data=snra)
table(snra$Type,predict(snra.qda)$class)
100-100*sum(diag(table(snra$Type,predict(snra.qda)$class)))/699


snra.qda.xval=rep(0,nrow(snra))
xvs=rep(1:10,length=nrow(snra))
xvs=sample(xvs)
for(i in 1:10){
    test=snra[xvs==i,]
    train=snra[xvs!=i,]
    glub=qda(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI,data=train)
    snra.qda.xval[xvs==i]=predict(glub,test)$class
}
table(snra$Type,snra.qda.xval)
100-100*sum(diag(table(snra$Type,snra.qda.xval)))/699



