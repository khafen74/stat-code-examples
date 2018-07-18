# Libraries ---------------------------------------------------------------

library(MASS)

# Load data ---------------------------------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "SNRA.csv"
snra = read.csv(fn)


# LDA ---------------------------------------------------------------------

snra.lda = lda(Type~Blue+Green+Red+NearInfrared+Elevation+NDVI, data=snra) #lda model
table(snra$Type,predict(snra.lda)$class)
100-100*sum(diag(table(snra$Type,predict(snra.lda)$class)))/699 #error/misclassification rate
