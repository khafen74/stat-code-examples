
# No data for this section, example code ----------------------------------


#classification trees

data = 

library(rpart)
xyrpart = rpart(y~x, data=data)
plot(xyrpart, margin=0.1)
text(xyrpart, us.n=TRUE)
summary(xyrpart)

#4 part tree
xyrpart4nodes = rpart(y~x, data=data, control=rpart.control(cp=0.0)) #cp=0.0, keep splitting, minbucket=number of observations to split (or maybe minsplit)

#fit fully grown tree
xypartfull = rpart(y~x, data=data, control=rpart.control(cp=0.0, minsplit=2))
plot(cyrpartfull)

#plot to use 1-SE rule to identify tree
plotcp(xyrpartfull) #cp value should be aroun 0.11 for this dataset

#use cp value from plot to fit final tree
syrpartfinal = rpart(y~x, data=data, control=rpart.control(cp=0.11, minsplit=2))


# Use lichen data ---------------------------------------------------------

library(verification)
library(rpart)

# Load data ---------------------------------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "LAQI.csv"
laqi = read.csv(fn) #should subset data to not include response for other species, thats why there are issues below
fn2 = "pilotI.csv"
pilotI = read.csv(fn2)
vars = c("LobaOreg","TransAspect","Elevation","Slope","ACONIF","PctConifCov","DegreeDays","EvapoTransAve",
         "EvapoTransDiff","MoistIndexAve","MoistIndexDiff")
laqi = laqi[vars]


# Classification tree -----------------------------------------------------

loreg.rpartfull=rpart(LobaOreg~., method="class", data=laqi, control=rpart.control(cp=0.0, minsplit=2)) #if method not specified regression tree is default
plot(loreg.rpartfull)
plotcp(loreg.rpartfull)


# Refit tree --------------------------------------------------------------

loreg.rpart059=rpart(LobaOreg~., method="class", data=laqi, control=rpart.control(cp=0.059, minsplit=2)) #if method not specified regression tree is default
plot(loreg.rpart059, margin=0.1) #this plot doesn't match the one showed in class, variables should be elevation, ageconif, tempdiff
text(loreg.rpart059, use.n=TRUE)


