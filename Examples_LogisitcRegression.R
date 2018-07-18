# Load data ---------------------------------------------------------------

rm(list = ls())
setwd("C:\\Users\\konrad\\Desktop\\Classes\\STAT_5810_DataMiningStatLearning\\Data")
fn = "LAQI.csv"
laqi = read.csv(fn)
fn2 = "pilotI.csv"
pilotI = read.csv(fn2)


# Libraries ---------------------------------------------------------------

library(MASS)
library(verification)
