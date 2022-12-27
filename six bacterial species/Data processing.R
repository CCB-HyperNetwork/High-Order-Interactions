setwd("~/Documents/sci_data")


library(ggalt)
library("deSolve")
library("mvtnorm")
library("parallel")
library("ggplot2")


three_123 <- read.csv("123.csv",header=T,check.names=F)
three_456 <- read.csv("456.csv",header=T,check.names=F)
six_123456 <- read.csv("123456.csv",header=T,check.names=F)

three_123 <- three_123[,-1]
three_456 <- three_456[,-1]
six_123456 <- six_123456[,-1]


data1 <- matrix(NA,ncol = 11,nrow = 4)
data1[1,]<- as.numeric(three_123[1,])
data1[2,]<- as.numeric(three_123[2,])
data1[3,]<- as.numeric(three_123[3,])
data1 <- log(data1+1)
data1[4,]<- data1[2,]+data1[3,]


data1 <- data1-data1[,1]
data1 <- data1+abs(min(data1))

t <- as.numeric(colnames(three_123))
times <- t


data2 <- matrix(NA,ncol = 11,nrow = 4)
data2[1,]<- as.numeric(three_456[1,])
data2[2,]<- as.numeric(three_456[2,])
data2[3,]<- as.numeric(three_456[3,])
data2 <- log(data2+1)
data2[4,]<- data2[2,]+data2[3,]

data2 <- data2-data2[,1]
data2 <- data2+abs(min(data2))

data31 <- matrix(NA,ncol = 11,nrow = 16)
data31[1,]<- as.numeric(six_123456[1,])
data31[2,]<- as.numeric(six_123456[2,])
data31[3,]<- as.numeric(six_123456[3,])
data31[4,]<- as.numeric(six_123456[4,])
data31[5,]<- as.numeric(six_123456[5,])
data31[6,]<- as.numeric(six_123456[6,])
data31 <- log(data31+1)
data31[7,]<- data31[2,]+data31[3,]
data31[8,]<- data31[2,]+data31[4,]
data31[9,]<- data31[2,]+data31[5,]
data31[10,]<-data31[2,]+data31[6,] 
data31[11,]<- data31[3,]+data31[4,]
data31[12,]<- data31[3,]+data31[5,]
data31[13,]<- data31[3,]+data31[6,]
data31[14,]<- data31[4,]+data31[5,]
data31[15,]<- data31[4,]+data31[6,]
data31[16,]<- data31[5,]+data31[6,]

data31 <- data31-data31[,1]
data31 <- data31+abs(min(data31))


rownames(data31) <- c("1","2","3","4","5","6","23","24","25","26","34","35","36","45","46","56")
colnames(data31) <- t
