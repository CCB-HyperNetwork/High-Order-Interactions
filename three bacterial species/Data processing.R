setwd("~/Documents/ori_data")

library(ggalt)
library("deSolve")
library("mvtnorm")
library("parallel")
library("ggplot2")

mo_E <- read.csv("mo_E.csv",header=T,check.names=F)
mo_P <- read.csv("mo_P.csv",header=T,check.names=F)
mo_S <- read.csv("mo_S.csv",header=T,check.names=F)
EP_E <- read.csv("EP_E.csv",header=T,check.names=F)
EP_P <- read.csv("EP_P.csv",header=T,check.names=F)
ES_E <- read.csv("ES_E.csv",header=T,check.names=F)
ES_S <- read.csv("ES_S.csv",header=T,check.names=F)
SP_P <- read.csv("SP_P.csv",header=T,check.names=F)
SP_S <- read.csv("SP_S.csv",header=T,check.names=F)
ESP_E <- read.csv("ESP_E.csv",header=T,check.names=F)
ESP_P <- read.csv("ESP_P.csv",header=T,check.names=F)
ESP_S <- read.csv("ESP_S.csv",header=T,check.names=F)

data <- matrix(NA,ncol = 14,nrow = 12)
data[1,] <- as.numeric(mo_E[57,3:16])
data[2,] <- as.numeric(mo_P[57,3:16])
data[3,] <- as.numeric(mo_S[57,3:16])
data[4,] <- as.numeric(EP_E[57,3:16])
data[5,] <- as.numeric(EP_P[57,3:16])
data[6,] <- as.numeric(ES_E[57,3:16])
data[7,] <- as.numeric(ES_S[57,3:16])
data[8,] <- as.numeric(SP_P[57,3:16])
data[9,] <- as.numeric(SP_S[57,3:16])
data[10,] <- as.numeric(ESP_E[57,3:16])
data[11,] <- as.numeric(ESP_P[57,3:16])
data[12,] <- as.numeric(ESP_S[57,3:16])


data <- log(data)


t <- c(0.5,1,1.5,2,4,6,8,10,12,16,20,24,30,36)

data <- data-data[,1]
data <- data+abs(min(data))


rownames(data) <- c("mo_E","mo_P","mo_S","EP_E","EP_P","ES_E","ES_S","SP_P","SP_S","ESP_E","ESP_P","ESP_S")

times <- t

data_three1 <- matrix(NA,ncol = 14,nrow = 7)
data_three1[1,] <- data[10,]
data_three1[2,] <- data[11,]
data_three1[3,] <- data[12,]
data_three1[4,] <- data[11,]+data[12,]
data_three1[5,] <- 1/(data[11,]+data[12,])
data_three1[6,] <- data[11,]-data[12,]
data_three1[7,] <- 1-(data[11,]-data[12,])
