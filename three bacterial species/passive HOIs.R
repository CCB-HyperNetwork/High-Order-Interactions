Legendre.model11 <- function(t, np.order,tmin = NULL, tmax = NULL)
{
  u <- -1;
  v <- 1;
  if (is.null(tmin))
    tmin <- min(t);
  if (is.null(tmax))
    tmax <- max(t);
  ti    <- u + ((v - u) * (t - tmin)) / (tmax - tmin);
  L <- rep(NA,np.order)
  L[1] <- 1;
  if (np.order >= 2)
    L[2] <- 0.5 * (6 * ti) 
  if (np.order >= 3)
    L[3] <- 0.5 * (15 * ti ^ 2 - 3) 
  if (np.order >= 4)
    L[4] <-  0.125 * (35 * 4 * ti ^ 3 - 60 * ti) 
  if (np.order >= 5)
    L[5] <-  0.125 * (63 * 5 * ti ^ 4 - 210 * ti ^ 2 + 15)
  if (np.order >= 6)
    L[6] <-(1 / 16) * (231 * 6 * ti ^ 5 - 315 * 4 * ti ^ 3 + 105 * 2 * ti) 
  if (np.order >= 7)
    L[7] <- (1 / 16) * (429 * 7 * ti ^ 6 - 693 * 5 * ti ^ 4 + 315 * 3 *ti ^ 2 - 35)
  if (np.order>=8)
    L[8] <-  (1/128) * (6435 * 8 * ti ^ 7 - 12012 * 6 * ti ^ 5 + 6930 * 4 * ti ^ 3 - 1260 * 2 * ti)
  if (np.order>=9)
    L[9] <-  (1/128) * (12155 * 9 * ti ^ 8 - 25740 * 7 * ti ^ 6 + 18018 * 5 * ti ^ 4 - 4620 * 3 * ti ^ 2 + 315)
  if (np.order>=10)
    L[10] <-  (1/256) * (46189 * 10 * ti ^ 9 - 109395 * 8 * ti ^ 7 + 90090 * 6 * ti ^ 5 - 30030 * 4 * ti ^ 3 + 3465 * 2 * ti)
  if (np.order>=11)
    L[11] <-  (1/512) * (176358 * 11 * ti ^ 10 - 461890 * 9 * ti ^ 8 + 437580 * 7 * ti ^ 6 - 180180 * 5 * ti ^ 4 + 30030 * 3 * ti^ 2 - 1386)
  if (np.order>=12)
    L[12] <-  660.1943 * 12 * ti ^ 11 -1894.471 * 10 * ti ^ 9 + 2029.79 * 8 * ti ^ 7  - 997.0898 * 6 * ti ^ 5 + 219.9463 * 4 * ti^ 3 - 17.5957* 2 * ti
  if (np.order>=13)
    L[13] <-  1269.604 * 13 * ti ^ 12 -3961.166 * 11 * ti ^ 10 + 4736.177 * 9 * ti ^ 8  - 2706.387 * 7 * ti ^ 6 + 747.8174 * 5 * ti^ 4 - 87.97852* 3 * ti^ 2 + 2.932617
  if (np.order>=14)
    L[14] <-  2448.523 * 14 * ti ^ 13 -8252.429 * 12 * ti ^ 11 + 10893.21 * 10 * ti ^ 9  - 7104.265 * 8 * ti ^ 7 + 2368.088 * 6 * ti^ 5 - 373.9087* 4 * ti^ 3 + 21.99463* 2 * ti 
  if (np.order>=15)
    L[15] <-  4733.811 * 15 * ti ^ 14 -17139.66 * 13 * ti ^ 12 + 24757.29 * 11 * ti ^ 10  - 18155.34 * 9 * ti ^ 8 + 7104.265 * 7 * ti^ 6 - 1420.853* 5 * ti^ 4 + 124.6362* 3 * ti ^ 2 - 3.14209
  if (np.order>=16)
    L[16] <-  9171.759 * 16 * ti ^ 15 -35503.58 * 14 * ti ^ 13 + 55703.9 * 12 * ti ^ 11  - 45388.36 * 10 * ti ^ 9 + 20424.76 * 8 * ti^ 7 - 4972.986* 6 * ti^ 5 + 592.0221* 4 * ti ^ 3 - 26.70776* 2 * ti 
  if (np.order>=17)
    L[17] <-  17804 * 17 * ti ^ 16 -73374.07 * 15 * ti ^ 14 + 124262.5 * 13 * ti ^ 12  - 111407.8 * 11 * ti ^ 10 + 56735.45 * 9 * ti^ 8 - 16339.81* 7 * ti^ 6 + 2486.493* 5 * ti ^ 4 - 169.1492* 3 * ti ^ 2 +3.33847
  if (np.order>=18)
    L[18] <-  34618.89 * 18 * ti ^ 17 -151334 * 16 * ti ^ 15 + 275152.8 * 14 * ti ^ 13  - 269235.5 * 12 * ti ^ 11 + 153185.7 * 10 * ti^ 9 - 51061.91* 8 * ti^ 7 + 9531.556* 6 * ti ^ 5 - 888.0331* 4 * ti ^ 3 +31.71547* 2 * ti 
  if (np.order>=19)
    L[19] <-  67415.74 * 19 * ti ^ 18 -311570 * 17 * ti ^ 16 + 605336.1 * 15 * ti ^ 14  - 642023.1 * 13 * ti ^ 12 + 403853.3 * 11 * ti^ 10 - 153185.7* 9 * ti^ 8 + 34041.27* 7 * ti ^ 6 - 4084.952* 5 * ti ^ 4 +222.0083* 3 * ti ^ 2 -3.523941
  if (np.order>=20)
    L[20] <-  131460.7 * 20 * ti ^ 19 -640449.5 * 18 * ti ^ 17 + 1324173 * 16 * ti ^ 15  - 1513340 * 14 * ti ^ 13 + 1043288 * 12 * ti^ 11 - 444238.6* 10 * ti^ 9 + 114889.3* 8 * ti ^ 7 - 17020.64* 6 * ti ^ 5 +1276.548* 4 * ti ^ 3 -37.00138* 2 * ti 
  if (np.order>=21)
    L[21] <-  256661.4 * 21 * ti ^ 20 -1314607 * 19 * ti ^ 18 + 2882023 * 17 * ti ^ 16  - 3531127 * 15 * ti ^ 14 + 2648345 * 13 * ti^ 12 - 1251945* 11 * ti^ 10 + 370198.8* 9 * ti ^ 8 - 65651.02* 7 * ti ^ 6 +6382.738* 5 * ti ^ 4 -283.6773* 3 * ti^ 2 + 3.700138 
  if (np.order>=22)
    L[22] <-  501656.3 * 22 * ti ^ 21 -2694944 * 20 * ti ^ 19 + 6244383 * 18 * ti ^ 17  - 8165732 * 16 * ti ^ 15 + 6620863 * 14 * ti^ 13 - 3442849* 12 * ti^ 11 + 1147616* 10 * ti ^ 9 - 237985* 8 * ti ^ 7 +28722.32* 6 * ti ^ 5 -1772.983* 4 * ti^ 3 + 42.55159* 2 * ti
  if (np.order>=23)
    L[23] <-  981501.4 * 23 * ti ^ 22 -5518219 * 21 * ti ^ 20 + 13474721 * 19 * ti ^ 18  - 18733149 * 17 * ti ^ 16 + 16331463 * 15 * ti^ 14 - 9269209* 13 * ti^ 12 + 3442849* 11 * ti ^ 10 - 819725.9* 9 * ti ^ 8 +118992.5* 7 * ti ^ 6 -9574.107* 5 * ti^ 4 + 354.5966* 3 * ti^ 2 -3.868326
  if (np.order>=24)
    L[24] <-  1922107 * 24 * ti ^ 23 -11287266 * 22 * ti ^ 21 + 28970650 * 20 * ti ^ 19  - 42669950 * 18 * ti ^ 17 + 39807941 * 16 * ti^ 15 - 24497195* 14 * ti^ 13 + 10041643* 12 * ti ^ 11 - 2705096* 10 * ti ^ 9 +461095.8* 8 * ti ^ 7 -46274.85* 6 * ti^ 5 + 2393.527* 4 * ti^ 3 -48.35408* 2 * ti
  if (np.order>=25)
    L[25] <-  3767330 * 25 * ti ^ 24 -23065284 * 23 * ti ^ 22 + 62079965 * 21 * ti ^ 20  - 96568835 * 19 * ti ^ 18 + 96007388 * 17 * ti^ 16 - 63692706* 15 * ti^ 14 + 28580061* 13 * ti ^ 12 - 8607122* 11 * ti ^ 10 +1690685* 9 * ti ^ 8 -204931.5* 7 * ti^ 6 + 13882.46* 5 * ti^ 4 -435.1867* 3 * ti ^ 2 + 4.029506
  if (np.order>=26)
    L[26] <-  7389762 * 26 * ti ^ 25 -47091621 * 24 * ti ^ 23 + 132625380 * 22 * ti ^ 21  - 217279879 * 20 * ti ^ 19 + 229350983 * 18 * ti^ 17 - 163212560* 16 * ti^ 15 + 79615883* 14 * ti ^ 13 - 26538628* 12 * ti ^ 11 +5917397* 10 * ti ^ 9 -845342.4* 8 * ti^ 7 + 71726.02* 6 * ti^ 5 -3155.104* 4 * ti ^ 3 + 54.39834* 2 * ti
  
  return(L);
}

LMall <- function(NX,nt,nstep=30,order){
  
  stp <- (max(nt)-min(nt))/nstep
  res <- c()
  for(j in 1:nstep){
    
    tg1 <- Legendre.model11((j-1)*stp+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg2 <- Legendre.model11(j*stp/2+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg3 <- Legendre.model11(j*stp/2+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tg4 <- Legendre.model11(j*stp+1,np.order=order-1,tmin=min(nt), tmax=max(nt))
    tmp1 <- rbind(tg1,tg2,tg3,tg4)
    res <- rbind(res,tmp1)
  }
  res
}
ode.sovle.ind <- function(effect,para,nt,order,nstep,LL,nconnect,times){
  stp <- (max(times)-min(times))/nstep
  index <- which(nconnect==1)
  
  ind.par <- matrix(para[1:(length(index)*(order-1))],ncol=order-1,byrow=T)
  allrep <- matrix(rep(0,length(index)),nrow=1)
  #allrep[1,1] <- effect[1,1]
  nn <- 1
  for(j in 1:nstep){
    tg1 <- (rowSums(t(apply(ind.par,1,"*",LL[nn,])))*effect[j,index])
    tg2 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+1,])))*effect[j,index])
    tg3 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+2,])))*effect[j,index])
    tg4 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+3,])))*effect[j,index])
    tmp <- allrep[j,] +stp*(tg1+2*tg2+2*tg3+tg4)/6
    allrep <- rbind(allrep,tmp)
    nn <- nn + 4
  }
  
  allrep
}


fitPKM <- function(para,effect,nconnect,nt,order,nstep,LL,times,y.c){
  
  odes <- ode.sovle.ind(effect,para,nt,order,nstep,LL,nconnect,times)
  sum((effect[,y.c]-(rowSums(odes)))^2
      #+1*sum(para^2)
  )
}

ode.optim <- function(H0_init_par,connect,effect,nstep,order,times,nt,y.c){
  
  LL <- LMall(NX=1,nt=nt,nstep=nstep,order=order)
  para=H0_init_par
  H0_par <- optim(para,fitPKM, times = times,effect=effect,nconnect=connect[y.c,],nt=nt, order=order,nstep=nstep,LL=LL,y.c=y.c, method = "Nelder-Mead")
  H0_par$par
  A <- ode.sovle.ind(effect,H0_par$par,nconnect=connect[y.c,],nt=nt,order=order,nstep=nstep,LL=LL,times)
  
  return(A)
}

data_EP <- matrix(NA,ncol = 14,nrow = 5)
data_EP[1,] <- out_E1[,2]
data_EP[2,] <- out_E2[,2]
data_EP[3,] <- out_E3[,2]
data_EP[4,] <- out_E4[,2]
data_EP[5,] <- data_three1[3,]
colnames(data_EP) <- t
rownames(data_EP) <- c("1","2","3","4","3")
lasso <- matrix(0,ncol = 5,nrow = 5)
diag(lasso) <- 1 
lasso[,5] <- 1


#H0_init_par <- c(0.10994003, -0.15629194 , 0.09313838,  -0.001671529, -0.012521805,  0.005611711)
#H0_init_par <- c(0.10087336 ,-0.13878588,  0.06050166,  -0.0005550065,  0.0052655673 ,-0.0006022072)
H0_init_par <- c(0.10087336 ,-0.13878588,  0.06050166, -0.001754410, -0.012625725,0.004970505)

out_EP1 <- ode.optim(H0_init_par,connect=lasso,effect=t(data_EP),nstep=13, order=4, times = times,nt=times,y.c=1)

