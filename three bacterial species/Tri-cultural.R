ode.sovle.ind <- function(NG,para,nt,order,nstep,LL,A){
  stp <- (max(times)-min(times))/nstep
  index <- A
  
  ind.par <- matrix(para[1:(length(index)*(order-1))],ncol=order-1,byrow=T)
  allrep <- matrix(rep(0,length(index)),nrow=1)
  allrep[1,1] <- NG[1,1]
  nn <- 1
  for(j in 1:nstep){
    tg1 <- (rowSums(t(apply(ind.par,1,"*",LL[nn,])))*NG[j,index])
    tg2 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+1,])))*NG[j,index])
    tg3 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+2,])))*NG[j,index])
    tg4 <- (rowSums(t(apply(ind.par,1,"*",LL[nn+3,])))*NG[j,index])
    tmp <- allrep[j,] +stp*(tg1+2*tg2+2*tg3+tg4)/6
    allrep <- rbind(allrep,tmp)
    nn <- nn + 4
  }
  
  allrep
}

get_miu = function(miu_par,state,times,order,pheno,effect,nstep,A)
{
  parameters <- c(r1 = miu_par[1],
                  a1 = miu_par[2],
                  k1 = miu_par[3]
                  
                  
  )
  
  miu1 <- as.numeric(ode(y = state, times = times, func = Lorenz, parms = parameters, method = 'rk4')[,2])
  nt=seq(min(times),max(times),(max(times)-min(times))/nstep)
  LL <- LMall(NX=1,nt=nt,nstep=nstep,order=order)
  miu2 <- ode.sovle.ind(effect,para=miu_par[4:(3+(order-1)*4)],nt,order,nstep,LL,A)
  miu3 <- cbind(miu1,miu2[,2:4])
  miu <- as.numeric(rowSums(miu3))
  return (miu);
}


get_value = function(miu_par,state,times,effect,order,nstep,A)
{
  parameters <- c(r1 = miu_par[1],
                  a1 = miu_par[2],
                  k1 = miu_par[3]
                  
                  
  )
  
  miu1 <- as.numeric(ode(y = state, times = times, func = Lorenz, parms = parameters, method = 'rk4')[,2])
  nt=seq(min(times),max(times),(max(times)-min(times))/nstep)
  LL <- LMall(NX=1,nt=nt,nstep=nstep,order=order)
  miu2 <- ode.sovle.ind(effect,para=miu_par[4:(3+(order-1)*4)],nt,order,nstep,LL,A)
  miu3 <- as.matrix(miu2[,2:4])
  value <- matrix(NA,ncol = 4,nrow = length(times))
  value[,1] <-miu1
  value[,2:4] <-miu3
  return (value);
}
SAD1.get_mat <- function (par0, times, traits = 1, options = list()) {
  
  par <- par0
  if (class(par0) == "list") 
    par <- unlist(par0)
  t_len <- length(times)
  SAD.1 <- array(0, dim = c(t_len * traits, t_len * traits))
  for (i0 in 1:traits) for (i1 in 1:traits) {
    if (i0 == i1) 
      for (k0 in 1:t_len) for (k1 in k0:t_len) {
        for (k0 in 1:t_len) for (k1 in k0:t_len) {
          if(k0==k1){
            SAD.1[(i0 - 1) * t_len + k0, (i1 - 1) * t_len + 
                    k1] <- abs(par[i0 * 2])^2 * ((1-par[i0 * 2 - 1]^(2*k0))/(1-par[i0 * 2 - 1]^2))
          }
          if(k0<k1){
            SAD.1[(i0 - 1) * t_len + k0, (i1 - 1) * t_len + 
                    k1] <- abs(par[i0 * 2])^2 *par[i0 * 2 - 1]^(k1 - k0)*(1-par[i0 * 2 - 1]^(2*k0))/(1-par[i0 * 2 - 1]^(2))
            SAD.1[(i0 - 1) * t_len + k1, (i1 - 1) * t_len + 
                    k0] <- abs(par[i0 * 2])^2 *par[i0 * 2 - 1]^(k1 - k0)* (1-par[i0 * 2 - 1]^(2*k0))/(1-par[i0 * 2 - 1]^(2))
          }
        }
      }
  }
  return(SAD.1)
}




H_fn <- function(par,pheno,effect, state,times,order,nstep,A){
  miu1 =get_miu(par[1:((order-1)*4+3)], state = state,times = times,order=order,effect=effect,pheno=pheno,nstep=nstep,A)
  sigma1 = SAD1.get_mat(par[((order-1)*4+4):((order-1)*4+9)],times = times)
  L0 = c()
  L0 = sum(dmvnorm(pheno,miu1,sigma1,log = T))
  return(-L0)
}


A <- c(1,2,3,4)
sample_m <- as.numeric(c(data_three1[1,]))
H0_state <- c(E=(as.numeric(c(data_three1[1,1]))))

#LS_init_par <- c(-0.6093445,-0.5952629,3.8336319,rep(0.00001,12))




LS_init_par <- c(0.066823 , 4.245165, 15.360690,rep(0.00001,3),
                 -0.029179215,-0.005039992,-0.019824777,
                 0.022341209,0.024114919,-0.041621631,
                 0.019435340,-0.072920937,0.022321871)
H0_init_par = c(LS_init_par,0.1, 0.1, 0.1, 0.1, 0.1, 0.1)


H0_par = optim(H0_init_par,H_fn, times = times, pheno = sample_m,effect=t(data_three1),A=A, state = H0_state,order=4,nstep=13, method = "Nelder-Mead")
#par=H0_init_par
out_E1 <- get_value(H0_par$par[1:length(LS_init_par)], state = H0_state,effect=t(data_three1),A=A, times = times,order=4,nstep=13)

