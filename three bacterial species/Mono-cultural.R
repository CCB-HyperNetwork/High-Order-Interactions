Lorenz <- function(t,state, parameters)
{
  with( as.list(c(state, parameters)),
        {
          dE <- a1*(1-(E/k1)^r1)*E
          
          list(c(dE))
        }
  )
}
get_miu = function(miu_par,state,times)
{
  parameters <- c(r1 = miu_par[1],
                  a1 = miu_par[2],
                  k1 = miu_par[3]
                  
                  
  )
  miu <- as.numeric(ode(y = state, times = times, func = Lorenz, parms = parameters, method = 'rk4')[,2])
  return (miu);
}

fn <- function (par, pheno, state){
  y <- get_miu(par, state = state,times = times);
  return(sum((pheno-y)^2))}

get_value = function(miu_par,state,times)
{
  parameters <- c(r1 = miu_par[1],
                  a1 = miu_par[2],
                  k1 = miu_par[3]
                  
                  
  )
  value <- ode(y = state, times = times, func = Lorenz, parms = parameters, method = 'rk4')[,2]
  return (value);
}

sample_m <- as.numeric(c(data[1,]))
H0_state <- c(E=as.numeric(c(data[1,1])))
LS_init_par <- c(0.1, 3, 5)#0.066823 , 4.245165, 15.360690
LS  = optim(LS_init_par, fn = fn, pheno = sample_m, state = H0_state, method = "Nelder-Mead")
