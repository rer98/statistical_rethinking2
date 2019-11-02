library(rethinking)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
data(Panda_nuts)
d <- Panda_nuts
head(d)

dat_list <- list(
  n = as.integer( Panda_nuts$nuts_opened ),
  age = Panda_nuts$age / max(Panda_nuts$age),
  gid = ifelse(Panda_nuts$sex=='m', 1L, 2L),    # Males have gid=1, females gid=2.  We define this way for simpler prior in model below.
  phiMu = c(1, 0.7),   # We use this in phi ~ lognorm(log(phiMu), 0.1), with phiMu=1 for gid=1 (i.e. males) and phiMu=0.7 for gid=2 (i.e. females)
  seconds = Panda_nuts$seconds )

  m16H1 <- ulam(
    alist(
      n ~ poisson( lambda ),
      lambda <- seconds*phi*(1-exp(-k*age))^theta,
  #   phi ~ lognormal( log(1) , 0.1 ),           # This was the original gender neutral prior.  Instead, we replace with gender specific prior below:
      phi ~ lognormal( log(phiMu[gid]) , 0.1 ),    # This enables males to have distribution with larger mean/median phi than females.  Phi[gid] represents α * (β * M_max[gid])^θ, where males and femals have different M_max values.
      k ~ lognormal( log(2) , 0.25 ),
      theta ~ lognormal( log(5) , 0.25 )
    ), data=dat_list , chains=4 , cores=4 )
  
  
  
m16H1b <- ulam(
    alist(
      n ~ poisson( lambda ),
      lambda <- seconds*phi[gid]*(1-exp(-k*age))^theta,
      #   phi ~ lognormal( log(1) , 0.1 ),           # This was the original gender neutral prior.  Instead, we replace with gender specific prior below:
      vector[84]: phi ~ lognormal( log(phiMu[gid]) , 0.1 ),    # This enables males to have distribution with larger mean/median phi than females.  Phi[gid] represents α * (β * M_max[gid])^θ, where males and femals have different M_max values.
      k ~ lognormal( log(2) , 0.25 ),
      theta ~ lognormal( log(5) , 0.25 )
    ), data=dat_list , chains=4 , cores=4 )

dat_list

          
  m16H1d <- ulam(
    alist(
      n ~ poisson( lambda ),
      lambda <- seconds*phi[gid]*(1-exp(-k*age))^theta,
  #    phi[gid] ~ lognormal( log(1) , 0.1 ),           # The prior is the same, but allows the posterior of phi to be gender specific
      phi[gid] ~ lognormal( log(phiMu) , 0.1 ),           # This enables males to have distribution with larger mean/median phi than females.  Phi[gid] represents α * (β * M_max[gid])^θ, where males and femals have different M_max values.
      k ~ lognormal( log(2) , 0.25 ),
      theta ~ lognormal( log(5) , 0.25 )
    ), data=dat_list , chains=4 , cores=4 )
