library(rethinking)
options(mc.cores = parallel::detectCores())

# setwd('~/Documents/Stats, Machine Learning and AI/Statistical Rethinking/Rcode/Chpt16 Practice 16M3 Figures Prior Predictive Sims/')
data(Lynx_Hare)

dat_list <- list(
N = nrow(Lynx_Hare),
pelts = Lynx_Hare[,2:3] )

compiled_model <- stan_model("model_priorPredict.stan")

#fake_data <- Lynx_Hare[,2:3]
f <- rep(1,21)
fake_data <- data.frame(f,f)

set.seed(23)
sim_out <- sampling(compiled_model, data = list(N = 21, pelts = fake_data, run_estimation = 0 )
, chains=4, cores=4 , control=list(adapt_delta=0.99, max_treedepth=20) , iter=3000 )



library(dplyr)

fake_data_matrix  <- sim_out %>% 
as.data.frame %>% 
select(contains("pelts_pred"))



prior <- data.frame(fake_data_matrix)


pelts <- dat_list$pelts

  plot( 1:21 , pelts[,2] , pch=16 , ylim=c(0,200) , xlab="year" , 
        ylab="thousands of pelts" , xaxt="n" )
  at <- c(1,11,21)
  axis( 1 , at=at , labels=Lynx_Hare$Year[at] )
  points( 1:21 , pelts[,1] , col=rangi2 , pch=16 )
  
  # 21 time series from prior
  for ( s in 1:30 ) {
    #     lines( 1:21 , prior$pelts_pred[s,,2] , col=col.alpha("black",0.2) , lwd=2 )
    #     lines( 1:21 , prior$pelts_pred[s,,1] , col=col.alpha(rangi2,0.3) , lwd=2 )
    lines( 1:21 , prior[s,22:42] , col=col.alpha("black",0.2) , lwd=2 )
    lines( 1:21 , prior[s,1:21] , col=col.alpha(rangi2,0.3) , lwd=2 )
      
  }
  
  # text labels
  text( 17 , 90 , "Lepus" , pos=2 )
  text( 19 , 50 , "Lynx" , pos=2 , col=rangi2 )



# Now try to estimate and get the posterior:

  sim_out_post <- sampling(compiled_model, data = list(N = 21, 
                                                  # pelts should be real but fake data if we're simulating
                                                  # new pelts
                                                  pelts = pelts,
                                                  run_estimation = 1     # turn off estimation to instead do prior predictive simulation
  )
  , chains=4, cores=4
  , control=list(adapt_delta=0.99, max_treedepth=20)
  , iter=2000
  )
  
  library(dplyr)
  
  data_matrix  <- sim_out_post %>% 
    as.data.frame %>% 
    select(contains("pelts_pred"))
  
  post <- data.frame(data_matrix)
  # post[1,22:42]
  # post[22]
  
  pelts <- dat_list$pelts
  plot( 1:21 , pelts[,2] , pch=16 , ylim=c(0,120) , xlab="year" , 
        ylab="thousands of pelts" , xaxt="n" )
  at <- c(1,11,21)
  axis( 1 , at=at , labels=Lynx_Hare$Year[at] )
  points( 1:21 , pelts[,1] , col=rangi2 , pch=16 )
  
  # 21 time series from posterior
  for ( s in 1:30 ) {
    #     lines( 1:21 , prior$pelts_pred[s,,2] , col=col.alpha("black",0.2) , lwd=2 )
    #     lines( 1:21 , prior$pelts_pred[s,,1] , col=col.alpha(rangi2,0.3) , lwd=2 )
    lines( 1:21 , post[s,22:42] , col=col.alpha("black",0.2) , lwd=2 )
    lines( 1:21 , post[s,1:21] , col=col.alpha(rangi2,0.3) , lwd=2 )
    
  }
  
  # text labels
  text( 17 , 90 , "Lepus" , pos=2 )
  text( 19 , 50 , "Lynx" , pos=2 , col=rangi2 )