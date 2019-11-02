library(rethinking)
options(mc.cores = parallel::detectCores())
data(Lynx_Hare)
library(shinystan)

# Try using generated quantities to calculate the predicted pelts in ulam

dat_ar1gq <- list(                
  L = Lynx_Hare$Lynx[2:21],
  L_lag1 = Lynx_Hare$Lynx[1:20],
  H = Lynx_Hare$Hare[2:21],
  H_lag1 = Lynx_Hare$Hare[1:20] ,
  R = length(Lynx_Hare$Hare[1:20])
)

m16H3gq <- ulam(
  alist(
    L ~ lognormal( log(mu_L), sigma_L ),
    H ~ lognormal( log(mu_H), sigma_H ),
    
    # Cannot define mu_L and mu_H as gq> (generated quantities) as parser error occurs...
    transpars> mu_L  <- a_L + b_LL * L_lag1 + b_LH * H_lag1,
    transpars> mu_H  <- a_H + b_HH * H_lag1 - minus_b_HL * L_lag1,
    
    #          c(a_L, a_H) ~ exponential( 10 ),     # a_L and a_H are the values of mu_L and mu_H if there were no L_lag1 and H_lag1 (i.e. no Lynx of Hares in the previous time period).  One would think the a_H and a_L should therefore be 0, unless we have migration into the area.  Clearly, though should be positive, bounded below by 0.
    c(a_L, a_H) ~ lognormal( log(0.1), 0.5 ),     # a_L and a_H are the values of mu_L and mu_H if there were no L_lag1 and H_lag1 (i.e. no Lynx of Hares in the previous time period).  One would think the a_H and a_L should therefore be 0, unless we have migration into the area.  Clearly, though should be positive, bounded below by 0.
    #          c(b_LL, b_HH, b_LH) ~ lognormal(log(1), 1),    # b_LL and b_HH must be positive otherwise would have oscillation every period.  b_LH is likely to be positive as an increase in hares should cause a lagged increase in lynx.
    c(b_LL, b_HH) ~ lognormal(log(1), 1),    # b_LL and b_HH must be positive otherwise would have oscillation every period.
    #          b_HL ~ normal(0, 1),            # These may be negative as more Lynx should lead to a reduction in Hares.
    #          b_LH ~ normal(0, 1),            # Let the data decide this relationship...
    minus_b_HL ~ lognormal(log(2), 1),   # This is negative as more Lynx should lead to a reduction in Hares.  Cannot do -(b_HL), so rename parameter minus_b_HL and assume it's positive, so use -ve operator on this parameter in the link function
    b_LH ~ lognormal(log(0.5), 1),    # 
    
    sigma_L ~ exponential(0.5),
    sigma_H ~ exponential(0.5),
    
    gq> vector[R]:pelts_pred_L <- lognormal_rng( log(mu_L[i]), sigma_L ),
    gq> vector[R]:pelts_pred_H <- lognormal_rng( log(mu_H[i]), sigma_H )
  )
  , data=dat_ar1gq, chains=4, cores=4
  , log_lik=TRUE
  , control=list(adapt_delta=0.99)
)
  
            
post <- extract.samples(m16H3gq)
post$pelts_pred_L
