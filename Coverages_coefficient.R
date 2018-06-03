library(quantreg) 
library(parallel)

##################
#FUNCTIONS NEEDED#
##################

sample_Vt_par <- function( y, x, tau, BT, b, p=5)
{
  #calcul des variables----
  grand_T<- nrow(x)
  #juste apres 4.17
  tau_b <- grand_T*tau/b
  
  #beta_taub : full sample, mais quantile taub. juste apres 4.17
  beta_taub <- rq(y~x,tau =  tau_b, method = "fn")$coeff
  m <- (p+length(beta_taub))/(tau*grand_T)+1
  
  
  #estimateurs bs----
  res <- list()
  nocores <- detectCores()-2
  cores <- makeCluster(nocores)
  clusterExport(cl = cores, varlist = c("rq", "BT", "y", "x", "tau", "tau_b", "beta_taub",  "b",  "m", "grand_T"), envir = environment())
  res <- parSapply(cores, 1:BT, function(i)
  {
    #tirage de l'echantillon sans remplacement----
    idx <- sample(1:grand_T, size = b, replace = F)
    x_b = x[idx,]
    y_b = y[idx]
    #beta_ibt = restreint a l'echantillon bs. juste apres 4.17----
    beta_ibt <- rq(y_b~x_b, tau_b, method = "fn")$coeff
    beta_ibt_m <- rq(y_b~x_b, m*tau_b, method = "fn")$coeff
    #A_ibt : calcul selon 4.17----
    X_ibt <- c(1, apply(x_b, 2, mean))
    A_ibt <- sqrt(tau_b*b)/(X_ibt%*%(beta_ibt_m - beta_ibt))
    return(A_ibt*(beta_ibt-beta_taub))
  })  
  stopCluster(cores)
  return(res)
}


confidence_interval <- function(alpha,  y, x, tau, BT, b, p=5)
{
  grand_T <- nrow(x)
  test <- sample_Vt_par(y = y, x = x, tau = tau, BT = BT, b=b, p=p)
  #calcul de A : 4.16
  beta_tau <- rq(y~x, tau, method = "fn")$coeff
  m <- (p+length(beta_tau))/(tau*grand_T)+1
  beta_tau_m <- rq(y~x, m*tau, method = "fn")$coeff
  X <- c(1, apply(x, 2, mean))
  A <- sqrt(tau*grand_T)/(X%*%(beta_tau_m-beta_tau))
  
  alpha <- alpha/2
  c_alpha <- apply(test, 1, function(v)quantile(v, alpha))
  c_1_alpha <- apply(test, 1, function(v)quantile(v, 1-alpha))
  
  borne_sup <- beta_tau - c_alpha/A
  borne_inf <- beta_tau - c_1_alpha/A
  return(list("beta" = beta_tau, "inf" = borne_inf, "sup" = borne_sup))
}



test_NORM <- list()
cov_intercept_NORM <- c()
cov_x_NORM <- c()
cov_sqrtx_NORM <- c()

test_EV <- list()
cov_intercept_EV <- c()
cov_x_EV <- c()
cov_sqrtx_EV <- c()


for(tau in c(0.001,0.002,0.003,0.004,0.005,0.01,0.02,
             0.03,0.04,0.05,0.06,0.07,0.08,0.09)){
  show(tau)
  for(i in seq(100)){
    x <- runif(1000, 1, 10)
    y <- x+sqrt(x)+rnorm(1000, 0, sqrt(x))
    
    QR <- rq(y~x+sqrt(x), tau=tau, method = 'fn')
    resume <- summary(QR, se="ker")
    coeff <- coef(resume)
    borne_inf <- coeff[,1]-1.64*coeff[,2]
    borne_sup <- coeff[,1]+1.64*coeff[,2]
    beta_tau <- QR$coeff
    res_NORM <- list("beta" = beta_tau, "inf" = borne_inf, "sup" = borne_sup)

    test_NORM[[i]] <- (res_NORM$inf<c(0, 1, -0.96))*(res_NORM$sup>c(0, 1, -0.96))
    
    res_EV <- confidence_interval(alpha = 0.05, y = y, x = cbind(x, sqrt(x)), tau = tau, BT= 200, b=100, p=2)
    
    test_EV[[i]] <- (res_EV$inf<c(0, 1, -0.96))*(res_EV$sup>c(0, 1, -0.96)) 

  }
  
  cov_intercept_NORM <- c(cov_intercept_NORM,mean(sapply(test_NORM, function(i)i[1])))
  cov_x_NORM <- c(cov_x_NORM,mean(sapply(test_NORM, function(i)i[2])))
  cov_sqrtx_NORM <- c(cov_sqrtx_NORM,mean(sapply(test_NORM, function(i)i[3])))

  cov_intercept_EV <- c(cov_intercept_EV,mean(sapply(test_EV, function(i)i[1])))
  cov_x_EV <- c(cov_x_EV,mean(sapply(test_EV, function(i)i[2])))
  cov_sqrtx_EV <- c(cov_sqrtx_EV,mean(sapply(test_EV, function(i)i[3])))
  
}

cov_intercept_NORM 
cov_x_NORM 
cov_sqrtx_NORM 

cov_intercept_EV 
cov_x_EV 
cov_sqrtx_EV 

# In order not to waste hours recalculating, here are the results
# cov_intercept_NORM 
# 0.39 0.50 0.60 0.71 0.74 0.78 0.94 0.88 0.97 0.90 0.96 0.93
# cov_x_NORM 
# 0.36 0.40 0.55 0.68 0.70 0.77 0.89 0.86 0.89 0.88 0.92 0.89
# cov_sqrtx_NORM 
# 0.37 0.40 0.60 0.69 0.70 0.76 0.91 0.86 0.94 0.89 0.94 0.92
# 
# cov_intercept_EV 
# 0.87 0.93 0.88 0.92 0.88 0.91 0.97 0.88 0.96 0.89 0.91 0.89
# cov_x_EV 
# 0.89 0.92 0.87 0.92 0.87 0.91 0.95 0.88 0.90 0.91 0.89 0.91
# cov_sqrtx_EV
# 0.84 0.91 0.86 0.90 0.89 0.90 0.98 0.87 0.91 0.89 0.91 0.90


x_axis = c(0.001,0.002,0.003,0.004,0.005,0.01,0.02,0.03,0.04,0.05,0.08,0.09)

par(mfrow = c(1,3))
plot(0, 0, xlim = c(0, 0.1), ylim = c(0.35, 1),
     xlab = "tau", ylab = "Coverage", main = 'Coverage - Intercept')
lines(x_axis,cov_intercept_EV,type="b", col = "blue")
lines(x_axis,cov_intercept_NORM, col = "red",type="b")
abline(a=0.9,b=0, col ="black")
legend(0.04, 0.5,  legend=c("Extremal", "Central"),
       col=c("blue", "red"), lty=1:1, cex=1.5)

plot(0, 0, xlim = c(0, 0.1), ylim = c(0.35, 1),
     xlab = "tau", ylab = "Coverage", main = 'Coverage - x')
lines(x_axis,cov_x_EV,type="b", col = "blue")
lines(x_axis,cov_x_NORM, col = "red",type="b")
abline(a=0.9,b=0, col ="black")
legend(0.04, 0.5,  legend=c("Extremal", "Central"),
       col=c("blue", "red"), lty=1:1, cex=1.5)

plot(0, 0, xlim = c(0, 0.1), ylim = c(0.35, 1),
     xlab = "tau", ylab = "Coverage", main = 'Coverage - sqrt(x)')
lines(x_axis,cov_sqrtx_EV,type="b", col = "blue")
lines(x_axis,cov_sqrtx_NORM, col = "red",type="b")
abline(a=0.9,b=0, col ="black")
legend(0.04, 0.5,  legend=c("Extremal", "Central"),
       col=c("blue", "red"), lty=1:1, cex=1.5) 