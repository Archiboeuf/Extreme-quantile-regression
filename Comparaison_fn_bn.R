library(quantreg)	
set.seed(1)

tau <- 0.025

v <- rep(0, 10000)
w <- rep(0, 10000)
z5 <- rep(0, 10000)
z6 <- rep(0, 10000)

for (i in seq(10000))
{
  y <- rcauchy(200, 1, 1)
  test <- rq(y~1,tau = tau, method = "fn")
  v[i] = test$coefficients
  test <- rq(y~1,tau = tau, method = "br")
  w[i] = test$coefficients
  z5[i] = sort(y)[length(y)*tau]
  z6[i] = sort(y)[length(y)*tau+1]
}

jpeg('fn vs br.jpg')
plot(0, 0, xlim = c(-100, 10), ylim = c(-20, 20),xlab='Estimated cauchy quantile',ylab='Estimated normal quantile')
#Pour l'inférence normale, on utilise la formule simplifiée du cours : 
# qestimé -q ~(1/sqrt(n)) * sqrt(tau)(1-tau)/(f epsilon(q tau(epsilon))^2) * N(0, 1)
x= sort((1/sqrt(200))*(((tau*(1-tau))/(dcauchy(qcauchy(tau))^2))^0.5)*rnorm(10000,0, 1))
lines(sort(v-qcauchy(tau, 1, 1)), x, col ="red")
lines(sort(w-qcauchy(tau, 1, 1)), x, col ="blue")

#On devrait être tengeant ici
abline(a = 0, b = 1)
abline(v = 0)
abline(h = 0)
legend("topleft", legend = c("br", "fn"), fill = c("blue", "red"))
dev.off()


jpeg('fn vs br vs z5 vs z6.jpg')
plot(0, 0, xlim = c(-100, 10), ylim = c(-20, 20),xlab='Estimated cauchy quantile',ylab='Estimated normal quantile')
lines(sort(v-qcauchy(tau, 1, 1)), x, col ="red")
lines(sort(w-qcauchy(tau, 1, 1)), x, col ="blue")
lines(sort(z5-qcauchy(tau, 1, 1)), x, col ="green")
lines(sort(z6-qcauchy(tau, 1, 1)), x, col ="purple")

abline(a = 0, b = 1)
abline(v = 0)
abline(h = 0)
legend("topleft", legend = c("fn", "br","z5","z6"), fill = c("red", "blue","green","purple"))
dev.off()

jpeg('v vs z5.jpg')
plot(v,z5)
abline(a=0,b=1)
dev.off()

jpeg('v vs z5 and z6.jpg')
plot(v,z5,col="blue",xlab='v',ylab='')
abline(a=0,b=1)
points(v,z6,col="red")
legend("topleft", legend = c("z5","z6"), fill = c("blue", "red"))
dev.off()
mean(abs(v-z5)) #0.47
mean(abs(z6-v)) #2.71

jpeg('w vs z6.jpg')
plot(w,z6)
abline(a=0,b=1)
dev.off()
mean(abs(z6-w)) # O

for (i in seq(10000))
{
  y <- rcauchy(200, 1, 1)
  test <- rq(y~1,tau = tau-1/100000, method = "fn")
  v[i] = test$coefficients
  test <- rq(y~1,tau = tau-1/100000, method = "br")
  w[i] = test$coefficients
  z5[i] = sort(y)[length(y)*tau]
  z6[i] = sort(y)[length(y)*tau+1]
}

jpeg('v vs z5 tau-epsilon.jpg')
plot(v,z5)
abline(a=0,b=1)
dev.off()
mean(abs(v-z5)) #7.324693e-06

jpeg('w vs z5 tau-epsilon.jpg')
plot(w,z5)
abline(a=0,b=1)
dev.off()
mean(abs(w-z5)) #0




####----
#EV théoriques de Sébastien
library(quantreg)	
set.seed(1)

tau <- 0.025

v <- rep(0, 10000)
w <- rep(0, 10000)

for (i in seq(10000))
{
  y <- rcauchy(200, 1, 1)
  test <- rq(y~1,tau = tau-0.00001, method = "fn")
  v[i] = test$coefficients
  test <- rq(y~1,tau = tau-0.00001, method = "br")
  w[i] = test$coefficients
}


#Exemple pour les EV théoriques pour les quantiles
plot(0, 0, xlim = c(-100, 10), ylim = c(-100, 20),xlab='Estimated cauchy quantile',ylab='Normal & EV théorique')
#Pour l'inférence normale, on utilise la formule simplifiée du cours : 
# qestimé -q ~(1/sqrt(n)) * sqrt(tau)(1-tau)/(f epsilon(q tau(epsilon))^2) * N(0, 1)
x= sort((1/sqrt(200))*(((tau*(1-tau))/(dcauchy(qcauchy(tau))^2))^0.5)*rnorm(10000,0, 1))
lines(sort(v-qcauchy(tau, 1, 1)), x, col ="red")
lines(sort(w-qcauchy(tau, 1, 1)), x, col ="blue")
#EV théoriques
x_ = sort(qcauchy(0.005, 1, 1)*(1/rgamma(10000, 5, 1)-1/5)) #eq 2.5
lines(sort(v-qcauchy(tau, 1, 1)), x_, col ="red")
lines(sort(w-qcauchy(tau, 1, 1)), x_, col ="blue")


#toy example de régression quantile qui ne soit pas un location medel
library(quantreg)
a1 <- list()
a2 <- list()
b1 <- list()
b2 <- list()
for(i in seq(5000))
{  
  x <- runif(200, 0, 10)
  y <- x + sqrt(x) + sapply(x, function(x)rnorm(1, 0, sqrt(x)))
  a1[[i]] <- rq(y~x+sqrt(x), tau = 0.025)$coeff
  a2[[i]] <- rq(y~x+sqrt(x), tau = 0.025, method = "fn")$coeff
  b1[[i]] <- rq(y~x+sqrt(x), tau = 0.975)$coeff
  b2[[i]] <- rq(y~x+sqrt(x), tau = 0.975, method = "fn")$coeff
}

plot(x, y)


mean(sapply(a1, function(x)x[3]))
mean(sapply(a2, function(x)x[3]))
mean(sapply(b1, function(x)x[3]))
mean(sapply(b2, function(x)x[3]))


#eq 2.10
a1 <- list()
a2 <- list()
a1m <- list()
a2m <- list()
Xbar <- list()
A1 <- list()
A2 <- list()
Z1 <- list()
Z2 <- list()
tau=0.025
grand_T=200
m = 1.61 # il faut tau*T(m-1)>d ... d = dimension beta ? N'importe quel m satisfaisant la condition convient ?
for(i in seq(5000))
{  
  x <- runif(grand_T, 0, 10)
  y <- x + sqrt(x) + sapply(x, function(x)rnorm(1, 0, sqrt(x)))
  a1[[i]] <- rq(y~x+sqrt(x), tau = tau)$coeff
  a2[[i]] <- rq(y~x+sqrt(x), tau = tau, method = "fn")$coeff
  a1m[[i]] <- rq(y~x+sqrt(x), tau = m*tau)$coeff
  a2m[[i]] <- rq(y~x+sqrt(x), tau = m*tau, method = "fn")$coeff
  Xbar[[i]]=c(1,mean(x),mean(sqrt(x)))
  A1[[i]]=sqrt(tau*T)/(Xbar[[i]]"pourcent"*"pourcent"(a1m[[i]]-a1[[i]]))
  A2[[i]]=sqrt(tau*T)/(Xbar[[i]]"pourcent"*"pourcent"(a2m[[i]]-a2[[i]])) # remplacer les "pourcent" par le signe
  Z1[[i]]=A1[[i]]*(a1[[i]]-c(0,1,-0.96))
  Z2[[i]]=A2[[i]]*(a2[[i]]-c(0,1,-0.96))
}

var=3
plot(sort(sapply(a1, function(x)x[var])),sort(sapply(Z1, function(x)x[var])),ylim=c(-10,20))
abline(a=0,b=1)

plot(sort(sapply(a2, function(x)x[var])),sort(sapply(Z2, function(x)x[var])))
abline(a=0,b=1)


### version de Seb : 
#il y avait une erreur de parenthèses dans la version de Ben

for(i in seq(800))
{  
  x <- runif(grand_T, 0, 10)
  y <- x + sqrt(x) + sapply(x, function(x)rnorm(1, 0, sqrt(x)))
  a1[[i]] <- rq(y~x+sqrt(x), tau = tauT)$coeff
  a2[[i]] <- rq(y~x+sqrt(x), tau = 2*tauT)$coeff
  a4[[i]] <- rq(y~x+sqrt(x), tau = 4*tauT)$coeff
  Xbar[[i]]=c(1,mean(x),mean(sqrt(x)))
  xi[[i]]=-(1/log(2))*log((Xbar[[i]]%*%(a4[[i]]-a1[[i]]))/(Xbar[[i]]%*%(a2[[i]]-a1[[i]])))
  L[[i]]=(Xbar[[i]]%*%(a2[[i]]-a1[[i]]))/((2^(-xi[[i]])-1)*tau^(-xi[[i]]))
}


#4.16----
A_T <- function(y, x, tau, p)
{
  t <- nrow(x)
  #m = (d+p)/tau T   + 1
  m <- (((ncol(x)+p)/(tau*t))+1)
  beta_m_tau <- rq(y~x, m*tau)$coeff
  beta_tau <- rq(y~x, tau)$coeff
  return(c(sqrt(tau*t)/(c(1, apply(x, 2, mean))%*%(beta_m_tau-beta_tau))))
}
A_T(y, cbind(x, sapply(x, sqrt)),0.025, 5 )


#4.17----
#on ne fait pas A_ibt car là on calcule pour un i donné
A_bt <- function(y_b, x_b, tau_b, m)
{
  b <- nrow(x_b)
  beta_m_tau_b <- rq(y_b~x_b, m*tau_b)$coeff
  beta_tau_b <- rq(y_b~x_b, tau_b)$coeff
  return(c(sqrt(tau_b*b)/(c(1, apply(x_b, 2, mean))%*%(beta_m_tau_b-beta_tau_b))))
}

sample_Vt <- function(n_samples, psi, y, x, tau, p, b)
{
  t <- nrow(x)
  m <- (((ncol(x)+1+p)/(tau*t))+1)
  tau_b <- tau*nrow(x)/b
  res <- rep(0, n_samples)
  for(i in seq(n_samples))
  {
    idx <- sample.int(nrow(x), size = b)
    x_b = x[idx,]
    y_b = y[idx]
    beta_ibt <- rq(y_b~x_b, tau_b)$coeff
    beta <- rq(y~x, tau_b)$coeff
    res[i]<- c(A_bt(y_b, x_b, tau_b, m)*psi%*%(beta_ibt-beta))
  }
  return(res)
}


sample_Vt(1000, c(0, 0, 1), y, cbind(rep(1, 10000), x, sqrt(x)), 0.025, 5, 500)


################################


#eq 3.15
xi=list()
L=list()
a1 <- list()
a2 <- list()
a4 <- list()
Xbar <- list()
grand_T=200
tau=5/grand_T
tauT=1/sqrt(grand_T) #il faut tauT*T tend vers infini et tauT tend vers 0 

var=3
min(sapply(a4, function(x)x[var])-sapply(a1, function(x)x[var]))
#log d'un truc négatif ???

### version de Seb : 
#il y avait une erreur de parenthèses dans la version de Ben

for(i in seq(800))
{  
  x <- runif(grand_T, 0, 10)
  y <- x + sqrt(x) + sapply(x, function(x)rnorm(1, 0, sqrt(x)))
  a1[[i]] <- rq(y~x+sqrt(x), tau = tauT)$coeff
  a2[[i]] <- rq(y~x+sqrt(x), tau = 2*tauT)$coeff
  a4[[i]] <- rq(y~x+sqrt(x), tau = 4*tauT)$coeff
  Xbar[[i]]=c(1,mean(x),mean(sqrt(x)))
  xi[[i]]=-(1/log(2))*log((Xbar[[i]]%*%(a4[[i]]-a1[[i]]))/(Xbar[[i]]%*%(a2[[i]]-a1[[i]])))
  L[[i]]=(Xbar[[i]]%*%(a2[[i]]-a1[[i]]))/((2^(-xi[[i]])-1)*tau^(-xi[[i]]))
}




###################################

picklands <- function(x, y)
{
  grand_T <- nrow(x)
  tauT <- max(1/sqrt(grand_T), 1/10) #pas plutôt min ? il faut que tauT tende vers 0
  a1 <- rq(y~x, tau = tauT  )$coeff
  a2 <- rq(y~x, tau = 2*tauT)$coeff
  a4 <- rq(y~x, tau = 4*tauT)$coeff
  Xbar <- apply(x, 2, mean)
  Xbar <- c(1, Xbar)
  xi=-(1/log(2))*log((Xbar"pourcent"*"pourcent"(a4-a1))/(Xbar"pourcent"*"pourcent"(a2-a1)))
  L=(Xbar"pourcent"*"pourcent"(a2-a1))/((2^(-xi)-1)*tau^(-xi))
  return(list("xi"=xi, "L" = L))
}
picklands(cbind(x, sqrt(x)), y)


