setwd('C:/Users/benji/Desktop/Cours/Semi and non-parametric econometrics')
BDD_Heart_Rate <- read.csv("Add-Health-Public-Use-In-Home-Data-Wave-IV.csv", header = T)
attach(BDD_Heart_Rate)

BDD_Heart_Rate <- subset(BDD_Heart_Rate, select = c("H4PR","H4BMI","H4OD1Y", "BIO_SEX4", "H4WS4", "H4GH8","H4GH9","H4GH10","H4GH12","H4DA3","H4DA4","H4DA5","H4DA6","H4DA8","H4DA13","H4TO41","H4TO39","H4TO11","H4MH6","H4LM26","H4HS1", "H4HS7"))


model.matrix(BDD_Heart_Rate$too_many_problems_past_30_days)
BDD_Heart_Rate <- BDD_Heart_Rate[complete.cases(BDD_Heart_Rate),]
names(BDD_Heart_Rate)<- sapply(names(BDD_Heart_Rate), function(name)
{  
if (name == "H4PR"    ) return("pulse_rate")
if (name == "H4BMI"    ) return("BMI")
if (name == "BIO_SEX4") return("sex")
if (name == "H4OD1Y") return("age")
if (name == "H4WS4"   ) return("number_close_friends")
if (name == "H4GH8"   ) return("fastfood_7_days")
if (name == "H4GH9"   ) return("sodas_7_days")
if (name == "H4GH10"  ) return("light_sodas_7_days")
if (name == "H4GH12"  ) return("caffeinated_drinks_24h")
if (name == "H4HS1"   ) return("insurance")
if (name == "H4HS7"   ) return("check_up")
if (name == "H4LM26"  ) return("work_satisfaction")
if (name == "H4MH6"   ) return("too_many_problems_past_30_days")
if (name == "H4TO11"  ) return("cigs_per_day")
if (name == "H4TO39"  ) return("alcohol_30_days")
if (name == "H4TO41"  ) return("alcohol_24_h")
if (name == "H4DA13"  ) return("gym_per_week")
if (name == "H4DA8"   ) return("sport_walk")
if (name == "H4DA6"   ) return("sport_gym")
if (name == "H4DA5"   ) return("sport_endurance_martial")
if (name == "H4DA4"   ) return("sport_team")
if (name == "H4DA3"   ) return("sport_portmanteau_H4DA3")
})


#traitement 

#pulse_rate
BDD_Heart_Rate <- BDD_Heart_Rate[BDD_Heart_Rate$pulse_rate<900,]

#sex and age----
BDD_Heart_Rate$sex <- BDD_Heart_Rate$sex-1
BDD_Heart_Rate$age <- 2008-BDD_Heart_Rate$age

#health care----
#insurance : The next questions are about health insurance and health services. Which of the following best describes your current health insurance situation? (H4HS1)
#merging various providers into one insurance
#putting "don't know" and "don't know what my policy is" with "don't have a policy"

BDD_Heart_Rate$insurance <-sapply(BDD_Heart_Rate$insurance , function(code)
{
  if(code ==1|code==11 |code ==98){return("no insurance")}
  else if(code < 9| code ==10){return("insurance")}
  else if(code ==9){return("medicaid")}
})

#BMI
BDD_Heart_Rate$BMI <-sapply(BDD_Heart_Rate$BMI,function(code)
{
  if(code >880 ){return(median(BDD_Heart_Rate$BMI))}
  else {return(code)}
})

#check-up  How long ago did you last have a routine check-up? (H4HS7)
# "never" and "IDK" were counted as 2 years
BDD_Heart_Rate$check_up <-sapply(BDD_Heart_Rate$check_up , function(code)
{
  if(code ==1){return(3)}
  if(code ==2){return(6)}
  if(code ==3){return(9)}
  if(code ==4){return(12)}
  if(code ==5){return(24)}
  if(code ==6){return(24)}
  if(code ==7){return(24)}
  if(code ==98){return(24)}

})


#psychological well being----
#friends : How many close friends do you have? (Close friends include people whom you feel at ease with, can talk to about private matters, and can call on for help.) (H4WS4)
#removing individuals that were not asked
#don't know or no answer -> 0
#question is by group of 2... We troncaturate to the lower
BDD_Heart_Rate <- BDD_Heart_Rate[BDD_Heart_Rate$number_close_friends!=95,]
BDD_Heart_Rate$number_close_friends <- sapply(BDD_Heart_Rate$number_close_friends , function(code)
{
  if(code ==1){return(0)}
  else if(code ==2){return(1)}
  else if(code ==3){return(3)}
  else if(code ==4){return(6)}
  else if(code ==5){return(10)}
  else{return(0)}
})

#How satisfied (are/were) you with this job, as a whole? (H4LM26)
#Removing know/want to answer (5 inds)
BDD_Heart_Rate<- BDD_Heart_Rate[BDD_Heart_Rate$work_satisfaction !=96&BDD_Heart_Rate$work_satisfaction !=98,]
BDD_Heart_Rate$work_satisfaction <- sapply(BDD_Heart_Rate$work_satisfaction , function(code)
{
  if(code ==1){return(":))")}
  if(code ==2){return(":)")}
  else if(code == 3){return(":|")}
  else if(code == 4){return(":(")}
  else if(code == 5){return(":((")}
  else return("not working")
})
BDD_Heart_Rate$work_satisfaction<- as.factor(BDD_Heart_Rate$work_satisfaction)
relevel(BDD_Heart_Rate$work_satisfaction , ref = "not working")

#In the last 30 days, how often have you felt that difficulties were piling up so high that you could not overcome them? (H4MH6)
#making "refuse" and "don't now" (8 individuals) be "sometimes"
BDD_Heart_Rate$too_many_problems_past_30_days <- sapply(BDD_Heart_Rate$too_many_problems_past_30_days , function(code)
{
  if(code>5)code <- 2
  if(code==1){return("never")}
  if(code==2){return("almost never")}
  if(code==3){return("sometimes or often")}
  if(code==4){return("sometimes or often")}
  if(code==5){return("sometimes or often")}
  else{return("sometimes or often")}

})


#drugs----
#cigarettes : During the past 30 days, on the days you smoked, how many cigarettes did you smoke each day? 
#binned data into quantitative
#refusal and don't know (13 inds) were put with light smokers
BDD_Heart_Rate$cigs_per_day
BDD_Heart_Rate$cigs_per_day <- sapply(BDD_Heart_Rate$cigs_per_day , function(code)

{
  if(code==1){return(10)}
  else if(code==2){return(20)}
  else if(code==3){return(30)}
  else if(code==4){return(30)}
  else{return(10)}
})



#caffeinated drinks.  Did you drink a caffeinated beverage (e.g., coffee, tea or soda) in the past 24 hours? (H4GH12)
#1 IDK set to 0
BDD_Heart_Rate$caffeinated_drinks_24h<- sapply(BDD_Heart_Rate$caffeinated_drinks_24h , function(code)
{
  if(code>1){return(0)}
  else(return(code))
})

#alcohol :  During the past 30 days, on how many days did you drink? (H4TO39)
#legitimate skip  = no alcohol ever + revious refusals. No alcohol are majority
#A couple of IDK and refuse 
#All set to 0
BDD_Heart_Rate$alcohol_30_days<- sapply(BDD_Heart_Rate$alcohol_30_days , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})

#Alcohol past 24 h
#Same
BDD_Heart_Rate$alcohol_24_h<- sapply(BDD_Heart_Rate$alcohol_24_h , function(code)
{
  if(code>1){return(0)}
  else(return(code))
})

#unhealthy food and drinks----
#fastfood the past 7 days : How many times in the past seven days did you eat food from a fast food restaurant, such as McDonald's, Burger King, Wendy's, Arby's, Pizza Hut, Taco Bell, or Kentucky Fried Chicken or a local fast food restaurant? (H4GH8)
#1 "IDK" put to 0, 2 funny guys who said "99 times" put to 0 too
BDD_Heart_Rate$fastfood_7_days<- sapply(BDD_Heart_Rate$fastfood_7_days , function(code)
{
  if(code>30){return(0)}
  else(return(code))
})
#regular soda the past 7 days : The next questions ask about what you generally eat and drink at home and away from home. In the past seven days, how many regular (non-diet) sweetened drinks did you have? Include regular soda, juice drinks, sweetened tea or coffee, energy drinks, flavored water, or other sweetened drinks. (H4GH9)
#nothing to do
BDD_Heart_Rate$sodas_7_days


#diet soda the past 7 days :  In the past seven days, how many diet or low-calorie drinks did you have? Include diet sodas, unsweetened tea or coffee, or other drinks sweetened with artificial sweeteners. (H4GH10)
#4 "IDK" put to 0, 1 refused put to 0 too
BDD_Heart_Rate$light_sodas_7_days<- sapply(BDD_Heart_Rate$light_sodas_7_days , function(code)
{
  if(code>100){return(0)}
  else(return(code))
})


#sport----
#fitness center : On the average, how many times per week do you use a physical fitness or recreation center in your neighborhood? (H4DA13)
#People in prison were set to 0
#2 refusals were set to 0
BDD_Heart_Rate$gym_per_week<- sapply(BDD_Heart_Rate$gym_per_week , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})
#sports : refused and IDK were set to 0
#sports : 1 every time(the same ?)
BDD_Heart_Rate$sport_portmanteau_H4DA3<- sapply(BDD_Heart_Rate$sport_portmanteau_H4DA3 , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})
BDD_Heart_Rate$sport_team<- sapply(BDD_Heart_Rate$sport_team , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})
BDD_Heart_Rate$sport_endurance_martial<- sapply(BDD_Heart_Rate$sport_endurance_martial , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})
BDD_Heart_Rate$sport_gym<- sapply(BDD_Heart_Rate$sport_gym , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})
# BDD_Heart_Rate$sport_portmanteau_H4DA7<- sapply(BDD_Heart_Rate$sport_portmanteau_H4DA7 , function(code)
# {
#   if(code>50){return(0)}
#   else(return(code))
# })
BDD_Heart_Rate$sport_walk<- sapply(BDD_Heart_Rate$sport_walk , function(code)
{
  if(code>50){return(0)}
  else(return(code))
})


# BDD_Heart_Rate <- BDD_Heart_Rate[BDD_Heart_Rate$work_satisfaction!= "not working",]
BDD_Heart_Rate$work_satisfaction=NULL

library(mlr )

BDD_dum <- rep(1,nrow(BDD_Heart_Rate))
noms <-c()
for(i in seq(ncol(BDD_Heart_Rate)))
{
  if(is.numeric(BDD_Heart_Rate[,i]))
  {
    BDD_dum <- cbind(BDD_dum, BDD_Heart_Rate[,i])
    noms <- c(noms, names(BDD_Heart_Rate)[i])
  }
  else 
  {
    BDD_dum <- cbind(BDD_dum, createDummyFeatures(BDD_Heart_Rate[, i])[,-1])
    noms <- c(noms, names(createDummyFeatures(BDD_Heart_Rate[, i])[,-1]))
  }  
}

BDD_dum <-BDD_dum[,-1]
colnames(BDD_dum)<- noms

#regression----

rq(pulse_rate~.,
   data = BDD_dum , tau = 0.5)




#theoreme 2----
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

confidence_interval_inverse=function(alpha,y,x,tau,BT,b,p){
  res=confidence_interval(alpha,-y,x,tau,BT,b,p)
  res$beta=-res$beta
  inf=res$inf
  sup=res$sup
  res$inf=-sup
  res$sup=-inf
  return(res)
}


confidence_int_normal = function(alpha,y,x,tau){
  QR <- rq(y~x, tau=tau, method = 'fn')
  
  resume <- summary(QR, se="iid")
  coeff <- coef(resume)
  
  borne_inf <- coeff[,1]-qnorm(1-alpha/2)*coeff[,2]
  borne_sup <- coeff[,1]+qnorm(1-alpha/2)*coeff[,2]
  return(list("inf"=borne_inf,"sup"=borne_sup))
}

confidence_int_normalinverse = function(alpha,y,x,tau){
  res=confidence_int_normal(alpha,-y,x,tau)
  inf=res$inf
  sup=res$sup
  res$inf=-sup
  res$sup=-inf
  return(res)
}


set.seed(3)
taus=c(0.01,0.02,0.03,0.04,0.05,0.1)
beta_tau_faibles=list()
beta_tau_eleves=list()
beta_tau_normaux_faibles=list()
beta_tau_normaux_eleves=list()
for(tau in taus){
  beta_tau_faibles[[paste("tau = ", tau)]] <- confidence_interval(alpha=0.1,y=BDD_dum$pulse_rate,
                    x = as.matrix(BDD_dum[-1]),
                    tau=tau, BT=500,b=1000,p=5)
  
  beta_tau_normaux_faibles[[paste("tau = ", tau)]] <- confidence_int_normal(alpha=0.1,y=BDD_dum$pulse_rate,x=as.matrix(BDD_dum[-1]),tau)

  beta_tau_eleves[[paste("tau = ", tau)]] <- confidence_interval_inverse(alpha=0.1,y=BDD_dum$pulse_rate,
                    x = as.matrix(BDD_dum[-1]),
                    tau=tau, BT=500,b=1000,p=5)

  beta_tau_normaux_eleves[[paste("tau = ", tau)]] <- confidence_int_normalinverse(alpha=0.1,y=BDD_dum$pulse_rate,x=as.matrix(BDD_dum[-1]),tau)
}



plotter <- function(lev, ln, taus)
{
  coeffs <- as.matrix.data.frame(data.frame(lapply(lev, function(l)unlist(l[[1]]))))
  ev_l <-   as.matrix.data.frame(data.frame(lapply(lev, function(l)unlist(l[[2]]))))
  ev_h <-   as.matrix.data.frame(data.frame(lapply(lev, function(l)unlist(l[[3]]))))
  n_l <-    as.matrix.data.frame(data.frame(lapply(ln, function(l)unlist(l[[1]]))))
  n_h <-    as.matrix.data.frame(data.frame(lapply(ln, function(l)unlist(l[[2]]))))
  
  for(i in seq(nrow(coeffs)))
  {
    if(i%%6==1)
    {
      show("new")
      show(i%%6)
      jpeg(paste("CI", taus[1], i%/%6, ".jpeg"), height = 700, width = 500)
      par(mfrow = c(3, 2))
      }
    m <-min(c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,]))[c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,]))!=min(c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,])))])
    M <-max(c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,]))[c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,]))!=max(c(c(coeffs[i, ]), c(ev_l[i, ]), c(ev_h[i, ]), c(n_l[i, ]), c(n_h[i,])))])
    plot(x = taus, y = coeffs[i, ], ylim = c(m, M), type = "b", xlab = expression(tau), ylab = "estimate")
    lines(x = taus,y = ev_h[i,], type = "b", col = "blue")
    lines(x = taus,y = ev_l[i,], type = "b", col = "blue")
    lines(x = taus,y = n_h[i,] ,type = "b", col = "red")
    lines(x = taus,y = n_l[i,] ,type = "b", col = "red")
    abline(h=0)
    title(main = substring(rownames(coeffs)[i], first = 2, 10000) )
    if(i%%6==0){dev.off()}
  }
  dev.off()
}
plotter(beta_tau_faibles, beta_tau_normaux_faibles, taus)
plotter(beta_tau_eleves, beta_tau_normaux_eleves, 1-taus)
length(beta_tau_eleves$`tau =  0.01`[[1]])
beta_tau_faibles

# summary(BDD_dum$medicaid)
# summary(BDD_dum$`not working`)
# summary(BDD_dum$`no insurance`)
# summary(BDD_dum$`:|`)
# summary(BDD_dum$`:((`)
# summary(BDD_dum$`:)`)
# summary(BDD_dum$`:))`)
# summary(BDD_dum$`sometimes`)
# summary(BDD_dum$`often`)
# summary(BDD_dum$`never`)
summary(BDD_dum$`BMI`)

