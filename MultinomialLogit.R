setwd("C:/Users/Rayyan Sayeed/Documents/Northwestern Spring 2018/MKTG 551-3/Homework 1")
hh100<- read.csv("hh100.csv",header=T)
hh100$ints<-rep(1,length(hh100$hh.id)) # add column of 1's, will need later, alphas no coeff

# theta vector; (a1,a2,a3,bf,bp)
theta<-c(2,0,-2,1,-30) # true theta 
Ltheta<-function(theta,x){
# identify trip block
for(i in 1:length(x$hh.id)){ 
  if(i%%4==1){
    denom<-0 # zero the denominator at beginning of each trip
  }
  item<-x$alt.id[i] # assign proper item id
  if(item==4) beta <- c(theta[4],theta[5],0) else beta<-c(theta[4],theta[5],theta[item]) # define beta
  denom<-denom + exp(beta %*% t(as.matrix(x[i,c("feat","price","ints")]))) # update denom
  num<-exp(beta %*% t(as.matrix(x[i,c("feat","price","ints")]))) # define numerator
  Prob<-num # define choice probability
  x$Prob[i]<-Prob
  
  if(item==4){ # make sure denom gets used for every point in trip
    for(j in 0:3){
    x$Prob[i-j]<- x$Prob[i-j]/denom
    }
  }
}
  output<- t(as.matrix(x$y)) %*% log(as.matrix(x$Prob)) # log-likelihood formula
  return(-output)
}

result <-nlm(Ltheta,theta,hh100,print.level= 2,hessian=T) # optimize

theta_b<-c(rep(0,4),-10)
theta_c<-c(rep(100,4),-100)

# optimizing using BFGS and Nelder-Mead methods for starting vectors theta_b, theta_c:

BFGS_b=optim(theta_b,Ltheta,gr = NULL,hh100, method="BFGS",control=list(trace=1))
Nelder_b=optim(theta_b,Ltheta,gr = NULL,hh100, method="Nelder-Mead",control=list(trace=1))
BFGS_c=optim(theta_c,Ltheta,gr = NULL,hh100, method="BFGS",control=list(trace=1))
Nelder_c=optim(theta_c,Ltheta,gr = NULL,hh100, method="Nelder-Mead",control=list(trace=1))

# profile likehlihood plots

# setting alpha hat and beta_p hat values from BFGS_b
alpha_hat=2.3063363
betap_hat=-34.547

half_int=10 # setting half interval length, whole int = alpha hat +/- half int
step_size=.5

alpha_vals=c(seq(beta_p-half_int,beta_p+half_int,step_size)) # defining domain
profile_vals=c(rep(0,length(alpha_vals))) # initialize profile likelihood

for (i in 1:length(alpha_vals)){
  new_theta=c(BFGS_b$par)
  new_theta[5]=alpha_vals[i]
  profile_vals[i]=Ltheta(new_theta,hh100)
}

plot(alpha_vals,profile_vals)

# coefficient estimates given by result$estimate

std_error=(1/sqrt(8000))*(1/sqrt(diag(result$hessian))) # compute standard error

# log-likelihood at optimum is 2055.144

BIC=-2*(2055.144)+(5)*log(8000) # compute BIC

betap_hat=-34.547
theta = c(2.305,0.275,-1.783,0.839,-34.522)

# the following function returns probability of ith item
for(i in 1:length(hh100$hh.id)){ 
  if(i%%4==1){
    denom<-0 # zero the denominator at beginning of each trip
  }
  item<-hh100$alt.id[i] # assign proper item id
  if(item==4) beta <- c(theta[4],theta[5],0) else beta<-c(theta[4],theta[5],theta[item]) # define beta
  denom<-denom + exp(beta %*% t(as.matrix(hh100[i,c("feat","price","ints")]))) # update denom
  num<-exp(beta %*% t(as.matrix(hh100[i,c("feat","price","ints")]))) # define numerator
  Prob<-num # define choice probability
  hh100$Prob[i]<-Prob
  
  if(item==4){ # make sure denom gets used for every point in trip
    for(j in 0:3){
      hh100$Prob[i-j]<- hh100$Prob[i-j]/denom
    }
  }
}
# calculate own-price elasticity
own_elasticity=c(rep(0,length(hh100$hh.id))) # initialize own-price elasticity

for (i in 1:length(hh100$hh.id)){
  own_elasticity[i]=-betap_hat*hh100$price[i]*(1-hh100$Prob[i])
}

mean(own_elasticity)

# calculate cross-price elasticity

cross_elasticity=c(rep(0,length(hh100$hh.id))) # initialize cross-price elasticity
for (i in 1:length(hh100$hh.id)){
  cross_elasticity[i]=betap_hat*hh100$price[i]*(hh100$Prob[i])
}

mean(cross_elasticity)








