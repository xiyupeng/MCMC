library(rstan)
h_model = "
data {
int<lower=0> N;
real<lower=0> x[N];
int<lower=0> y[N];
}

parameters {
real<lower=0> beta;
real<lower=0> mu;
real<lower=0,upper=1> lambda[N];
}
transformed parameters {
real<lower=0> alpha;
alpha = beta*mu;
}
model {
for(i in 1:N){
y[i] ~ poisson(x[i]*lambda[i]);
lambda[i] ~ gamma(alpha,beta);
}
mu ~cauchy(0,1);
beta ~ cauchy(0,1);
}
"
d = data.frame(year=1976:1985,
               fatal_accidents = c(24,25,31,31,22,21,26,20,16,22),
               passenger_deaths = c(734,516,754,877,814,362,764,809,223,1066),
               death_rate = c(0.19,0.12,0.15,0.16,0.14,0.06,0.13,0.13,0.03,0.15))
d$miles_flown = d$passenger_deaths/d$death_rate # 100 million miles
dat=list(y<-d$fatal_accidents, x<-d$miles_flow, N=nrow(d))

m = stan_model(model_code=h_model)
re = sampling(m, dat, c("lambda","mu","beta"))