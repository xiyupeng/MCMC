log_q_mu = function(lambda, mu, beta) {
  if (mu<0) return(-Inf)
  n = length(lambda)
  n*mu*beta*log(beta)-n*log(gamma(mu*beta))+(mu*beta-1)*sum(log(lambda))-log(1+mu^2)
}

log_q_beta = function(lambda, mu, beta) {
  if (beta<0) return(-Inf)
  n = length(lambda)
  n*mu*beta*log(beta)-n*log(gamma(mu*beta))+(mu*beta-1)*sum(log(lambda))-beta*sum(lambda)-log(1+beta^2)
}

mcmc = function(n_sims, dat, inits, tune) {
  n_groups= nrow(dat)
  mu = inits$mu
  beta= inits$beta
  # Recording structure
  lambda_keep = matrix(NA, nrow=n_sims, ncol=n_groups)
  mu_keep = rep(mu, n_sims)
  beta_keep= rep(beta , n_sims)
  
  for (i in 1:n_sims) {
    # Sample thetas
    lambda = with(dat, rgamma(length(y), mu*beta+y, beta+x))
    # Sample mu
    mu_prop = rnorm(1, mu, tune$mu)
    logr = log_q_mu(lambda, mu_prop, beta)-log_q_mu(lambda, mu, beta)
    mu = ifelse(log(runif(1))<logr, mu_prop, mu)
    # Sample beta
    beta_prop = rnorm(1, beta, tune$beta)
    logr = log_q_beta(lambda, mu, beta_prop)-log_q_beta(lambda, mu, beta)
    beta = ifelse(log(runif(1))<logr, beta_prop, beta)
    # Record parameter values
    lambda_keep[i,] = lambda
    mu_keep[i]= mu
    beta_keep[ i]= beta
  }
  
  return(list(mu=mu_keep,beta=beta_keep,lambda=lambda_keep))
}

d = data.frame(year=1976:1985,
               fatal_accidents = c(24,25,31,31,22,21,26,20,16,22),
               passenger_deaths = c(734,516,754,877,814,362,764,809,223,1066),
               death_rate = c(0.19,0.12,0.15,0.16,0.14,0.06,0.13,0.13,0.03,0.15))
d$miles_flown = d$passenger_deaths/d$death_rate # 100 million miles
dat=data.frame(y<-d$fatal_accidents, x<-d$miles_flown)
inits = list(mu=y[1]/x[1], beta=x[1])
tune=list(mu=0.005,beta=5000)
# Run the MCMC
r = mcmc(2000, dat=dat, inits=inits, tune)
