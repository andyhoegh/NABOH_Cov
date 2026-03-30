data {
  int<lower=0> total_pools;
  int<lower=1> num_sites;
  int<lower=0,upper=1> y[total_pools];
  int<lower=1,upper=num_sites> site_index[total_pools];
  int<lower=0> pool_size[total_pools];
  real visit2_indicator[total_pools];
  real visit3_indicator[total_pools];
}

parameters {
  real mu;
  real<lower=0> sigma;
  vector[num_sites] beta0;
  real tau2;
  real tau3;
}

model {
  for (i in 1:total_pools){
      y[i] ~ bernoulli(1 - (1 - inv_logit(beta0[site_index[i]] + visit2_indicator[i] * tau2 + visit3_indicator[i] * tau3 ))^pool_size[i]); 
  }
  for (l in 1:num_sites){
    beta0[l] ~ normal(mu, sigma);
  }
  // priors here
  mu ~ normal(-1, 4);
  sigma ~ gamma(.2, 1);
  tau2 ~ normal(0, 2);
  tau3 ~ normal(0, 2);
}

generated quantities {
  vector[total_pools] log_lik;
  for (i in 1:total_pools) {
    log_lik[i] = bernoulli_logit_lpmf(y[i] | 1 - (1 - inv_logit(beta0[site_index[i]] + visit2_indicator[i] * tau2 + visit3_indicator[i] * tau3 ))^pool_size[i]);
  }
}
