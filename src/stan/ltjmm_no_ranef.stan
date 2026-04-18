// LTJMM with univariate normal distributions on random effects

data{
	// Define variables in data
	int<lower=0> N_obs;              // Number of observations
	int<lower=0, upper=N_obs> N_sub; // Number of subjects
	int<lower=0, upper=N_obs> N_out; // Number of outcomes

	int<lower=0> N_X;                
	matrix[N_obs, N_X] X;            // fixed effects

	vector[N_obs] y;                 // outcome
	vector[N_obs] obs_time;          // observation times
  
	array[N_obs] int<lower=1, upper=N_out> outcome;      // Outcome index
	array[N_obs] int<lower=1, upper=N_sub> subject;      // Subject index
}

parameters{
	matrix[N_out, N_X] beta;        // parameter vector of observed covariates
	vector[N_sub] delta_raw;        // time shift random effect: normally distributed with mean 0
	vector<lower=0>[N_out] gamma;   // restrict gamma to be positive for identifiability
	vector<lower=0>[N_out] sigma_y; // population standard deviation 
	real<lower=0> sigma_delta;      // standard deviation of time shift random effect
}

transformed parameters{
	vector[N_obs] mu;
	vector[N_sub] delta = sigma_delta * delta_raw;

	for (n in 1:N_obs) {
	  int j = outcome[n];
	  int i = subject[n];
	  mu[n] = dot_product(X[n], beta[j]) + gamma[j] * (obs_time[n] + delta[i]);
	}
}

model{	
	// Priors
	gamma ~ normal(0, 1);
	sigma_delta ~ normal(0, 1);
	sigma_y ~ normal(0, 1);
	delta_raw ~ normal(0, 1);
	
	for(n_out in 1:N_out){
		beta[n_out] ~ normal(0, 10.0);
	}
  
	// Likelihood
	y ~ normal(mu, sigma_y[outcome]);
}

generated quantities {
  vector[N_obs] log_lik;

  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma_y[outcome[n]]);
  }
}
