// LTJMM with univariate normal distributions on random effects
data {
  int<lower=1> N_obs;                 // Number of observations
  int<lower=1> N_sub;                 // Number of subjects
  int<lower=2> N_out;                 // Number of outcomes

  int<lower=1> N_X;                
  matrix[N_obs, N_X] X;               // Fixed effects design matrix

  vector[N_obs] y;                    // Outcome
  vector[N_obs] obs_time;             // Observation times (preferably centered)

  array[N_obs] int<lower=1, upper=N_out> outcome;   // Outcome index
  array[N_obs] int<lower=1, upper=N_sub> subject;   // Subject index
}

parameters {
  // Fixed effects
  matrix[N_out, N_X] beta;

  // Global time-shift random effect (non-centered)
  vector[N_sub] delta_raw;
  real<lower=0> sigma_delta;

  // Outcome-specific slopes and residual SDs
  vector<lower=0>[N_out] gamma;
  vector<lower=0>[N_out] sigma_y;

  // Random intercepts (sum-to-zero across outcomes)
  matrix[N_sub, N_out - 1] alpha0_raw;
  vector<lower=0>[N_out - 1] sigma_alpha0;

  // Random slopes (non-centered)
  matrix[N_sub, N_out] alpha1_raw;
  vector<lower=0>[N_out] sigma_alpha1;
}

transformed parameters {
  // Non-centered transformations
  vector[N_sub] delta = sigma_delta * delta_raw;

  matrix[N_sub, N_out] alpha1 =
    diag_post_multiply(alpha1_raw, sigma_alpha1);

  matrix[N_sub, N_out] alpha0;

  vector[N_obs] mu;

  // Sum-to-zero constraint for random intercepts
  for (s in 1:N_sub) {
    for (j in 1:(N_out - 1))
      alpha0[s, j] = sigma_alpha0[j] * alpha0_raw[s, j];

    alpha0[s, N_out] = -sum(alpha0[s, 1:(N_out - 1)]);
  }

  // Linear predictor
  for (n in 1:N_obs) {
    int j = outcome[n];
    int s = subject[n];

    mu[n] =
      dot_product(X[n], beta[j]) +
      gamma[j] * (obs_time[n] + delta[s]) +
      alpha0[s, j] +
      alpha1[s, j] * obs_time[n];
  }
}

model {
  // Priors
  to_vector(beta) ~ normal(0, 10);

  delta_raw ~ normal(0, 1);
  sigma_delta ~ normal(0, 1);

  gamma ~ normal(0, 1);
  sigma_y ~ normal(0, 1);

  to_vector(alpha0_raw) ~ normal(0, 1);
  sigma_alpha0 ~ normal(0, 1);

  to_vector(alpha1_raw) ~ normal(0, 1);
  sigma_alpha1 ~ normal(0, 1);

  // Likelihood (vectorized)
  y ~ normal(mu, sigma_y[outcome]);
}

generated quantities {
  vector[N_obs] log_lik;

  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma_y[outcome[n]]);
  }
}
