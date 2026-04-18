// Stacked mixed model with independent normal random effects
data {
  int<lower=1> N_obs;
  int<lower=1> N_sub;
  int<lower=1> N_out;

  int<lower=1> N_X;
  matrix[N_obs, N_X] X;            // includes intercept if desired

  vector[N_obs] y;
  vector[N_obs] obs_time;          // should be centered in preprocessing

  array[N_obs] int<lower=1, upper=N_out> outcome;
  array[N_obs] int<lower=1, upper=N_sub> subject;
}

parameters {
  // Fixed effects
  matrix[N_out, N_X] beta;
  vector[N_out] gamma;
  vector<lower=0>[N_out] sigma_y;

  // Non-centered random intercepts
  matrix[N_sub, N_out] alpha0_raw;
  vector<lower=0>[N_out] sigma_alpha0;

  // Non-centered random slopes
  matrix[N_sub, N_out] alpha1_raw;
  vector<lower=0>[N_out] sigma_alpha1;
}

transformed parameters {
  matrix[N_sub, N_out] alpha0;
  matrix[N_sub, N_out] alpha1;
  vector[N_obs] mu;

  // Non-centered transforms
  for (j in 1:N_out) {
    alpha0[, j] = sigma_alpha0[j] * alpha0_raw[, j];
    alpha1[, j] = sigma_alpha1[j] * alpha1_raw[, j];
  }

  // Linear predictor
  for (n in 1:N_obs) {
    int j = outcome[n];
    int s = subject[n];

    mu[n] =
      dot_product(X[n], beta[j]) +
      gamma[j] * obs_time[n] +
      alpha0[s, j] +
      alpha1[s, j] * obs_time[n];
  }
}

model {
  // Priors
  to_vector(beta) ~ normal(0, 10);
  gamma ~ normal(0, 1);

  to_vector(alpha0_raw) ~ normal(0, 1);
  to_vector(alpha1_raw) ~ normal(0, 1);

  sigma_alpha0 ~ normal(0, 1);
  sigma_alpha1 ~ normal(0, 1);
  sigma_y ~ normal(0, 1);

  // Likelihood (vectorized)
  y ~ normal(mu, sigma_y[outcome]);
}

generated quantities {
  vector[N_obs] log_lik;

  // Pointwise log-likelihood for LOO / WAIC
  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma_y[outcome[n]]);
  }
}