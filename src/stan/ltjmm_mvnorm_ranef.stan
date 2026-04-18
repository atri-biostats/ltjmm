// LTJMM with multivariate normal random effects
data {
  int<lower=1> N_obs;                 // Number of observations
  int<lower=1> N_sub;                 // Number of subjects
  int<lower=2> N_out;                 // Number of outcomes

  int<lower=1> N_X;                
  matrix[N_obs, N_X] X;               // Fixed effects

  vector[N_obs] y;                    // Outcome
  vector[N_obs] obs_time;             // Observation times (should be centered)

  array[N_obs] int<lower=1, upper=N_out> outcome;
  array[N_obs] int<lower=1, upper=N_sub> subject;
}

parameters {
  // Fixed effects
  matrix[N_out, N_X] beta;
  vector<lower=0>[N_out] gamma;
  vector<lower=0>[N_out] sigma_y;

  // Global time-shift random effect (non-centered)
  vector[N_sub] delta_raw;
  real<lower=0> sigma_delta;

  // Multivariate random effects:
  // (N_out - 1) intercepts + N_out slopes
  cholesky_factor_corr[2 * N_out - 1] Lcorr;
  vector<lower=0>[2 * N_out - 1] sigma_diag;
  matrix[2 * N_out - 1, N_sub] z_alpha;
}

transformed parameters {
  // Global time shift
  vector[N_sub] delta = sigma_delta * delta_raw;

  // Multivariate random effects
  matrix[2 * N_out - 1, 2 * N_out - 1] sigma_L =
    diag_pre_multiply(sigma_diag, Lcorr);

  matrix[N_sub, 2 * N_out - 1] alpha =
    (sigma_L * z_alpha)';

  // Random intercepts (sum-to-zero) and slopes
  matrix[N_sub, N_out - 1] alpha0_raw = alpha[, 1:(N_out - 1)];
  matrix[N_sub, N_out] alpha0;
  matrix[N_sub, N_out] alpha1 = alpha[, N_out:(2 * N_out - 1)];

  vector[N_obs] mu;

  // Enforce sum-to-zero constraint for intercepts
  for (s in 1:N_sub) {
    for (j in 1:(N_out - 1))
      alpha0[s, j] = alpha0_raw[s, j];
    alpha0[s, N_out] = -sum(alpha0_raw[s,]);
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
  gamma ~ normal(0, 1);

  delta_raw ~ normal(0, 1);
  sigma_delta ~ normal(0, 1);

  to_vector(z_alpha) ~ normal(0, 1);
  Lcorr ~ lkj_corr_cholesky(2);
  sigma_diag ~ normal(0, 1);

  sigma_y ~ normal(0, 1);

  // Likelihood (vectorized)
  y ~ normal(mu, sigma_y[outcome]);
}

generated quantities {
  matrix[2 * N_out - 1, 2 * N_out - 1] Omega;
  matrix[2 * N_out - 1, 2 * N_out - 1] Sigma;
  vector[N_obs] log_lik;

  Omega = multiply_lower_tri_self_transpose(Lcorr);
  Sigma = quad_form_diag(Omega, sigma_diag);

  // Pointwise log-likelihood (required for LOO)
  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(y[n] | mu[n], sigma_y[outcome[n]]);
  }
}
