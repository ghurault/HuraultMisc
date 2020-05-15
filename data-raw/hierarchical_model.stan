data {
  int<lower = 0> N; // Number of observations
  int<lower = 0> N_subject; // Number of subjects
  int<lower = 1> N_repeat[N_subject]; // Number of observations per subject
  vector[N] y; // Data
  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood
}

transformed data {
  int start[N_subject]; // Index of first observation for each subject
  int end[N_subject]; // Index of last observation for each subject

  for (k in 1:N_subject) {
    if (k == 1) {
      start[k] = 1;
    } else {
      start[k] = end[k - 1] + 1;
    }
    end[k] = start[k] - 1 + N_repeat[k];
  }
}

parameters {
  real mu_pop;
  real<lower = 0> sigma_pop;
  vector[N_subject] eta_mu;
  real<lower = 0> sigma;
}

transformed parameters {
  vector[N_subject] mu = mu_pop + sigma_pop * eta_mu;
}

model {
  // Priors
  mu_pop ~ std_normal();
  sigma_pop ~ normal(2, 1);
  sigma ~ normal(0, 1);
  eta_mu ~ std_normal();

  // Likelihood
  if (run == 1) {
    for (k in 1:N_subject) {
      y[start[k]:end[k]] ~ normal(mu[k], sigma);
    }
  }

}

generated quantities {
  real y_rep[N]; // Replications

  for (k in 1:N_subject) {
    for (i in start[k]:end[k]) {
      y_rep[i] = normal_rng(mu[k], sigma);
    }
  }
}
