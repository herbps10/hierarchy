data {
  int N;
  int P;

  vector[N] y;

  int n_nze;
  int nze[n_nze];
  int start[N];
  int stop[N];
  vector[n_nze] X_vec;
}

parameters {
  vector[P] betas;
  vector[2] alphas;
  real<lower = 0> sigma;
}

model { 
  vector[N] mu;

  alphas ~ normal(0, 1);
  betas ~ normal(0, 1);
  sigma ~ normal(0, 1);

  for (i in 1:N)
    mu[i] = sum(X_vec[start[i]:stop[i]] .* betas[nze[start[i]:stop[i]]]);
    
  y ~ normal(mu, sigma);
}


