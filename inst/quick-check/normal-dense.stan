data {
  int N;
  int P;

  vector[N] y;
  matrix[N,P] X;

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

  mu = X * betas;

  y ~ normal(mu, sigma);
}


