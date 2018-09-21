data {
  int N;
  int P;

  vector[N] y;

  int N_NZE;
  int NZE[NNZE];
  int starts[N];
  int stops[N];
  vector[NNZE] Xv;
}

parameters {
  vector[P] betas;
  real<lower = 0> sigma;
}

model { 
  P ~ normal(0, 1);
  sigma ~ normal(0, 1);
  for (i in 1:N) {
    y ~ normal(sum(Xv[starts[i]:stops[i]] .* betas[NZE[starts[i]:stops[i]]]), sigma);
  }
}


