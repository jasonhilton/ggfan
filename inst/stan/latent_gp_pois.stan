// This model is based on the example models provided in the stan package.
// A more recent implementation which is likely to be more efficient can be found here https://github.com/stan-dev/example-models/blob/master/misc/gaussian-process/gp-fit-pois.stan
data {
  int<lower=1> N;
  vector[N] x;
  int<lower=0> y[N,2];
}
transformed data {
  vector[N] mu;
  for (i in 1:N)
    mu[i] = 0;
}
parameters {
  real<lower=0> eta_sq;
  real<lower=0> rho_sq;
  vector[N] z;
}
model {
  matrix[N,N] Sigma;
  
  // off-diagonal elements
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      Sigma[i,j] = eta_sq * exp(-rho_sq * pow(x[i] - x[j],2));
      Sigma[j,i] = Sigma[i,j]; // for symmetry
    }
  }
  
  // diagonal elements
  for (k in 1:N){
    Sigma[k,k] = eta_sq + 0.001; // nugget for numeric purposes
  }
  
  eta_sq ~ cauchy(0,5);
  rho_sq ~ cauchy(0,5);
  
  z ~ multi_normal(mu, Sigma);
  for (j in 1:2){
    y[,j] ~ poisson_log(z);
  }
}
generated quantities {
  int y_gen[N];
  for (i in 1:N){
    // only sample one observation per x
    y_gen[i] = poisson_rng(exp(z[i]));
  }
}
