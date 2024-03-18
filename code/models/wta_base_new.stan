data{
  int N;
  vector[N] u;
  vector[N] wta;
  array[N] int E;
  array[N] int S;
}
transformed data{
  int Y[N];
  for(i in 1:N){
    if(wta[i] > 0) Y[i] = 1;
    if(wta[i] == 0) Y[i] = 0;
  }
}

parameters{
  vector[N] wta_tru;
  vector[6] bE[2];
  real<lower=0> sigma_e[2];
  real a[2];
  vector[24] bS[2];
  real<lower=0> sigma_S[2];
  real<lower=0> sigma;
  //real<lower=0> mu_u;
}
model{
  vector[N] mu;
  vector[N] p;
  sigma ~ exponential( 1 );
  a[1] ~ normal( 13 , 2 );
  a[2] ~ normal( 2 , 1 );
  //mu_u ~ exponential(2);
  for(i in 1:2) sigma_e[i] ~ exponential( .5 );
  for(i in 1:2) bE[i] ~ normal( 0 , 1 );
  for(i in 1:2) sigma_S[i] ~ exponential( 1 );
  for(i in 1:2) bS[i] ~ normal( 0 , 1 );
  for ( i in 1:N ) {
    mu[i] =  a[1] + bE[1][E[i]] * sigma_e[1] + bS[1][S[i]]*sigma_S[1];
    p[i] =   a[2] + bE[2][E[i]] * sigma_e[2] + bS[2][S[i]]*sigma_S[2];
  }
  
  for(i in 1:N){
    if(Y[i]== 1){
      wta_tru[i] ~ normal( mu[i] , sigma);
      wta[i] ~ normal( wta_tru[i] , u[i]);
      
    }
  }
  Y ~ bernoulli_logit(p);
  //u ~ exponential(mu_u);
}
