data{
  int N;
  int E[N];
  int S[N];
  int EA[N];
  int M[N];
  int RG[N];
  int RL[N];
  vector[N] FI;
  vector[N] W;
  vector[N] WF;
  vector[N] SN;
  vector[N] FS;
  vector[N] p_mu;
  int Y[N];
  vector<lower=0>[9] alpha;
}

parameters{
  vector[6] bE;
  real<lower=0> sigma_E;
  real a;
  real b_mu;
  vector[24] bS;
  real<lower=0> sigma_S;
  vector[2] bM;
  real<lower=0> sigma_M;
  vector[2] bF;
  real<lower=0> sigma_F;
  real bF_mu;
  vector[2] bW;
  real<lower=0> sigma_W;
  real bW_mu;
  vector[2] bWF;
  real<lower=0> sigma_WF;
  real bWF_mu;
  vector[2] bFS;
  real<lower=0> sigma_FS;
  real bFS_mu;
  vector[2] bSN;
  real<lower=0> sigma_R;
  real bR_mu;
  vector[2] bR;
  real<lower=0> sigma_SN;
  real bSN_mu;
  vector[2] bEA;
  real<lower=0> sigma_EA;
  real bEA_mu;
  simplex[9] delta[2];
  vector<lower=0>[2] Sigma_RG;
  cholesky_factor_corr[2] L_Rho_RG;
  matrix[2,2] z_RG; 
  vector<lower=0>[2] Sigma_RL;
  cholesky_factor_corr[2] L_Rho_RL;
  matrix[2,2] z_RL; 
}
transformed parameters{
  matrix[2,2] bRG;
  matrix[2,2] bRL;
  bRG = (diag_pre_multiply(Sigma_RG, L_Rho_RG) * z_RG)';
  bRL = (diag_pre_multiply(Sigma_RL, L_Rho_RL) * z_RL)';

}

model{
  vector[N] p;
  vector[10] delta_j[2];
  for(i in 1:2){
    delta[i] ~ dirichlet( alpha);
    delta_j[i] = append_row(0, delta[i]);
  }
  a ~ normal( -2 , 1 );
  b_mu ~ normal( -2 , 1 );
  //mu_u ~ exponential(2);
   sigma_E ~ exponential( 2 );
   bE ~ normal( 0 , 0.5 );
   sigma_S ~ exponential( 2 );
   bS ~ normal( 0 , 0.5 );
   sigma_M ~ exponential( 2 );
   bM ~ normal( 0 , 0.5 );
   sigma_F ~ exponential( 2 );
   bF ~ normal( 0 , .5 );
   bF_mu ~ normal( 0 , .5 );
   sigma_W ~ exponential( 2 );
   bW ~ normal( 0 , .5 );
   bW_mu ~ normal( 0 , .5 );
   sigma_WF ~ exponential( 2 );
   bWF ~ normal( 0 , .5 );
   bWF_mu ~ normal( 0 , .5 );
   sigma_FS ~ exponential( 2 );
   bFS ~ normal( 0 , .5 );
   bFS_mu ~ normal( 0 , .5 );
   sigma_SN ~ exponential( 2 );
   bSN ~ normal( 0 , .5 );
   bSN_mu ~ normal( 0 , .5 );
   bEA ~ normal(0, .5);
   sigma_EA ~ exponential(2);
   bEA_mu  ~ normal(0, 0.5);
   bR ~ normal(0, .5);
   sigma_R ~ exponential(2);
   bR_mu  ~ normal(0, 0.5);
   L_Rho_RG ~ lkj_corr_cholesky( 2 );
   Sigma_RG ~ exponential( 2 );
   to_vector( z_RG ) ~ normal( 0 , 0.5 );
   L_Rho_RL ~ lkj_corr_cholesky( 2 );
   Sigma_RL ~ exponential( 2 );
   to_vector( z_RL ) ~ normal( 0 , 0.5 );
  
  
  for ( i in 1:N ) {
      p[i] =   a + p_mu[i]*b_mu + bE[E[i]] * sigma_E + bS[S[i]]*sigma_S +  bM[M[i]]*sigma_M + (bWF_mu+bWF[M[i]]*sigma_WF)*WF[i]  + (bF_mu+bF[M[i]]*sigma_F)*FI[i] + (bW_mu+bW[M[i]]*sigma_W)*W[i] + (bFS_mu+bFS[M[i]]*sigma_FS)*FS[i]  + (bSN_mu+bSN[M[i]]*sigma_SN)*SN[i] + bRG[M[i], RG[i]] + bRL[M[i], RL[i]] +  (bEA[M[i]]*sigma_EA + bEA_mu)*sum(delta_j[M[i]][1:EA[i]]);
  }
  
  Y ~ bernoulli_logit(p);
  //u ~ exponential(mu_u);
}