data{
  int N;
  vector[N] U;
  vector[N] wta;
  int E[N];
  int S[N];
  int EA[N];
  int M[N];
  int RG[N];
  int RL[N];
  vector[N] FI;
  vector[N] W;
  vector[N] FS;
  vector[N] mu_mu;
  vector[N] SN;
  vector<lower=0>[9] alpha;
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
  real<lower=0> sigma_E[2];
  real a[2];
  real b_mu;
  vector[24] bS[2];
  real<lower=0> sigma_S[2];
  vector[2] bM[2];
  real<lower=0> sigma_M[2];
  vector[2] bF[2];
  real<lower=0> sigma_F[2];
  real bF_mu[2];
  vector[2] bW[2];
  real<lower=0> sigma_W[2];
  real bW_mu[2];
  vector[2] bFS[2];
  real bFS_mu[2];
  real<lower=0> sigma_FS[2];
  real<lower=0> sigma;
  vector[2] bSN[2];
  real<lower=0> sigma_SN[2];
  real bSN_mu[2];
  vector[2] bEA[2];
  real<lower=0> sigma_EA[2];
  real bEA_mu[2];
  simplex[9] delta[2,2];
  vector<lower=0>[2] Sigma_RG[2];
  cholesky_factor_corr[2] L_Rho_RG[2];
  matrix[2,2] z_RG[2]; 
  vector<lower=0>[2] Sigma_RL[2];
  cholesky_factor_corr[2] L_Rho_RL[2];
  matrix[2,2] z_RL[2]; 
}
transformed parameters{
  matrix[2,2] bRG[2];
  matrix[2,2] bRL[2];
 for(i in 1:2){
    bRG[i] = (diag_pre_multiply(Sigma_RG[i], L_Rho_RG[i]) * z_RG[i])';
    bRL[i] = (diag_pre_multiply(Sigma_RL[i], L_Rho_RL[i]) * z_RL[i])';
     }
}

model{
  vector[N] mu;
  vector[N] p;
  vector[10] delta_j[2,2];
  for(i in 1:2){
    for(j in 1:2){
      delta[i,j] ~ dirichlet( alpha);
      delta_j[i,j] = append_row(0, delta[i,j]);
    }
  }
  sigma ~ exponential( 1 );
  a[1] ~ normal( 2.6 , 0.5 );
  a[2] ~ normal( 3 , 1 );
  mu_mu ~ normal( 0 , 1 );
  //mu_u ~ exponential(2);
  for(i in 1:2) sigma_E[i] ~ exponential( 2 );
  for(i in 1:2) bE[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) sigma_S[i] ~ exponential( 2 );
  for(i in 1:2) bS[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) sigma_M[i] ~ exponential( 2 );
  for(i in 1:2) bM[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) sigma_F[i] ~ exponential( 2 );
  for(i in 1:2) bF[i] ~ normal( 0 , .5 );
  for(i in 1:2) bF_mu[i] ~ normal( 0 , .5 );
  for(i in 1:2) sigma_SN[i] ~ exponential( 2 );
  for(i in 1:2) bSN[i] ~ normal( 0 , .5 );
  for(i in 1:2) bSN_mu[i] ~ normal( 0 , .5 );
  for(i in 1:2) sigma_FS[i] ~ exponential( 2 );
  for(i in 1:2) bFS[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) bFS_mu[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) sigma_W[i] ~ exponential( 2 );
  for(i in 1:2) bW[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) bW_mu[i] ~ normal( 0 , 0.5 );
  for(i in 1:2) sigma_EA[i] ~ exponential( 2 );
  for(i in 1:2) bEA[i] ~ normal( 0 , .5 );
  for(i in 1:2) bEA_mu[i] ~ normal( 0 , .5 );
  for(i in 1:2) L_Rho_RG[i] ~ lkj_corr_cholesky( 2 );
  for(i in 1:2) Sigma_RG[i] ~ exponential( 2 );
  for(i in 1:2) to_vector( z_RG[i] ) ~ normal( 0 , 0.5 );
  for(i in 1:2) L_Rho_RL[i] ~ lkj_corr_cholesky( 2 );
  for(i in 1:2) Sigma_RL[i] ~ exponential( 2 );
  for(i in 1:2) to_vector( z_RL[i] ) ~ normal( 0 , 0.5 );
  
  
  for ( i in 1:N ) {
    mu[i] =  a[1] + bE[1][E[i]] * sigma_E[1] + bS[1][S[i]]*sigma_S[1] +  bM[1][M[i]]*sigma_M[1]  + (bF_mu[1]+bF[1][M[i]]*sigma_F[1])*FI[i] + (bW_mu[1]+bW[1][M[i]]*sigma_W[1])*W[i]  + (bFS_mu[1]+bFS[1][M[i]]*sigma_FS[1])*FS[i]  + (bSN_mu[1]+bSN[1][M[i]]*sigma_SN[1])*SN[i] + bRG[1][M[i], RG[i]] + bRL[1][M[i], RL[i]] + (bEA[1][M[i]]*sigma_EA[1] + bEA_mu[1])*sum(delta_j[1][M[i]][1:EA[i]]) + b_mu*mu_mu[i];
    p[i] =   a[2] + bE[2][E[i]] * sigma_E[2] + bS[2][S[i]]*sigma_S[2] +  bM[2][M[i]]*sigma_M[2]  + (bF_mu[2]+bF[2][M[i]]*sigma_F[2])*FI[i] + (bW_mu[2]+bW[2][M[i]]*sigma_W[2])*W[i] + (bFS_mu[2]+bFS[2][M[i]]*sigma_FS[2])*FS[i]  + (bSN_mu[2]+bSN[2][M[i]]*sigma_SN[2])*SN[i] + bRG[2][M[i], RG[i]] + bRL[2][M[i], RL[i]] + (bEA[2][M[i]]*sigma_EA[2] + bEA_mu[2])*sum(delta_j[2][M[i]][1:EA[i]]);
  }
  
  for(i in 1:N){
    if(Y[i]== 1){
      wta_tru[i] ~ normal( mu[i] , sigma);
      wta[i] ~ normal( wta_tru[i] , U[i]);
      
    }
  }
  Y ~ bernoulli_logit(p);
  //u ~ exponential(mu_u);
}
