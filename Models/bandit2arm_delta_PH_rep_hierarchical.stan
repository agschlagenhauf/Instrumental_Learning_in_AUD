// MILENA MUSIAL 12/2023

// input
data {
  int<lower=1> N; // number of subjects
  int<lower=1> C; // number of conditions (reinforcer_type)
  int<lower=1> T; // total number of trials across subjects
  int<lower=1> MT; // max number of trials / subject / condition
  
  int<lower=1, upper=MT> Tsubj[N, C]; // actual number of trials / subject / condition
  
  int<lower=-999, upper=1> choice[N, MT, C]; // choice of correct (1) or incorrect (0) card / trial / subject / condition
  int<lower=-999, upper=1> outcome[N, MT, C];  // outcome / trial / subject / condition
  
  int kS; // number of subj-level variables (aud_group)
  real subj_vars[N,kS]; //subj-level variable matrix (centered)
  int kV; // number of visit-level variables (reinforcer_type)
  real visit_vars[N,C,kV]; //visit-level variable matrix (centered) (X)
  
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
}

// transformed input 
transformed data {
  vector[2] initV;  // initial values for EV, both choices have a value of 0.5
  initV = rep_vector(0.5, 2);
  real initabsPE;
  initabsPE = 0.5; 
  int initrep; // initial value for choice repetition, always -999 as no previous choice
  initrep = 0;
}

// output - posterior distribution should be sought
parameters {
  
  // Declare all parameters as vectors for vectorizing
  // hyperparameters (group-level means)
  vector[4] mu; // group level means for the 4 parameters

  // Subject-level raw parameters (fixed effect of aud group)
  vector[kS] A_sub_m;    // learning rate
  vector[kS] tau_sub_m;  // inverse temperature
  vector[kS] gamma_sub_m; // decay constant
  vector[kS] pi_sub_m; // // stickiness
  
  // Condition-level raw parameters (fixed effect of reinforcer type)
  vector[kV] A_sub_con_m;    // learning rate
  vector[kV] tau_sub_con_m;  // inverse temperature
  vector[kV] gamma_sub_con_m;  // decay constant
  vector[kV] pi_sub_con_m;  // stickiness
  
  //cross-level interaction effects (fixed interaction effects)
  matrix[kV,kS] A_int_m;
  matrix[kV,kS] tau_int_m;
  matrix[kV,kS] gamma_int_m;
  matrix[kV,kS] pi_int_m;
  
  //visit-level (within subject) SDs
  real<lower=0> A_visit_s;
  real<lower=0> tau_visit_s;
  real<lower=0> gamma_visit_s; 
  real<lower=0> pi_visit_s; 
  
  //SDs of visit-level effects across subjects
  vector<lower=0>[kV+1] A_subj_s;
  vector<lower=0>[kV+1] tau_subj_s;
  vector<lower=0>[kV+1] gamma_subj_s;
  vector<lower=0>[kV+1] pi_subj_s;
  
  //non-centered parameterization (ncp) variance effect per visit & subject
  matrix[N,C] A_visit_raw;
  matrix[N,C] tau_visit_raw;
  matrix[N,C] gamma_visit_raw;
  matrix[N,C] pi_visit_raw;
  
  //NCP variance effect on subj-level effects
  matrix[kV+1,N] A_subj_raw;
  matrix[kV+1,N] tau_subj_raw;
  matrix[kV+1,N] gamma_subj_raw;
  matrix[kV+1,N] pi_subj_raw;
  
  //Cholesky factors of correlation matrices for subj-level variances
  cholesky_factor_corr[kV+1] A_subj_L;
  cholesky_factor_corr[kV+1] tau_subj_L;
  cholesky_factor_corr[kV+1] gamma_subj_L;
  cholesky_factor_corr[kV+1] pi_subj_L;
}

transformed parameters {
  
  // initialize condition-in-subject-level parameters
  matrix<lower=0, upper=1>[N,C] A; // bring alpha to range between 0 and 1
  matrix[N,C] A_normal; // alpha without range
  matrix<lower=0, upper=100>[N,C] tau; // bring tau to range between 0 and 100
  matrix[N,C] tau_normal; // tau without range
  matrix<lower=0, upper=1>[N,C] gamma; // bring gamma to range between 0 and 1
  matrix[N,C] gamma_normal; // gamma without range
  matrix[N,C] pi_normal; // C without range
  
  //convert Cholesky factorized correlation matrix into SDs per visit-level effect (create random intercept and slope variances)
  matrix[N,kV+1] A_vars = (diag_pre_multiply(A_subj_s,A_subj_L)*A_subj_raw)';
  matrix[N,kV+1] tau_vars = (diag_pre_multiply(tau_subj_s,tau_subj_L)*tau_subj_raw)';
  matrix[N,kV+1] gamma_vars = (diag_pre_multiply(gamma_subj_s,gamma_subj_L)*gamma_subj_raw)';
  matrix[N,kV+1] pi_vars = (diag_pre_multiply(pi_subj_s,pi_subj_L)*pi_subj_raw)';
  
  //create transformed parameters using non-centered parameterization for all
  // and logistic transformation for alpha (range: 0 to 1),
  // add in subject and visit-level effects as shifts in means
  
  // compute subject-level parameters
  for (s in 1:N) {
    A_normal[s,]   = mu[1] + A_visit_s*A_visit_raw[s,] + A_vars[s,1]; // overall mean + visit-level variance effect + random intercept per subject
    tau_normal[s,] = mu[2] + tau_visit_s*tau_visit_raw[s,] + tau_vars[s,1];
    gamma_normal[s,] = mu[3] + gamma_visit_s*gamma_visit_raw[s,] + gamma_vars[s,1];
    pi_normal[s,] = mu[4] + pi_visit_s*pi_visit_raw[s,] + pi_vars[s,1];
    
    //add subj- and visit-level effects
    for (v in 1:C) { // for every condition
      
      for (kv in 1:kV) { 
        //main effects of visit-level variables
        A_normal[s,v] += visit_vars[s,v,kv]*(A_sub_con_m[kv]+A_vars[s,kv+1]); // predictor * fixed and random slope
        tau_normal[s,v] += visit_vars[s,v,kv]*(tau_sub_con_m[kv]+tau_vars[s,kv+1]);
        gamma_normal[s,v] += visit_vars[s,v,kv]*(gamma_sub_con_m[kv]+gamma_vars[s,kv+1]);
        pi_normal[s,v] += visit_vars[s,v,kv]*(pi_sub_con_m[kv]+pi_vars[s,kv+1]);
        
        for (ks in 1:kS) { 
          //main effects of subject-level variables
          A_normal[s,v] += subj_vars[s,ks]*A_sub_m[ks]; // predictor * fixed slope
          tau_normal[s,v] += subj_vars[s,ks]*tau_sub_m[ks];
          gamma_normal[s,v] += subj_vars[s,ks]*gamma_sub_m[ks];
          pi_normal[s,v] += subj_vars[s,ks]*pi_sub_m[ks];
          
          //cross-level interactions
          A_normal[s,v] += subj_vars[s,ks]*visit_vars[s,v,kv]*A_int_m[ks,kv];
          tau_normal[s,v] += subj_vars[s,ks]*visit_vars[s,v,kv]*tau_int_m[ks,kv];
          gamma_normal[s,v] += subj_vars[s,ks]*visit_vars[s,v,kv]*gamma_int_m[ks,kv];
          pi_normal[s,v] += subj_vars[s,ks]*visit_vars[s,v,kv]*pi_int_m[ks,kv];
        }
        
      }
    
    }
    
    //transform to range [0,1] or [0,100]
    A[s,] = Phi_approx(A_normal[s,]);
    tau[s,] = Phi_approx(tau_normal[s,])*100;
    gamma[s,] = Phi_approx(gamma_normal[s,]);
    // pi_normal stays in opriginal range
    
  }
  
}

model {
  
  // define prior distributions
  
  // hyperparameters (group-level means)
  mu ~ normal(0, 1);
  
  // Subject-level raw parameters
  A_sub_m ~ normal(0, 1);
  tau_sub_m ~ normal(0, 1);
  gamma_sub_m ~ normal(0, 1);
  pi_sub_m ~ normal(0, 1);
  
  // Condition-level raw parameters
  A_sub_con_m ~ normal(0,1);
  tau_sub_con_m ~ normal(0,1);
  gamma_sub_con_m ~ normal(0,1);
  pi_sub_con_m ~ normal(0,1);
  
  // cross-level interactions
  for (ks in 1:kS) {
    A_int_m[,ks] ~ normal(0,1);
    tau_int_m[,ks] ~ normal(0,1);
    gamma_int_m[,ks] ~ normal(0,1);
    pi_int_m[,ks] ~ normal(0,1);
  }
  
  //visit-level (within subject) SDs
  A_visit_s ~ cauchy(0,2);
  tau_visit_s ~ cauchy(0,2); 
  gamma_visit_s ~ cauchy(0,2);
  pi_visit_s ~ cauchy(0,2); 
  
  //SDs of visit-level effects across subjects
  A_subj_s ~ student_t(3,0,2);
  tau_subj_s ~ student_t(3,0,3);
  gamma_subj_s ~ student_t(3,0,2);
  pi_subj_s ~ student_t(3,0,2);
  
  for (s in 1:N) {
    //non-centered parameterization (ncp) variance effect per visit & subject
    A_visit_raw[s,] ~ normal(0,1);
    tau_visit_raw[s,] ~ normal(0,1);
    gamma_visit_raw[s,] ~ normal(0,1);
    pi_visit_raw[s,] ~ normal(0,1);
    
    //NCP variance effect on subj-level effects
    to_vector(A_subj_raw[,s]) ~ normal(0,1);
    to_vector(tau_subj_raw[,s]) ~ normal(0,1);
    to_vector(gamma_subj_raw[,s]) ~ normal(0,1);
    to_vector(pi_subj_raw[,s]) ~ normal(0,1);
  }
  
  //Cholesky factors of correlation matrices for subj-level variances
  // lkj distribution with shape parameter η = 1.0 is a uniform prior; set to 2 to 
  // imply no correlation between random intercepts and slopes (Sorensen & Vasishth, 2016) 
  A_subj_L ~ lkj_corr_cholesky(1);
  tau_subj_L ~ lkj_corr_cholesky(1);
  gamma_subj_L ~ lkj_corr_cholesky(1);
  pi_subj_L ~ lkj_corr_cholesky(1);
  
  // only execute this part if we want to evaluate likelihood (fit real data)
  if (run_estimation==1){

    // subject loop
    for (s in 1:N) {
      
      // define needed variables
      vector[2] ev; // expected value for both options
      real PE;      // prediction error
      real absPE;   // absolute prediction error
      real k;       // learning rate per trial
      int rep;      // correct choice in last trial (1=yes, -1=no)
    
      // condition loop
      for (v in 1:C) {
        
        // set initial values
        ev = initV;
        absPE = initabsPE;
        k = A[s,v];
        rep = initrep;
      
        // trial loop
        for (t in 1:Tsubj[s,v]) {
        
          // how does choice relate to inverse temperature and action value
          choice[s,t,v] ~ bernoulli_logit(tau[s,v] * (ev[2]-ev[1]) + tau[s,v] * pi_normal[s,v] * rep); // inverse temp * Q
          
          //EXAMPLES 
    				// P(choice(t) = 0), given ev(choice(t) = 1)>ev(choice(t) = 0) and rep = 1)
    				// chosing incorrect choice when ev of correct choice is higher and correct choice was chosen in last trial
    				// --> (ev[2] - ev[1]) is positive AND tau * Pi * rep is positive --> overall high probability
    				// --> bernoulli logit: 1 - p --> overall low probability
    				
    				// P(choice(t) = 0), given ev(choice(t) = 1)>ev(choice(t) = 0) and rep = -1)
    				// chosing incorrect choice when ev of correct choice is higher but incorrect choice was chosen in last trial
    				// --> (ev[2] - ev[1]) is positive BUT tau * Pi * rep is negative --> overall less high probability
    				// --> bernoulli logit: 1 - p --> overall less low probability
    				
    				// P(choice(t) = 1), given ev(choice(t) = 1)>ev(choice(t) = 0) and rep = 1)
    				// chosing correct choice when ev of correct choice is higher and correct choice was chosen in last trial
    				// --> (ev[2] - ev[1]) is positive AND tau * Pi * rep is positive --> overall high probability
    				// --> bernoulli logit: p --> overall high probability
    				
    				// P(choice(t) = 1), given ev(choice(t) = 1)>ev(choice(t) = 0) and rep = -1)
    				// chosing correct choice when ev of correct choice is higher but incorrect choice was chosen in last trial
    				// --> (ev[2] - ev[1]) is positive BUT tau * Pi * rep is negative --> overall less high probability
    				// --> bernoulli logit: 1 - p --> overall less high probability
          
          // Pearce Hall learning rate
          k = gamma[s,v]*absPE + (1-gamma[s,v])*k; // decay constant * arbitrary constant * absolute PE from last trial + (1-decay constant) * learning rate from last trial
                                                    // if decay constant close to 1: dynamic learning rate will be strongly affected by PEs from last trial and only weakly affected by learning rate from previous trial (high fluctuation)
                                                    // if decay constant close to 0: dynamic learning rate will be weakly affected by PEs from last trial and strongly affected by learning rate from previous trial (low fluctuation)

        
          // prediction error
          PE = outcome[s,t,v] - ev[choice[s,t,v]+1]; // outcome - Q of choice taken
          absPE = abs(PE);
          
          // value updating (learning)
          ev[choice[s,t,v]+1] += k * PE; // Q + dynamic alpha * PE    
          
          // update rep for use in next trial
          rep = 2 * choice[s,t,v] - 1;
      
        }
        
      }
    
    }
  
  }
  
}

generated quantities {
  
  // Define mean group-level parameter values
  real<lower=0, upper=1> mu_A; // initialize mean of posterior
  real<lower=0, upper=100> mu_tau;
  real<lower=0, upper=1> mu_gamma;
  real mu_pi; 

  // For log likelihood calculation
  real log_lik[N,MT,C];
  
  // for choice propability calculation (of chosen option)
  real softmax_ev_chosen[N,MT,C];

  // For posterior predictive check
  int y_pred[N,MT,C];
  
  // extracting PEs per subject and trial
  real PE_pred[N,MT,C];
  
  // extracting q values per subject and trial
  real ev_pred[N,MT,C,2];
  real ev_chosen_pred[N,MT,C];
  
  // extracting dynamic learning rate per subject and trial
  real k_pred[N,MT,C];
  
  // correlation matrix
  corr_matrix[kV+1] A_cor = multiply_lower_tri_self_transpose(A_subj_L);
  corr_matrix[kV+1] tau_cor = multiply_lower_tri_self_transpose(tau_subj_L);
  corr_matrix[kV+1] gamma_cor = multiply_lower_tri_self_transpose(gamma_subj_L);
  corr_matrix[kV+1] pi_cor = multiply_lower_tri_self_transpose(pi_subj_L);

  // Set all PE and ev predictions to -999 (avoids NULL values)
  for (s in 1:N) {
    for (v in 1:C) {
      for (t in 1:MT) {
        y_pred[s,t,v] = -999;
        PE_pred[s,t,v] = -999;
        ev_chosen_pred[s,t,v] = -999;
        k_pred[s,t,v] = -999;
        softmax_ev_chosen[s,t,v] = -999;
        log_lik[s,t,v] = -999;
        for (c in 1:2) {
          ev_pred[s,t,v,c] = -999;
        }
      }
    }
  }
  
  
  // calculate overall means of parameters
  mu_A   = Phi_approx(mu[1]); 
  mu_tau = Phi_approx(mu[2]) * 100;
  mu_gamma   = Phi_approx(mu[3]);
  mu_pi = mu[4];
  

  { // local section, this saves time and space
    for (s in 1:N) {
      
      vector[2] ev; // expected value
      real PE;      // prediction error
      real absPE; // absolute prediction error
      real k; // learning rate
      vector[2] softmax_ev; // softmax per ev
      int rep;       // correct choice in last trial (1=yes, -1=no)

      for (v in 1:C) {
        
        // initialize values
        ev = initV;
        absPE = initabsPE;
        k = A[s,v];
        rep = initrep;
      
        // quantities of interest
        for (t in 1:Tsubj[s,v]) {
          
          // generate prediction for current trial
          // if estimation = 1, we draw from the posterior
          // if estimation = 0, we equally draw from the posterior, but the posterior is equal to the prior as likelihood is not evaluated
          y_pred[s,t,v] = bernoulli_logit_rng(tau[s,v] * (ev[2]-ev[1]) + tau[s,v] * pi_normal[s,v] * rep); // following the recommendation to use the same function as in model block but with rng ending
          
          // if estimation = 1, compute quantities of interest based on actual choices
          if (run_estimation==1){
            
            // compute log likelihood of current trial
            log_lik[s,t,v] = bernoulli_logit_lpmf(choice[s,t,v] | tau[s,v] * (ev[2]-ev[1]) + tau[s,v] * pi_normal[s,v] * rep);
            
            // compute choice probability
            softmax_ev = softmax(tau[s,v] * ev + tau[s,v] * pi_normal[s,v] * rep);
            
            softmax_ev_chosen[s,t,v] = softmax_ev[choice[s,t,v]+1];
            
            // Pearce Hall learning rate
            k = gamma[s,v]*absPE + (1-gamma[s,v])*k;
            k_pred[s,t,v] = k;
            
            // prediction error
            PE = outcome[s,t,v] - ev[choice[s,t,v]+1];
            PE_pred[s,t,v] = PE;
          
            // value updating (learning)
            ev[choice[s,t,v]+1] += k * PE;
            
            ev_pred[s,t,v,1] = ev[1]; // copy both evs into pred
            ev_pred[s,t,v,2] = ev[2]; // copy both evs into pred
            
            ev_chosen_pred[s,t,v] = ev[choice[s,t,v]+1];
          
          }
        
          // if estimation = 0, compute quantities of interest based on simulated choices
          if (run_estimation==0){
          
            // compute log likelihood of current trial
            log_lik[s,t,v] = bernoulli_logit_lpmf(y_pred[s,t,v] | tau[s,v] * (ev[2]-ev[1]) + tau[s,v] * pi_normal[s,v] * rep);
          
            // Pearce Hall learning rate
            k = gamma[s,v]*absPE + (1-gamma[s,v])*k;
            k_pred[s,t,v] = k;
            
            // prediction error
            PE = outcome[s,t,v] - ev[y_pred[s,t,v]+1];
            PE_pred[s,t,v] = PE;
            
            // value updating (learning)
            ev[y_pred[s,t,v]+1] += k * PE;
          
            ev_pred[s,t,v,1] = ev[1]; // copy both evs into pred
            ev_pred[s,t,v,2] = ev[2]; // copy both evs into pred
            
            ev_chosen_pred[s,t,v] = ev[y_pred[s,t,v]+1];
          
          }
        
        } // trial loop
    
      } // condition loop
  
    } // subject loop

  } // local section
  
} // generated quiantities

