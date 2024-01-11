// input
data {
  int<lower=1> N; // subjects
  int<lower=1> T; // total number of trials across subjects
  int<lower=1> MT; // max number of trials / subject
  int<lower=1, upper=MT> Tsubj[N]; // actual number of trials / subject
  int<lower=-999, upper=2> choice[N, MT]; // choice of correct (1) or incorrect (0) card
  int<lower=-999, upper=1> outcome[N, MT];  // outcome 
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
}

// transformed input 
transformed data {
  vector[2] initV;  // initial values for EV, both choices have a value of 0.5
  initV = rep_vector(0.5, 2); // set initial Q value to 0.5 (middle btw. 0 and 1)
  real initabsPE_chosen;
  initabsPE_chosen = 0.5; /// 
}

// output - posterior distribution should be sought
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[4] mu_pr; // group level means for the 4 parameters
  vector<lower=0>[4] sigma; // group level variance for the 4 parameters

  // Subject-level raw parameters (for Matt trick - efficient way of sampling)
  vector[N] A_pr;    // initial learning rate
  vector[N] tau_pr;  // inverse temperature
  vector[N] gamma_pr; // decay constant
  vector[N] C_pr; // arbitrary constant
}

transformed parameters {
  // initialize subject-level parameters
  vector<lower=0, upper=1>[N] A; // bring alpha to range between 0 and 1
  vector<lower=0, upper=20>[N] tau; // bring tau to range between 0 and 20
  vector<lower=0, upper=1>[N] gamma; // bring gamma to range between...
                                      // 0 and 1 (as in Theresas code)
  vector<lower=0, upper=1>[N] C; // arbitrary constant                                    
  
  // compute subject-level parameters: group mean per parameter + individual subject parameter * group level variance, then take AOC
  for (i in 1:N) { // subject loop
    A[i]   = Phi_approx(mu_pr[1]  + sigma[1]  * A_pr[i]);
    tau[i] = Phi_approx(mu_pr[2] + sigma[2] * tau_pr[i]) * 20;
    gamma[i] = Phi_approx(mu_pr[3] + sigma[3] * gamma_pr[i]);
    C[i] = Phi_approx(mu_pr[4]  + sigma[4]  * C_pr[4]); // times 10 as same done for tau?
    
  }
  
}

model {
  // Hyperparameters - define prior distributions
  mu_pr ~ normal(0, 1);
  sigma ~ normal(0, 0.2); // changed from (0, 1) in main fitting result, (0, 0.2) is according to hBayesdm
 
  // individual parameters - define prior distributions
  A_pr ~ normal(0, 1);
  tau_pr ~ normal(0, 1);
  gamma_pr ~ normal(0, 1);
  C_pr ~ normal(0, 1);

  // only execute this part if we want to evaluate likelihood (fit real data)
  if (run_estimation==1){
  
    // subject loop
    for (i in 1:N) {
      vector[2] ev; // expected value
      real ev_chosen; // expected value of chosen option
      vector[2] PE_fict; // prediction error for reward fictive updating (for unchosen options)
      real PE_chosen;      // prediction error
      real absPE_chosen; // absolute prediction error
      real k; // learning rate per trial
  
      // initializations
      ev = initV;
      ev_chosen = initV[1];
      absPE_chosen = initabsPE_chosen;
      k = A[i];
      
      // trial loop
      for (t in 1:Tsubj[i]) {
        
        // how does choice relate to inverse temperature and action value
        choice[i, t] ~ bernoulli_logit(tau[i] * (ev[2]-ev[1])); // inverse temp * Q from last trial (as not updated yet)
        
        // Pearce Hall learning rate
        k = gamma[i]*C[i]*absPE_chosen + (1-gamma[i])*k; // decay constant * arbitrary constant * absolute PE from last trial + (1-decay constant) * learning rate from last trial
                                                  // if decay constant close to 1: dynamic learning rate will be strongly affected by PEs from last trial and only weakly affected by learning rate from previous trial (high fluctuation)
                                                  // if decay constant close to 0: dynamic learning rate will be weakly affected by PEs from last trial and strongly affected by learning rate from previous trial (low fluctuation)
        
        // prediction error
        PE_chosen = outcome[i, t] - ev[choice[i, t]+1]; // outcome - Q of choice taken
        absPE_chosen = abs(PE_chosen);
        PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
        
        // store chosen Q value
        ev_chosen = ev[choice[i, t]+1];
        
        // First, update Q for all cards w/ fictive updating
        ev += k * PE_fict; // Q + alpha * PE
        
        // Replace Q values of chosen deck with correct values using stored values
        ev[choice[i, t]+1] = ev_chosen + k * PE_chosen;
      
      }
    
    }
    
  }
  
}

generated quantities {
  // For group level parameters
  real<lower=0, upper=1> mu_A; // initialize mean of posterior
  real<lower=0, upper=20> mu_tau;
  real<lower=0, upper=1> mu_gamma;
  real<lower=0, upper=1> mu_C;
  
  // calculate mean of posterior
  mu_A = Phi_approx(mu_pr[1]);
  mu_tau = Phi_approx(mu_pr[2]) * 20; 
  mu_gamma = Phi_approx(mu_pr[3]); 
  mu_C = Phi_approx(mu_pr[4]);// times 10 as same done for tau?

  // For log likelihood calculation
  real log_lik[N];

  // For posterior predictive check
  int y_pred_cat_log_rng[N, MT];
  
  // extracting PEs per subject and trial
  real PE_pred[N,MT,2];
  real PE_chosen_pred[N,MT];
  real absPE_chosen_pred[N,MT];
  
  // extracting q values per subject and trial (chosen option)
  real ev_pred[N,MT,2];
  real ev_chosen_pred[N,MT];
  
  // extracting dynamic learning rate per subject and trial
  real k_pred[N,MT];
  
  // Set all PE, ev, A and choice predictions to -999 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:MT) {
      y_pred_cat_log_rng[i, t] = -999;
      PE_chosen_pred[i, t] =-999;
      absPE_chosen_pred[i, t] = -999;
      ev_chosen_pred[i, t] = -999;
      k_pred[i, t] = -999;
      for (c in 1:2) {
        PE_pred[i, t, c] = -999;
        ev_pred[i, t, c] = -999;
      }
    }
  }

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[2] ev; // expected value
      real ev_chosen; // expected value of chosen option
      vector[2] PE_fict; // prediction error for reward fictive updating (for unchosen options)
      real PE_chosen;      // prediction error
      real absPE_chosen; // absolute prediction error
      real k; // learning rate per trial

      // initialize values
      ev = initV;
      ev_chosen = initV[1];
      absPE_chosen = initabsPE_chosen;
      k = A[i];
      log_lik[i] = 0;
      
      // quantities of interest
      for (t in 1:Tsubj[i]) {
        
        // generate posterior prediction for current trial
        y_pred_cat_log_rng[i, t] = bernoulli_logit_rng(tau[i] * (ev[2]-ev[1])); // following the recommendation to use the same function as in model block but with rng ending
        
        // if estimation = 1, compute quantities of interest based on actual choices
        if (run_estimation==1){
        
          // Pearce Hall learning rate
          k = gamma[i]*C[i]*absPE_chosen + (1-gamma[i])*k;
          
          k_pred[i,t] = k;
          
          // compute log likelihood of current trial
          log_lik[i] += bernoulli_logit_lpmf(choice[i, t] | tau[i] * (ev[2]-ev[1]));
          
          // prediction error
          PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
          PE_chosen = outcome[i, t] - ev[choice[i, t]+1];
          absPE_chosen = abs(PE_chosen);
          
          PE_pred[i, t, 1] = PE_fict[1]; // copy both RPEs into pred
          PE_pred[i, t, 2] = PE_fict[2]; // copy both RPEs into pred
          PE_pred[i, t, choice[i, t]+1] = PE_chosen; // replace RPE for chosen option
          
          PE_chosen_pred[i, t] = PE_chosen; // save PEs of chosen option only
          absPE_chosen_pred[i, t] = absPE_chosen; // save abs PEs of chosen option only
          
          // store chosen Q value
          ev_chosen = ev[choice[i, t]+1];
        
          // First, update Q for all cards w/ fictive updating
          ev += k * PE_fict; // Q + alpha * PE
          
          // Replace Q values of chosen deck with correct values using stored values
          ev[choice[i, t]+1] = ev_chosen + k * PE_chosen;
          
          ev_pred[i, t, 1] = ev[1]; // copy both evs into pred
          ev_pred[i, t, 2] = ev[2]; // copy both evs into pred
          
          ev_chosen_pred[i, t] = ev[choice[i, t]+1]; // save ev of chosen option only
          
        }
        
        // if estimation = 0, compute quantities of interest based on simulated choices
        if (run_estimation==0){

          // Pearce Hall learning rate
          k = gamma[i]*C[i]*absPE_chosen + (1-gamma[i])*k;
          
          k_pred[i,t] = k;
          
          // compute log likelihood of current trial
          log_lik[i] += bernoulli_logit_lpmf(y_pred_cat_log_rng[i, t] | tau[i] * (ev[2]-ev[1]));
          
          // prediction error
          PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
          PE_chosen = outcome[i, t] - ev[y_pred_cat_log_rng[i, t]+1];
          absPE_chosen = abs(PE_chosen);
          
          PE_pred[i, t, 1] = PE_fict[1]; // copy both RPEs into pred
          PE_pred[i, t, 2] = PE_fict[2]; // copy both RPEs into pred
          PE_pred[i, t, y_pred_cat_log_rng[i, t]+1] = PE_chosen; // replace RPE for chosen option
          
          PE_chosen_pred[i, t] = PE_chosen; // save PEs of chosen option only
          absPE_chosen_pred[i, t] = absPE_chosen; // save abs PEs of chosen option only
          
          // store chosen Q value
          ev_chosen = ev[y_pred_cat_log_rng[i, t]+1];
        
          // First, update Q for all cards w/ fictive updating
          ev += k * PE_fict; // Q + alpha * PE
          
          // Replace Q values of chosen deck with correct values using stored values
          ev[y_pred_cat_log_rng[i, t]+1] = ev_chosen + k * PE_chosen;
          
          ev_pred[i, t, 1] = ev[1]; // copy both evs into pred
          ev_pred[i, t, 2] = ev[2]; // copy both evs into pred
          
          ev_chosen_pred[i, t] = ev[y_pred_cat_log_rng[i, t]+1]; // save ev of chosen option only
          
        }
         
     }
    
  }
  
}

}

