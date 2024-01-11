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
}

// output - posterior distribution should be sought
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  vector[2] mu_pr; // group level means for the 2 parameters
  vector<lower=0>[2] sigma; // group level variance for the two parameters

  // Subject-level raw parameters (for Matt trick - efficient way of sampling)
  vector[N] A_pr;    // learning rate
  vector[N] tau_pr;  // inverse temperature
}

transformed parameters {
  // initialize subject-level parameters
  vector<lower=0, upper=1>[N] A; // bring alpha to range between 0 and 1
  vector<lower=0, upper=20>[N] tau; // bring tau to range between 0 and 20
  
  // compute subject-level parameters: group mean per parameter + individual subject parameter * group level variance, then take AOC
  for (i in 1:N) {
    A[i]   = Phi_approx(mu_pr[1]  + sigma[1]  * A_pr[i]); 
    tau[i] = Phi_approx(mu_pr[2] + sigma[2] * tau_pr[i]) * 20;
  }
  
}

model {
  // Hyperparameters - define prior distributions
  mu_pr ~ normal(0, 1);
  sigma ~ normal(0, 0.2); // changed from (0, 1) in main fitting result, (0, 0.2) is according to hBayesdm
  // individual parameters - define prior distributions
  A_pr ~ normal(0, 1);
  tau_pr ~ normal(0, 1);
  
  // only execute this part if we want to evaluate likelihood (fit real data)
  if (run_estimation==1){

    // subject loop
    for (i in 1:N) {
      vector[2] ev; // expected value for both choices
      real ev_chosen; // expected value of chosen choice
      vector[2] PE_fict; // prediction error for reward fictive updating (for unchosen options)
      real PE_chosen;      // prediction error
  
      ev = initV;
      ev_chosen = initV[1];
      
      // trial loop
      for (t in 1:Tsubj[i]) {
        
        // how does choice relate to inverse temperature and action value
        choice[i, t] ~ bernoulli_logit(tau[i] * (ev[2]-ev[1])); // inverse temp * Q
        
        // prediction error
        PE_chosen = outcome[i, t] - ev[choice[i, t]+1]; // outcome - Q of chosen card
        PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
        
        // store chosen Q value
        ev_chosen = ev[choice[i, t]+1];
        
        // First, update Q for all cards w/ fictive updating
        ev += A[i] * PE_fict;
        
        // Replace Q values of chosen deck with correct values using stored values
        ev[choice[i, t]+1] = ev_chosen + A[i] * PE_chosen;                                                                                     //??? do we need to update Q-values separately per choice?
      
      }
    
    }
  
  }
  
}

generated quantities {
  // For group level parameters
  real<lower=0, upper=1> mu_A; // initialize mean of posterior
  real<lower=0, upper=20> mu_tau;
  
  // calculate mean of posterior
  mu_A   = Phi_approx(mu_pr[1]); 
  mu_tau = Phi_approx(mu_pr[2]) * 20;

  // For log likelihood calculation
  real log_lik[N];

  // For posterior predictive check
  int y_pred_cat_log_rng[N, MT];
  
  // extracting PEs per subject and trial
  real PE_pred[N,MT,2];
  real PE_chosen_pred[N,MT];
  
  // extracting q values per subject and trial (chosen option only)
  real ev_pred[N,MT,2];
  real ev_chosen_pred[N,MT];
  
    // Set all PE and ev predictions to -999 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:MT) {
      y_pred_cat_log_rng[i, t] = -999;
      PE_chosen_pred[i, t] = -999;
      ev_chosen_pred[i, t] = -999;
      for (c in 1:2) {
        PE_pred[i, t, c] = -999;
        ev_pred[i, t, c] = -999;
      }
    }
  }

  { // local section, this saves time and space
    for (i in 1:N) {
      vector[2] ev; // expected value
      real ev_chosen;
      vector[2] PE_fict; // prediction error
      real PE_chosen;

      // initialize values
      ev = initV;
      log_lik[i] = 0;
      
      // quantities of interest
      
      for (t in 1:Tsubj[i]) {
        
        // generate prediction for current trial
        // if estimation = 1, we draw from the posterior
        // if estimation = 0, we equally draw from the posterior, but the posterior is equal to the prior as likelihood is not evaluated
        y_pred_cat_log_rng[i, t] = bernoulli_logit_rng(tau[i] * (ev[2]-ev[1])); // following the recommendation to use the same function as in model block but with rng ending
        
        // if estimation = 1, compute quantities of interest based on actual choices
        if (run_estimation==1) {
          
          // compute log likelihood of current trial
          log_lik[i] += bernoulli_logit_lpmf(choice[i, t] | tau[i] * (ev[2]-ev[1]));
          
          // prediction error
          PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
          PE_chosen = outcome[i, t] - ev[choice[i, t]+1];
          
          PE_pred[i, t, 1] = PE_fict[1]; // copy both RPEs into pred
          PE_pred[i, t, 2] = PE_fict[2]; // copy both RPEs into pred
          PE_pred[i, t, choice[i, t]+1] = PE_chosen; // replace RPE for chosen option
          
          PE_chosen_pred[i, t] = PE_chosen; // save PEs of chosen option only
          
          // store chosen Q value
          ev_chosen = ev[choice[i, t]+1];
        
          // First, update Q for all cards w/ fictive updating
          ev += A[i] * PE_fict;
        
          // Replace Q values of chosen deck with correct values using stored values
          ev[choice[i, t]+1] = ev_chosen + A[i] * PE_chosen;                                                                                   //??? do we need to update Q-values separately per choice?
          
          ev_pred[i, t, 1] = ev[1]; // copy both evs into pred
          ev_pred[i, t, 2] = ev[2]; // copy both evs into pred
          
          ev_chosen_pred[i, t] = ev[choice[i, t]+1]; // save ev of chosen option only
          
        }
        
        // if estimation = 0, compute quantities of interest based on simulated choices
        if (run_estimation==0){
          
          // compute log likelihood of current trial
          log_lik[i] += bernoulli_logit_lpmf(y_pred_cat_log_rng[i, t] | tau[i] * (ev[2]-ev[1]));
          
          // prediction error
          PE_fict = abs(outcome[i, t]-1) - ev; // opposite of outcome (0 if real outcome 1, 1 if real outcme 0) - Q of chosen and unchosen cards
          PE_chosen = outcome[i, t] - ev[y_pred_cat_log_rng[i, t]+1];
          
          PE_pred[i, t, 1] = PE_fict[1]; // copy both RPEs into pred
          PE_pred[i, t, 2] = PE_fict[2]; // copy both RPEs into pred
          PE_pred[i, t, y_pred_cat_log_rng[i, t]+1] = PE_chosen; // replace RPE for chosen option
          
          PE_chosen_pred[i, t] = PE_chosen; // save PEs of chosen option only
          
          // store chosen Q value
          ev_chosen = ev[y_pred_cat_log_rng[i, t]+1];
        
          // First, update Q for all cards w/ fictive updating
          ev += A[i] * PE_fict;
        
          // Replace Q values of chosen deck with correct values using stored values
          ev[y_pred_cat_log_rng[i, t]+1] = ev_chosen + A[i] * PE_chosen;                                                                                   //??? do we need to update Q-values separately per choice?
          ev_pred[i, t, 1] = ev[1];
          ev_pred[i, t, 2] = ev[2];
          
        }
        
      }
    
  }
  
}

}

