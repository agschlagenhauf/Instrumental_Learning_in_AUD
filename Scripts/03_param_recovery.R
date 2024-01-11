##### Preparation #####

# import packages
rm(list=ls())
libs<-c("rstan", "stringr")
sapply(libs, require, character.only=TRUE)

# activate these stan options -----------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# only on windows:
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

datapath<-"/fast/work/groups/ag_schlagenhauf/B01_FP1_WP2/ILT_DATA"
out_path <- '/fast/work/groups/ag_schlagenhauf/B01_FP1_WP2/ILT_Stan_Modeling'

# set estimation for stan data 
estimation <- 1

# get model name
args <- commandArgs(trailingOnly = TRUE)
model_name <- args[1] # which model should be fitted to data?
model_name
input_fit_rds <- args[2] # which fit object should be used as input?
input_fit_rds

# take 100 random draws from total 36000 draws
draws <- sample.int(36000, 100)
write.table(draws, file=file.path(out_path, paste('Output/Parameter_Recovery/draws_', model_name, '.txt', sep = "")), append = FALSE, sep = ";", dec = ".", row.names = F, col.names = F)
 
##### Read input #####

# load fit object
fit <- readRDS(file.path(out_path, 'Output', input_fit_rds))

# load real input
if (model_name == 'bandit2arm_delta_main' | model_name == 'bandit2arm_delta_PH_withC') {
  load(file.path(datapath,"Input/stan_data_bandit2arm_delta_main.RData")) # Behav and redcap data
} else if (model_name == 'bandit2arm_delta_main_hierarchical' | model_name == 'bandit2arm_delta_main_rep_hierarchical' | model_name == 'bandit2arm_delta_PH_hierarchical' 
           | model_name == 'bandit2arm_delta_PH_withC_hierarchical' | model_name == 'bandit2arm_delta_PH_rep_hierarchical' | model_name == 'bandit2arm_delta_PH_withC_rep_hierarchical' ) {
  load(file.path(datapath,"Input/stan_data_bandit2arm_delta_main_hierarchical.RData")) # Behav and redcap data
}

# extract posterior draws for all parameters
posterior <- extract(fit)


##### Load Stan Model #####

model_filename <- paste(model_name, ".stan", sep = "")
stan_model<- file.path(out_path, "Models", model_filename)
stanc(stan_model)


##### Fitting loop for 100 draws #####

for (draw in draws) {
  
  print(paste("current draw =", draw))
  
  # stan input as named list
  if (model_name == 'bandit2arm_delta_main' | model_name == 'bandit2arm_delta_PH_withC') {
    sim_data <- list(N = stan_data$N, 
                    T = stan_data$T,
                    MT = stan_data$MT,
                    Tsubj = stan_data$Tsubj,
                    choice = posterior$y_pred[draw,,],
                    outcome = stan_data$outcome,
                    run_estimation = estimation)
  } else if (model_name == 'bandit2arm_delta_main_hierarchical' | model_name == 'bandit2arm_delta_PH_withC_hierarchical'){
    sim_data <- list(N = stan_data$N,
                     C = stan_data$C,
                     T = stan_data$T,
                     MT = stan_data$MT,
                     Tsubj = stan_data$Tsubj,
                     choice = posterior$y_pred[draw,,,],
                     outcome = stan_data$outcome,
                     kS = stan_data$kS, # number of subj-level variables (aud_group)
                     subj_vars = stan_data$subj_vars,
                     kV = stan_data$kV, # number of visit-level variables (reinforcer_type)
                     visit_vars = stan_data$visit_vars,
                     run_estimation = estimation)
  }
  
  # Options
  s <- list(adapt_delta=0.90, stepsize=0.5)
  
  # Fit
  fit <- stan(file = stan_model, data = stan_data, warmup =1000, iter = 5000, chains = 4, verbose=TRUE, control=s)
  
  # Save fitted object as RDS
  
  output_filename <- paste("sim_fit_", Sys.Date(), '_', model_name, '_delta', s$adapt_delta, '_stepsize', s$stepsize, "_", input_fit_rds, "_draw", draw, ".rds", sep="")
  saveRDS(fit, file=file.path(out_path, "Output/Parameter_Recovery", output_filename))
  
}