##### Preparation #####

# import packages
rm(list=ls())
libs<-c("rstan", "stringr", "dplyr")
sapply(libs, require, character.only=TRUE)

# activate these stan options -----------------------------------
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE) 

# only on windows:
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

#datapath<-"C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling"
datapath<-"/fast/work/groups/ag_schlagenhauf/B01_FP1_WP2/ILT_DATA"
filepath<-"/fast/work/groups/ag_schlagenhauf/B01_FP1_WP2/ILT_Stan_Modeling"
#out_path<-'S:/AG/AG-Schlagenhauf_TRR265/Daten/B01/Analysen/WP2_ILT/Stan Output'

# get model name
args <- commandArgs(trailingOnly = TRUE)
model_name <- args[1]
#model_name <- 'bandit2arm_delta_main_hierarchical'

## Prior predictive checks or fitting?
estimation = 0 # 0 = prior predictive check because likelihood is not evaluated; 1 = model fitting to real data

##### Read input & prepare #####

input<-read.table(file.path(datapath, 'Input/Stan_input_hierarchical.txt'), header = T)

nsub <- length(unique(input$subjID))
nc <- length(unique(input$condition))
nt <- length(input$trial)
ntsub <- unlist(table(input$subjID,input$condition), use.names = F)

ncond <- length(unique(input$condition))

ntsub_non_na_jui <- c()
ntsub_non_na_alc <- c()

## restructure choice and outcome data so that NAs are displayed at column end
input_jui <- input[input$condition==0,]
input_alc <- input[input$condition==1,]

# choice juice
choice_ntsub_jui <- as.data.frame(matrix(input_jui$choice,nrow=ntsub,byrow = F))

for (i in 1:nsub) {
  choice_ntsub_non_na_jui <- na.omit(choice_ntsub_jui[,i]) # omit NAs
  ntsub_non_na_jui <- c(ntsub_non_na_jui,length(choice_ntsub_non_na_jui)) # count non-NA trials per subject
  choice_ntsub_non_na_as_list_jui <- as.list(choice_ntsub_non_na_jui) # transform choices without NAs to list
  pad_choice_jui <- ntsub[i,1]-length(choice_ntsub_non_na_as_list_jui) # number of missing trials per subject
  vec_pad_choice_jui <- rep(NA, pad_choice_jui) # create NA vector of length of missings
  padded_list_choice_jui <- append(choice_ntsub_non_na_as_list_jui, vec_pad_choice_jui) # pad non-NA trials and NA vector
  #print(choice_ntsub[,i])
  choice_ntsub_jui[,i] <- unlist(padded_list_choice_jui)
}
choice_ntsub_jui <- t(data.matrix(choice_ntsub_jui))
choice_ntsub_jui[is.na(choice_ntsub_jui)] <- -999

# choice alcohol
choice_ntsub_alc <- as.data.frame(matrix(input_alc$choice,nrow=ntsub,byrow = F))

for (i in 1:nsub) {
  choice_ntsub_non_na_alc <- na.omit(choice_ntsub_alc[,i]) # omit NAs
  ntsub_non_na_alc <- c(ntsub_non_na_alc,length(choice_ntsub_non_na_alc)) # count non-NA trials per subject
  choice_ntsub_non_na_as_list_alc <- as.list(choice_ntsub_non_na_alc) # transform choices without NAs to list
  pad_choice_alc <- ntsub[i,1]-length(choice_ntsub_non_na_as_list_alc) # number of missing trials per subject
  vec_pad_choice_alc <- rep(NA, pad_choice_alc) # create NA vector of length of missings
  padded_list_choice_alc <- append(choice_ntsub_non_na_as_list_alc, vec_pad_choice_alc) # pad non-NA trials and NA vector
  #print(choice_ntsub[,i])
  choice_ntsub_alc[,i] <- unlist(padded_list_choice_alc)
}
choice_ntsub_alc <- t(data.matrix(choice_ntsub_alc))
choice_ntsub_alc[is.na(choice_ntsub_alc)] <- -999

# combine
choice_ntsub <- array(c(choice_ntsub_jui,choice_ntsub_alc), dim = c(71,50,2)) # choice
ntsub_non_na <- cbind(matrix(t(ntsub_non_na_jui)),matrix(t(ntsub_non_na_alc)))# number of trials per subject

# outcome juice
outcome_ntsub_jui <- as.data.frame(matrix(input_jui$outcome,nrow=ntsub,byrow = F))

for (i in 1:nsub) {
  outcome_ntsub_non_na_jui <- na.omit(outcome_ntsub_jui[,i]) # omit NAs
  outcome_ntsub_non_na_as_list_jui <- as.list(outcome_ntsub_non_na_jui) # transform choices without NAs to list
  pad_outcome_jui <- ntsub[i,2]-length(outcome_ntsub_non_na_as_list_jui) # number of missing trials per subject
  vec_pad_outcome_jui <- rep(NA, pad_outcome_jui) # create NA vector of length of missings
  padded_list_outcome_jui <- append(outcome_ntsub_non_na_as_list_jui, vec_pad_outcome_jui) # pad non-NA trials and NA vector
  #print(outcome_ntsub[,i])
  outcome_ntsub_jui[,i] <- unlist(padded_list_outcome_jui)
}
outcome_ntsub_jui <- t(data.matrix(outcome_ntsub_jui))
outcome_ntsub_jui[is.na(outcome_ntsub_jui)] <- -999

# outcome alcohol
outcome_ntsub_alc <- as.data.frame(matrix(input_alc$outcome,nrow=ntsub,byrow = F))

for (i in 1:nsub) {
  outcome_ntsub_non_na_alc <- na.omit(outcome_ntsub_alc[,i]) # omit NAs
  outcome_ntsub_non_na_as_list_alc <- as.list(outcome_ntsub_non_na_alc) # transform choices without NAs to list
  pad_outcome_alc <- ntsub[i,2]-length(outcome_ntsub_non_na_as_list_alc) # number of missing trials per subject
  vec_pad_outcome_alc <- rep(NA, pad_outcome_alc) # create NA vector of length of missings
  padded_list_outcome_alc <- append(outcome_ntsub_non_na_as_list_alc, vec_pad_outcome_alc) # pad non-NA trials and NA vector
  #print(outcome_ntsub[,i])
  outcome_ntsub_alc[,i] <- unlist(padded_list_outcome_alc)
}
outcome_ntsub_alc <- t(data.matrix(outcome_ntsub_alc))
outcome_ntsub_alc[is.na(outcome_ntsub_alc)] <- -999

# combine
outcome_ntsub <- array(c(outcome_ntsub_jui,outcome_ntsub_alc), dim = c(71,50,2))

ntmax <- ntsub[1,1]
#ntsub <- as.numeric(ntsub)

# subject- and visit-level variables
num_subj_vars <- 1
num_visit_vars <- 1

df_aud_group <- input %>%
  group_by(subjID) %>%
  summarise_at(vars(group), list(group = mean), na.rm=F)
aud_group <- array(data = c(unlist(df_aud_group$group)),
                   dim = c(nsub, num_subj_vars))

df_reinforcer_type_jui <- input_jui %>%
  group_by(subjID) %>%
  summarise_at(vars(condition), list(condition = mean), na.rm=F)
df_reinforcer_type_alc <- input_alc %>%
  group_by(subjID) %>%
  summarise_at(vars(condition), list(condition = mean), na.rm=F)
reinforcer_type <- array(data = c(unlist(df_reinforcer_type_jui$condition), unlist(df_reinforcer_type_alc$condition)),
                         dim = c(nsub, ncond, num_visit_vars))

##### stan input as named list ####

stan_data <- list(N = nsub,
                  C = ncond,
                  T = nt,
                  MT = ntmax,
                  Tsubj = ntsub_non_na,
                  choice = choice_ntsub,
                  outcome = outcome_ntsub,
                  kS = num_subj_vars, # number of subj-level variables (aud_group)
                  subj_vars = aud_group,
                  kV = num_visit_vars, # number of visit-level variables (reinforcer_type)
                  visit_vars = reinforcer_type,
                  run_estimation = estimation) # 0 = prior predictive check because likelihood is not evaluated; 1 = model fitting to real data

input_filename <- paste("stan_data_", model_name, ".RData", sep="")
save(file=file.path(datapath, "Input", input_filename), stan_data)

##### Load Stan Model #####

model_filename <- paste(model_name, ".stan", sep = "")
stan_model<- file.path(filepath, "Models", model_filename)
stanc(stan_model)

##### Fit Model #####

# Options
s <- list(adapt_delta=0.90, stepsize=0.5)

# Fit
fit <- stan(file = stan_model, data = stan_data, warmup =1000, iter = 10000, chains = 4, verbose=TRUE, control=s)

##### Save fitted object as RDS #####
#output_filename <- paste("fit_n_", nsub ,'_', Sys.Date(), '_', model_name, ".rds", sep="")
output_filename <- paste("fit_n_", nsub ,'_', Sys.Date(), '_', model_name, '_estimation', estimation, '_delta', s$adapt_delta, '_stepsize', s$stepsize, ".rds", sep="")
saveRDS(fit, file=file.path(filepath, "Output", output_filename))
