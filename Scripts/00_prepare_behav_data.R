### READ IN RAW BEHAVIORAL DATA AND CREATE ONE SINGLE DF ###
### MILENA MUSIAL 05/2023 ##################################

###### Preps ######

# clear
rm(list=ls())

# install.packages('tidyverse', 'stringr')
libs<-c("tidyverse", "R.matlab", "stringr", "plyr", "dplyr")
sapply(libs, require, character.only=TRUE)

# define paths
redcap_path <- 'C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_DATA/RedCap/'
data_path<-'C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_DATA/Behav/raw/FilesReport_ILTdata_2023-05-24_1718/documents/'

# load redcap file to get final IDs
load(file.path(redcap_path,"redcap_n71.RData"))

###### define files #####

files<-list.files(data_path)

behav <- data.frame()


###### behav_final: combine all relevant vars of all subjects into a single df #################################################################################

# select vars
trial_vars<-c("a.side", "C", "rt", "R", "A", "MRI", "whichscanner", "ord", "subjn",
              "A", "vers.ord", "Drink.Type", "chosen.A", "chosen.J", "rating")

# loop across files
for (i in 1:length(files)) {
  
  list.t<-readMat(file.path(data_path, files[i], na.strings=""))
  
  subjn<-as.numeric(list.t[["subjn"]])
  
  if (subjn %in% redcap$ID) { # check if ID among included IDs
  
    list.select<-list.t[c(trial_vars)] # select vars of interest
    
    # reformat vars
    a.side<-matrix(list.select[["a.side"]])
    A<-matrix(list.select[["A"]])
    C<-matrix(list.select[["C"]])
    R<-matrix(list.select[["R"]])
    rt<-matrix(list.select[["rt"]])
    ID<-matrix(rep(as.numeric(list.select[["subjn"]]), each=50))
    MRI<-matrix(rep(as.numeric(list.select[["whichscanner"]]), each=50))
    if (length(MRI) == 0) {                                               # if there's no 'whichscanner' variable
      MRI<-matrix(rep(as.numeric(list.select[["MRI"]]), each=50))
    }
    ord<-matrix(rep(as.numeric(list.select[["ord"]]), each=50))
    vers.ord<-matrix(rep(as.numeric(list.select[["vers.ord"]]), each=50))
    Drink.Type<-matrix(rep(c(list.select[["Drink.Type"]]), each=50))
    chosen.A<-matrix(rep(c(list.select[["chosen.A"]]), each=50))
    chosen.J<-matrix(rep(c(list.select[["chosen.J"]]), each=50))
    taste1<-matrix(rep(as.numeric(list.select[["rating"]][[1]]), each=50))
    crave1<-matrix(rep(as.numeric(list.select[["rating"]][[2]]), each=50))
    taste2<-matrix(rep(as.numeric(list.select[["rating"]][[3]]), each=50))
    crave2<-matrix(rep(as.numeric(list.select[["rating"]][[4]]), each=50))
    
    # combine all vars into df
    df.select<-data.frame(ID,A,a.side,C,R,rt,MRI,ord,vers.ord,Drink.Type,chosen.A,chosen.J,taste1,crave1,taste2,crave2)
    
    # bind single subject and block dfs to one big df
    behav<-rbind(behav, df.select)
    
  } # end if statement

} # end for loop

###### behav_final: format df ######

# renaming and formatting
behav_final <- behav %>%
  dplyr::rename(choice = A, 
         side = a.side, 
         correct = C,
         outcome = R, 
         scanner = MRI,
         ord_JA = ord,
         ord_AB = vers.ord,
         reinforcer_type = Drink.Type,
         chosen_A = chosen.A,
         chosen_J = chosen.J) %>% 
  mutate(ID = as_factor(ID),
         scanner = as_factor(scanner),
         ord_JA = factor(ord_JA, labels=c("J-A", 
                                          "A-J")),
         ord_AB = factor(ord_AB, labels=c("A-B", 
                                          "B-A")),
         reinforcer_type = as_factor(reinforcer_type),
         chosen_A = as_factor(chosen_A),
         chosen_J = as_factor(chosen_J),
         outcome = ifelse(outcome == -1, 0, 1)) # recode outcome (0=no reward, 1=reward)
         #correct = ifelse(correct==1, 2, 1)) # recode correct (2=correct card, 1=incorrect card)

# creating new vars
behav_final <- behav_final %>%
  mutate(block = factor(ifelse(ord_JA == "J-A" & reinforcer_type == "J" | # var coding block number
                                 ord_JA == "A-J" & reinforcer_type == "A", 1,2)), 
         ID_block = paste(ID, '00', block, sep = "")) %>% # var combining ID and block number
  mutate(trial_block = rep(c(1:50),times=142)) %>%
  mutate(trial_ID = rep(c(1:100),times=71)) %>%
  mutate(stay = ifelse(lag(choice)==choice,1,0)) %>% # var coding if participant repeated the choice from the previous trial
  mutate(stay = ifelse(trial_block==1,NA,stay)) %>%
  mutate(win_stay = ifelse(lag(choice)==choice&lag(outcome)==1,1,0)) %>% # var coding if participant repeated the choice from the previous trial if it was rewarded in previous trial
  mutate(win_stay = ifelse(trial_block==1,NA,win_stay)) %>%
  mutate(nowin_switch = ifelse(lag(choice)!=choice&lag(outcome)==0,1,0)) %>% # var coding if participant switched the choice from the previous trial if it was not rewarded in previous trial
  mutate(nowin_switch = ifelse(trial_block==1,NA,nowin_switch))

n <- length(unique(behav_final$ID))

#sort
behav_final <- behav_final %>%
  arrange(ID,reinforcer_type)

# combine with redcap data
behav_final_redcap <- merge(behav_final,redcap,by='ID',all.x = TRUE)

###### save behav dfs #####################################################################################################################

### behav_final ###

save(file=file.path('C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_DATA/Behav', paste("behav_final_n", n, ".RData", sep="")), behav_final)
save(file=file.path('C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_DATA/Behav', paste("behav_final_redcap_n", n, ".RData", sep="")), behav_final_redcap)

##### Stan dfs #####################################################################################################################

##### main model #####

stan_input <- behav_final %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input[is.na(stan_input)]<- NA

stan_input_nona <- na.omit(stan_input)

##### hierarchical model #####

stan_input_hie <- behav_final_redcap %>%
  select(ID,trial_block,aud_group,reinforcer_type,correct,outcome) %>%
  mutate(aud_group = ifelse(aud_group == 'AUD',1,0), # AUD=1,HC=0
         reinforcer_type = ifelse(reinforcer_type == 'A',1,0)) # alc=1, jui=0

stan_input_hie[is.na(stan_input_hie)]<- NA

stan_input_hie_nona <- na.omit(stan_input_hie)

##### exploratory Stan dfs #####

### per reinforcer type ###

stan_input_alc <- behav_final_redcap[behav_final_redcap$reinforcer_type=='A',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_jui <- behav_final_redcap[behav_final_redcap$reinforcer_type=='J',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

### per AUD group ###

stan_input_aud <- behav_final_redcap[behav_final_redcap$aud_group=='AUD',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_hc <- behav_final_redcap[behav_final_redcap$aud_group=='HC',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

### per AUD group and reinforcer type ###

stan_input_alc_aud <- behav_final_redcap[behav_final_redcap$reinforcer_type=='A'&behav_final_redcap$aud_group=='AUD',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_alc_hc <- behav_final_redcap[behav_final_redcap$reinforcer_type=='A'&behav_final_redcap$aud_group=='HC',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_jui_aud <- behav_final_redcap[behav_final_redcap$reinforcer_type=='J'&behav_final_redcap$aud_group=='AUD',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_jui_hc <- behav_final_redcap[behav_final_redcap$reinforcer_type=='J'&behav_final_redcap$aud_group=='HC',] %>%
  select(ID_block,trial_block,correct,outcome) #%>%
  #mutate(ID_block = as.numeric(ID_block))

### outcome modulated by taste ratings ###

# get df with taste and craving ratings per subject and block
behav_rating <- behav_final_redcap %>%
  group_by(ID,reinforcer_type) %>%
  select(ID,reinforcer_type,ID_block,taste1, taste2, crave1, crave2) %>%
  distinct()

# create continuous vector from taste1 to taste2
taste_continuous <- c()
ID_block_taste <- c()

for (s in 1:length(behav_rating$ID)) {
  temp_taste <- seq(behav_rating$taste1[s], behav_rating$taste2[s], length.out = 50)
  temp_id_taste <- rep(behav_rating$ID_block[s], times=50)
  taste_continuous <- append(taste_continuous,temp_taste)
  ID_block_taste <- append(ID_block_taste,temp_id_taste)
}

# append continuous vector to final df
behav_final_redcap$taste_continuous <- taste_continuous

# create outcome weigthed by continuous taste rating
behav_final_redcap$outcome_taste_cont <- behav_final_redcap$taste_continuous*behav_final_redcap$outcome

#check if ID_block_taste created in loop and ID_block from final df are identical (so that taste_continuous values are put to the right place)
table(behav_final_redcap$ID_block == ID_block_taste)

# create outcome weighted by initial taste rating
behav_final_redcap$outcome_taste1 <- behav_final_redcap$taste1*behav_final_redcap$outcome

# create stan inputs
stan_input_taste_cont <- behav_final_redcap %>%
  select(ID_block,trial_block,correct,outcome_taste_cont) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_taste1 <- behav_final_redcap %>%
  select(ID_block,trial_block,correct,outcome_taste1) #%>%
  #mutate(ID_block = as.numeric(ID_block))

### outcome modulated by craving ratings ###

# create continuous vector from taste1 to taste2
crave_continuous <- c()
ID_block_crave <- c()

for (s in 1:length(behav_rating$ID)) {
  temp_crave <- seq(behav_rating$crave1[s], behav_rating$crave2[s], length.out = 50)
  temp_id_crave <- rep(behav_rating$ID_block[s], times=50)
  crave_continuous <- append(crave_continuous,temp_crave)
  ID_block_crave <- append(ID_block_crave,temp_id_crave)
}

# append continuous vector to final df
behav_final_redcap$crave_continuous <- crave_continuous

# create outcome weigthed by continuous crave rating
behav_final_redcap$outcome_crave_cont <- behav_final_redcap$crave_continuous*behav_final_redcap$outcome

#check if ID_block_taste created in loop and ID_block from final df are identical (so that taste_continuous values are put to the right place)
table(behav_final_redcap$ID_block == ID_block_crave)

# create outcome weighted by initial crave rating
behav_final_redcap$outcome_crave1 <- behav_final_redcap$crave1*behav_final_redcap$outcome

# create Stan inputs
stan_input_crave_cont <- behav_final_redcap %>%
  select(ID_block,trial_block,correct,outcome_crave_cont) #%>%
  #mutate(ID_block = as.numeric(ID_block))

stan_input_crave1 <- behav_final_redcap %>%
  select(ID_block,trial_block,correct,outcome_crave1) #%>%
  #mutate(ID_block = as.numeric(ID_block))

###### save stan_input ###################################################################################################################

# variable called choice in stan dfs is = correct from behav df

### main ###

write.table(stan_input, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_nona, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_nona.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

### hierarchical ###

write.table(stan_input_hie, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_hierarchical.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","group","condition","choice","outcome"))

write.table(stan_input_hie_nona, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_hierarchical_nona.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","group","condition","choice","outcome"))

### exploratory ###

# separate per aud group and reinforcer type

write.table(stan_input_alc, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_alc.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_jui, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_jui.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_aud, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_aud.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_hc, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_hc.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_alc_aud, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_alc_aud.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_alc_hc, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_alc_hc.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_jui_aud, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_jui_aud.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_jui_hc, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_jui_hc.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

# with outcome modulated by continuous taste rating

write.table(stan_input_taste_cont, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_taste_cont.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_taste1, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_taste1.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_crave_cont, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_crave_cont.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))

write.table(stan_input_crave1, file='C:/Users/musialm/OneDrive - Charité - Universitätsmedizin Berlin/PhD/04_B01/ILT/WP2_ILT_CODE/Stan Modeling/Input/Stan_input_crave1.txt', append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = c("subjID","trial","choice","outcome"))
