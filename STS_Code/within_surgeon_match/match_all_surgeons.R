# Load matching functions in the file match_one_surg.R
source('<data file>')


#######################################################
# For each surgeon ID, do the matching if we can
all_files = list.files('<data file>')
K = length(all_files) # number of surgeons to be matched: 3159
for (i in 1:K){
  ID = as.numeric(strsplit(all_files[i], split = '[.]')[[1]][1])
  cat(i, ID, '\n')
  try_match_one_surgeon(ID)
}


##############################################################
# Pool all matched patients together and save the matched treated 
# and control cohorts.
setwd('<data file>')
all_matched_files = list.files('./match_by_surgID/')

treated_all = NULL
control_all = NULL

for (ID in all_matched_files){
  treated_temp = read.csv(paste('./match_by_surgID/', ID, '/treated.csv', sep = ''))
  control_temp = read.csv(paste('./match_by_surgID/', ID, '/control.csv', sep = ''))
  orig_temp = read.csv(paste('./match_by_surgID/', ID, '/orig_data.csv', sep = ''))
  
  orig_dir_input = paste('<data file>', 
                    ID, '.csv', sep = '')
  orig_temp_2 = read.csv(orig_dir_input)
  
  # Add primary, secondary, and negative control outcomes to the matched data
  treated_temp$death = orig_temp$Mt30Stat_outcome[treated_temp$X]
  control_temp$death = orig_temp$Mt30Stat_outcome[control_temp$X]
  
  treated_temp$TEE_missing = orig_temp_2$TEE_missing[treated_temp$X]
  control_temp$TEE_missing = orig_temp_2$TEE_missing[control_temp$X]
  
  treated_temp$postcreat =  orig_temp$post_creat[treated_temp$X]
  control_temp$postcreat =  orig_temp$post_creat[control_temp$X]
  
  treated_temp$stroke_comp =  orig_temp$CNStrokP_Mt30Stat_outcome[treated_temp$X]
  control_temp$stroke_comp =  orig_temp$CNStrokP_Mt30Stat_outcome[control_temp$X]
  
  treated_temp$reop_complication = orig_temp$reoperation_complication[treated_temp$X]
  control_temp$reop_complication = orig_temp$reoperation_complication[control_temp$X]
  
  # Add SurgID and HospID to the matched data
  treated_temp$SurgID = orig_temp$SurgID[treated_temp$X]
  control_temp$SurgID = orig_temp$SurgID[control_temp$X]
  
  treated_temp$HospID = orig_temp$HospID[treated_temp$X]
  control_temp$HospID = orig_temp$HospID[control_temp$X]
  
  treated_all = rbind(treated_all, treated_temp)
  control_all = rbind(control_all, control_temp)
}

# Read in the surgical and hospital volume data
surg_vol = read.csv('<data file>')
hosp_vol = read.csv('<data file>')

# Merge volume data according to SurgID and HospID
treated_all = merge(treated_all, surg_vol, by = 'SurgID')
treated_all = merge(treated_all, hosp_vol, by = 'HospID')
control_all = merge(control_all, surg_vol, by = 'SurgID')
control_all = merge(control_all, hosp_vol, by = 'HospID')



write.csv(treated_all, './matched data/all_treated_data.csv', row.names = FALSE)
write.csv(control_all, './matched data/all_control_data.csv', row.names = FALSE)
