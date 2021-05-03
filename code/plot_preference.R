# Plot surgeons/hospitals' preference 

################################################################################
################################################################################
# Plot surgeons' preference
################################################################################
################################################################################
setwd("<data file>")

################################################################################
# Auxiliary function: process data by region
################################################################################
process_surg_data <- function(surg_data){
  surg_ID = surg_data$SurgID[1]
  total_surg_vol = dim(surg_data)[1]
  total_TEE = sum(surg_data$TEE_b)
  total_no_TEE = total_surg_vol - total_TEE
  region = surg_data$region_e[1]
  return(c(surg_ID, region, total_surg_vol, total_TEE, total_no_TEE))
}

surg_summary_data = NULL
all_files = list.files('./data by SurgID/')
for (i in 1:length(all_files)){
  cat(i, '\n')
  file = all_files[i]
  surg_data = read.csv(paste('./data by SurgID/', file, sep = ''))
  surg_summary_data = rbind(surg_summary_data, process_surg_data(surg_data))
}

surg_summary_data = as.data.frame(surg_summary_data)
colnames(surg_summary_data) = c('SurgID', 'Region','Total_surg_vol', 'TEE', 'no_TEE')
surg_summary_data$preference = as.numeric(surg_summary_data$TEE)/as.numeric(surg_summary_data$Total_surg_vol)

library(ggplot2)

################################################################################
# Surgeons' overall preference (all regions combined): eFigure 4
################################################################################
ggplot(surg_summary_data, aes(x = preference)) +
  geom_histogram(fill = 'white', color = 'black', size = 1.2, bins = 50) +
  xlab("Surgeons' overall preference for TEE") +
  geom_vline(xintercept = 0.3, color = 'red', size = 1.2, linetype = 'dashed') +
  geom_vline(xintercept = 0.7, color = 'red', size = 1.2, linetype = 'dashed') +
  theme_bw(base_size = 25)

################################################################################
# Surgeons' preference by region: eFigure 5
################################################################################
ggplot(data = surg_summary_data, aes(x = Region, y = preference)) + geom_boxplot() +
  ylab("Surgeons' Overall Preference for TEE")  + theme_bw(base_size = 20)

################################################################################
# Auxiliary function: process data by type
################################################################################
process_surg_data <- function(surg_data, type){
  surg_ID = surg_data$SurgID[1]
  total_surg_vol = dim(surg_data)[1]
  total_TEE = sum(surg_data$TEE_b)
  total_no_TEE = total_surg_vol - total_TEE
  preference = total_TEE/total_surg_vol
  return(c(surg_ID, total_surg_vol, total_TEE, total_no_TEE, preference, type))
}

surg_summary_data = NULL
all_files = list.files('./data by SurgID/')
for (i in 1:length(all_files)){
  file = all_files[i]
  surg_data = read.csv(paste('./data by SurgID/', file, sep = ''))
  surg_data_AV_repair = surg_data[surg_data$aortic_valve_repair_b == 1,]
  surg_data_AV_replacement = surg_data[surg_data$aortic_valve_replacement_b == 1,]
  surg_data_MV_repair = surg_data[surg_data$mitral_valve_repair_b == 1,]
  surg_data_MV_replacement = surg_data[surg_data$mitral_valve_replacement_b == 1,]
  surg_data_TV = surg_data[surg_data$tricuspid_repair_replace_b == 1,]
  surg_data_PV = surg_data[surg_data$pulmonic_repair_replace_b == 1,]
  surg_data_ben = surg_data[surg_data$bentall_ao_root_valve_conduit_b == 1,]
  surg_data_root = surg_data[surg_data$av_sparing_root_b == 1,]
  surg_data_ross = surg_data[surg_data$non_v_conduit_Ross_homograft_b == 1,]
  
  surg_data_all_types = rbind(process_surg_data(surg_data_AV_repair, 'AV  \n repair'),
                              process_surg_data(surg_data_AV_replacement, 'AV \n replacement'),
                              process_surg_data(surg_data_MV_repair, 'MV \n repair'),
                              process_surg_data(surg_data_AV_replacement, 'MV \n replacement'),
                              process_surg_data(surg_data_TV, 'Tricuspid \n valve repair \n and \n replacement'),
                              process_surg_data(surg_data_PV, 'Pulmonic \n valve repair \n and \n replacement'),
                              process_surg_data(surg_data_ben, 'Bentall aortic \n root/valve \n conduit'),
                              process_surg_data(surg_data_root, 'AV \n sparing \n root'),
                              process_surg_data(surg_data_ben, 'Ross \n homograft \n non-valved \n conduit'))
  
  surg_summary_data = rbind(surg_summary_data, surg_data_all_types)
}

surg_summary_data = surg_summary_data[complete.cases(surg_summary_data),]
surg_summary_data = as.data.frame(surg_summary_data)
colnames(surg_summary_data) = c('SurgID', 'Total_surg_vol', 'TEE', 'no_TEE', 'Preference', 'Type')
surg_summary_data$Preference = as.numeric(surg_summary_data$Preference)

################################################################################
# Surgeons' preference by type: eFigure 6
################################################################################
ggplot(data = surg_summary_data, aes(x = Type, y = Preference)) + geom_boxplot() +
  ylab("Surgeons' Overall Preference for TEE")  +
  theme_bw(base_size = 22, base_family = 'serif')














################################################################################
# Take a look at hospitals' preference for TEE by region
setwd("C:/Users/bzhan/Dropbox/with emily/surgeon_level_match/")

# Look at surgeons first
process_hosp_data <- function(hosp_data){
  hosp_ID = hosp_data$HospID[1]
  total_hosp_vol = dim(hosp_data)[1]
  total_TEE = sum(hosp_data$TEE_b)
  total_no_TEE = total_hosp_vol - total_TEE
  region = hosp_data$region_e[1]
  return(c(hosp_ID, region, total_hosp_vol, total_TEE, total_no_TEE))
}

hosp_summary_data = NULL
all_files = list.files('./data by HospID/')
for (i in 1:length(all_files)){
  cat(i, '\n')
  file = all_files[i]
  hosp_data = read.csv(paste('./data by HospID/', file, sep = ''))
  hosp_summary_data = rbind(hosp_summary_data, process_hosp_data(hosp_data))
}

hosp_summary_data = as.data.frame(hosp_summary_data)
colnames(hosp_summary_data) = c('HospID', 'Region','Total_hosp_vol', 'TEE', 'no_TEE')
hosp_summary_data$preference = as.numeric(hosp_summary_data$TEE)/as.numeric(hosp_summary_data$Total_hosp_vol)

library(ggplot2)

# Boxplots by region
ggplot(data = hosp_summary_data, aes(x = Region, y = preference)) + geom_boxplot() +
  ylab("Hospitals' Overall Preference for TEE")  + theme_bw(base_size = 20)

# Overall distribution (all region combined)
ggplot(hosp_summary_data, aes(x = preference)) +
  geom_histogram(fill = 'white', color = 'black', size = 1.2, bins = 50) +
  xlab("Hospitals' preference for TEE") +
  theme_bw(base_size = 25)



################################################################################
# Take a look at hospitals' preference for TEE by surgery type
setwd("C:/Users/bzhan/Dropbox/with emily/surgeon_level_match/")

process_hosp_data <- function(hosp_data, type){
  hosp_ID = hosp_data$HospID[1]
  total_hosp_vol = dim(hosp_data)[1]
  total_TEE = sum(hosp_data$TEE_b)
  total_no_TEE = total_hosp_vol - total_TEE
  preference = total_TEE/total_hosp_vol
  return(c(hosp_ID, total_hosp_vol, total_TEE, total_no_TEE, preference, type))
}

hosp_summary_data = NULL
all_files = list.files('./data by HospID/')
for (i in 1:length(all_files)){
  cat(i, '\n')
  file = all_files[i]
  hosp_data = read.csv(paste('./data by HospID/', file, sep = ''))
  hosp_data_AV_repair = hosp_data[hosp_data$aortic_valve_repair_b == 1,]
  hosp_data_AV_replacement = hosp_data[hosp_data$aortic_valve_replacement_b == 1,]
  hosp_data_MV_repair = hosp_data[hosp_data$mitral_valve_repair_b == 1,]
  hosp_data_MV_replacement = hosp_data[hosp_data$mitral_valve_replacement_b == 1,]
  hosp_data_TV = hosp_data[hosp_data$tricuspid_repair_replace_b == 1,]
  hosp_data_PV = hosp_data[hosp_data$pulmonic_repair_replace_b == 1,]
  hosp_data_ben = hosp_data[hosp_data$bentall_ao_root_valve_conduit_b == 1,]
  hosp_data_root = hosp_data[hosp_data$av_sparing_root_b == 1,]
  hosp_data_ross = hosp_data[hosp_data$non_v_conduit_Ross_homograft_b == 1,]
  
  hosp_data_all_types = rbind(process_hosp_data(hosp_data_AV_repair, 'AV \n Repair'),
                              process_hosp_data(hosp_data_AV_replacement, 'AV \n Replace'),
                              process_hosp_data(hosp_data_MV_repair, 'MV \n Repair'),
                              process_hosp_data(hosp_data_AV_replacement, 'MV \n Replace'),
                              process_hosp_data(hosp_data_TV, 'TV Repair \n and Replace'),
                              process_hosp_data(hosp_data_PV, 'PV Repair \n and Replace'),
                              process_hosp_data(hosp_data_ben, 'Bentall aortic \n root/valve \n conduit'),
                              process_hosp_data(hosp_data_root, 'AV sparing \n root'),
                              process_hosp_data(hosp_data_ben, 'Ross homograft \n non-valved \n conduit'))
  
  hosp_summary_data = rbind(hosp_summary_data, hosp_data_all_types)
}

hosp_summary_data = hosp_summary_data[complete.cases(hosp_summary_data),]
hosp_summary_data = as.data.frame(hosp_summary_data)
colnames(hosp_summary_data) = c('HospID', 'Total_surg_vol', 'TEE', 'no_TEE', 'Preference', 'Type')
hosp_summary_data$Preference = as.numeric(hosp_summary_data$Preference)


library(ggplot2)

ggplot(data = hosp_summary_data, aes(x = Type, y = Preference)) + geom_boxplot() +
  ylab("Hospitals' Preference for TEE")  + theme_bw(base_size = 20)




##############################################################################################
# Take a look at each surgeon's patients
all_files = list.files('./data by SurgID/')


process_surg_data <- function(surg_data){
  surg_ID = surg_data$SurgID[1]
  total_surg_vol = dim(surg_data)[1]
  total_TEE = sum(surg_data$TEE_b)
  total_no_TEE = total_surg_vol - total_TEE
  return(c(surg_ID, total_surg_vol, total_TEE, total_no_TEE))
}


surg_summary_data = NULL
for (i in 1:length(all_files)){
  file = all_files[i]
  surg_data = read.csv(paste('./data by SurgID/', file, sep = ''))
  surg_summary_data = rbind(surg_summary_data, process_surg_data(surg_data))
}

surg_summary_data = as.data.frame(surg_summary_data)
colnames(surg_summary_data) = c('SurgID', 'Total_surg_vol', 'TEE', 'no_TEE')
surg_summary_data$preference = surg_summary_data$TEE/surg_summary_data$Total_surg_vol

# Take a look at how many preference is between 0.4 and 0.6
# and take a look at the surgical volume for these surgeons

surg_summary_between_4_6 = surg_summary_data[(surg_summary_data$preference >= 0.4 &
                                                surg_summary_data$preference <= 0.6),]
# How many surgeons
dim(surg_summary_between_4_6)
# 273

# How many patients
sum(surg_summary_between_4_6$Total_surg_vol)

# Save these 273 surgeons into a separate folder
for (ID in surg_summary_between_4_6$SurgID){
  file.copy(paste('./data by SurgID/', ID, '.csv', sep = ''),
            './surg_between_4_and_6/')
}


library(ggplot2)
ggplot(surg_summary_between_4_6, aes(x = Total_surg_vol)) +
  geom_histogram(fill = 'white', color = 'black', size = 1.2, bins = 50) +
  xlab('Total surgical volume') +
  theme_bw(base_size = 25)


surg_summary_between_3_7 = surg_summary_data[(surg_summary_data$preference >= 0.3 &
                                                surg_summary_data$preference <= 0.7),]
ggplot(surg_summary_between_3_7, aes(x = Total_surg_vol)) +
  geom_histogram(fill = 'white', color = 'black', size = 1.2, bins = 50) +
  xlab('Total surgical volume') +
  theme_bw(base_size = 25)

# How many surgeons
dim(surg_summary_between_3_7)

# How many patients
sum(surg_summary_between_3_7$Total_surg_vol)


# Save these 285 surgeons into a separate folder
for (ID in surg_summary_between_3_7$SurgID){
  file.copy(paste('./data by SurgID/', ID, '.csv', sep = ''),
            './surg_between_3_and_7/')
}











