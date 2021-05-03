# Plot hospitals' preference: eFigure 4, 5, and 6 

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
  
  surg_data_all_types = rbind(process_surg_data(surg_data_AV_repair, '1. AV  \n repair'),
                              process_surg_data(surg_data_AV_replacement, '2. AV \n replacement'),
                              process_surg_data(surg_data_MV_repair, '3. MV \n repair'),
                              process_surg_data(surg_data_AV_replacement, '4. MV \n replacement'),
                              process_surg_data(surg_data_TV, '5. Tricuspid \n valve repair \n and \n replacement'),
                              process_surg_data(surg_data_PV, '6. Pulmonic \n valve repair \n and \n replacement'),
                              process_surg_data(surg_data_ben, '7. Aortic \n root/valved \n conduit \n (Bentall)'),
                              process_surg_data(surg_data_root, '8. AV \n sparing \n root'),
                              process_surg_data(surg_data_ben, '9. Aortic \n homograft \n or non-valved \n conduit'))
  
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


