# Plot hospitals' preference: eFigure 1, 2, and 3

################################################################################
################################################################################
# Plot surgeons' preference
################################################################################
################################################################################
setwd("<data file>")
library(ggplot2)
################################################################################
# Auxiliary function: process data by region
################################################################################
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

################################################################################
# Hospitals' overall preference (all regions combined): eFigure 1
################################################################################
ggplot(hosp_summary_data, aes(x = preference)) +
  geom_histogram(fill = 'white', color = 'black', size = 1.2, bins = 50) +
  xlab("Hospitals' preference for TEE") +
  theme_bw(base_size = 25)

################################################################################
# Surgeons' preference by region: eFigure 2
################################################################################
ggplot(data = hosp_summary_data, aes(x = Region, y = preference)) + geom_boxplot() +
  ylab("Hospitals' Overall Preference for TEE")  + theme_bw(base_size = 20)


################################################################################
# Auxiliary data: process data by type
################################################################################
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
  
  hosp_data_all_types = rbind(process_hosp_data(hosp_data_AV_repair, '1. AV  \n repair'),
                              process_hosp_data(hosp_data_AV_replacement, '2. AV \n replacement'),
                              process_hosp_data(hosp_data_MV_repair, '3. MV \n repair'),
                              process_hosp_data(hosp_data_AV_replacement, '4. MV \n replacement'),
                              process_hosp_data(hosp_data_TV, '5. Tricuspid \n valve repair \n and \n replacement'),
                              process_hosp_data(hosp_data_PV, '6. Pulmonic \n valve repair \n and \n replacement'),
                              process_hosp_data(hosp_data_ben, '7. Aortic \n root/valved \n conduit \n (Bentall)'),
                              process_hosp_data(hosp_data_root, '8. AV \n sparing \n root'),
                              process_hosp_data(hosp_data_ben, '9. Aortic \n homograft \n or non-valved \n conduit'))
  
  hosp_summary_data = rbind(hosp_summary_data, hosp_data_all_types)
}

hosp_summary_data = hosp_summary_data[complete.cases(hosp_summary_data),]
hosp_summary_data = as.data.frame(hosp_summary_data)
colnames(hosp_summary_data) = c('HospID', 'Total_surg_vol', 'TEE', 'no_TEE', 'Preference', 'Type')
hosp_summary_data$Preference = as.numeric(hosp_summary_data$Preference)


################################################################################
# Hospitals' preference by type: eFigure 3
################################################################################
ggplot(data = hosp_summary_data, aes(x = Type, y = Preference)) + geom_boxplot() +
  ylab("Hospitals' Overall Preference for TEE")  +
  theme_bw(base_size = 22, base_family = 'serif')

