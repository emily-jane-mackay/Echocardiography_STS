# This file saves the final cohort data by surgeon/hospital ID

#########################################################################################
setwd("<data file>")
dt_selected_row = read.csv('<data file>/final_cohort_data/STS_data_relevant_rows.csv')

dir.create('<data file>/final_cohort_data/data by SurgID')
setwd('<data file>/final_cohort_data/data by SurgID')
for (surgID in unique(dt_selected_row$SurgID)){
  subset_data = dt_selected_row[which(dt_selected_row$SurgID == surgID),]
  file_name = paste0(surgID, '.csv', sep = '')
  write.csv(subset_data, file_name, row.names = FALSE)
}

dir.create('<data file>/final_cohort_data/data by HospID')
setwd('<data file>/final_cohort_data/data by HospID')
for (hospID in unique(dt_selected_row$HospID)){
  subset_data = dt_selected_row[which(dt_selected_row$HospID == hospID),]
  file_name = paste0(hospID, '.csv', sep = '')
  write.csv(subset_data, file_name, row.names = FALSE)
}



##############################################################################################
# Calculate each surgeon's preference and save equivocal surgeons to a separate folder

all_files = list.files('<data file>/final_cohort_data/data by SurgID')

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
  surg_data = read.csv(paste('<data file>/final_cohort_data/data by SurgID/', 
                             file, sep = ''))
  surg_summary_data = rbind(surg_summary_data, process_surg_data(surg_data))
}

surg_summary_data = as.data.frame(surg_summary_data)
colnames(surg_summary_data) = c('SurgID', 'Total_surg_vol', 'TEE', 'no_TEE')
surg_summary_data$preference = surg_summary_data$TEE/surg_summary_data$Total_surg_vol

# Surgeons whose preference is between 0.3 and 0.7
surg_summary_between_3_7 = surg_summary_data[(surg_summary_data$preference >= 0.3 &
                                                surg_summary_data$preference <= 0.7),]
# How many equivocal surgeons
dim(surg_summary_between_3_7)
# 595

# How many patients treated by equivocal surgeons
sum(surg_summary_between_3_7$Total_surg_vol)
# 140,398

dir.create('<data file>/final_cohort_data/equivocal surgeon data')
# Save these equivocal surgeons (preference between 0.3 and 0.7) into a separate folder
for (ID in surg_summary_between_3_7$SurgID){
  file.copy(paste('<data file>/final_cohort_data/data by SurgID/', ID, '.csv', sep = ''),
            '<data file>/final_cohort_data/equivocal surgeon data/')
}






