# This file reads the whole dataset ~900K and save them
# in separate files according to SurgID.

library(data.table)
library(dplyr)

# Read each dataset, select relevant columns, and finally
# combine all datasets
# 905533 in total to start with
setwd("<data file>")

dt_selected_row = NULL
for (i in 1:91){
  cat(i, '\n')
  dt = read.csv(paste0('data in chunks/data_', i, '.csv', sep = ''))
  dt_temp_selected_row = dt %>%
    select(SurgID, SiteID, HospID,
           TEE_b, Age, BSA, HeightCm, WeightKg, BMI, sex_e, race_e,
           ethnic_e, region_e, admit_source_e, surgyear,
           surgery_vol_Site_ID_yr, surgery_vol_Surg_ID_yr, surgery_vol_Hosp_ID_yr,
           Diabetes_preexist, Dyslip_preexist, Dialysis_preexist,
           HeartFail_preexist, ClassNYH_preexist_e, Hypertn_preexist,
           InfEndo_preexist, HmO2_preexist_e, SlpApn_preexist,
           LiverDis_preexist, Cancer_preexist, PVD_preexist,
           CVD_preexist, CVA_preexist, HDEF, PASYS, CreatLst, TotAlbumin,
           MELDScr, PrCAB_prior, PrValve_prior, IABP_placed_e,
           PerfusTm, ORDuration, SIDuration, CathtoSurg, CABG_e, multiple_valve_e,
           aortic_valve_replacement_b, aortic_valve_repair_b,
           AV_repair_replace_b, mitral_valve_replacement_b,
           mitral_valve_repair_b, MV_repair_replace_b,
           tricuspid_repair_replace_b, pulmonic_repair_replace_b,
           bentall_ao_root_valve_conduit_b, av_sparing_root_b,
           non_v_conduit_Ross_homograft_b, plus_CABG_b,
           plus_other_cardiac_surg_b, exclude,
           PredMort, PredDeep, PredReop, PredStro, PredVent, 
           PredRenF, PredMM, Pred6D, Pred14D, PredCoefVrsn,
           Mt30Stat_outcome, CNStrokP_outcome, CNStrokP_Mt30Stat_outcome,
           reoperation_complication, PostCreat)
  dt_selected_row = rbind(dt_selected_row, dt_temp_selected_row)
}

######################################################################################
# Risk score quantiles
dt_selected_row$PredMort_quartile = as.integer(cut(dt_selected_row$PredMort, 
                      quantile(dt$PredMort, probs=0:4/4, na.rm = TRUE), 
                      include.lowest=TRUE))
dt_selected_row$PredMort_quartile[is.na(dt_selected_row$PredMort_quartile)] = 0

colnames(dt_selected_row) = c('SurgID', 'SiteID', 'HospID',
                              'TEE_b', 'Age', 'BSA', 'HeightCm', 'WeightKg', 'BMI', 'sex_e', 'race_e',
                              'ethnic_e', 'region_e', 'admit_source_e', 'surgyear',
                              'surgery_vol_Site_ID_yr', 'surgery_vol_Surg_ID_yr', 'surgery_vol_Hosp_ID_yr',
                              'Diabetes_preexist', 'Dyslip_preexist', 'Dialysis_preexist',
                              'HeartFail_preexist', 'ClassNYH_preexist_e', 'Hypertn_preexist',
                              'InfEndo_preexist', 'HmO2_preexist_e', 'SlpApn_preexist',
                              'LiverDis_preexist', 'Cancer_preexist', 'PVD_preexist',
                              'CVD_preexist', 'CVA_preexist', 'HDEF', 'PASYS', 'CreatLst', 'TotAlbumin',
                              'MELDScr', 'PrCAB_prior', 'PrValve_prior', 'IABP_placed_e',
                              'PerfusTm', 'ORDuration', 'SIDuration', 'CathtoSurg', 'CABG_e', 'multiple_valve_e',
                              'aortic_valve_replacement_b', 'aortic_valve_repair_b',
                              'AV_repair_replace_b', 'mitral_valve_replacement_b',
                              'mitral_valve_repair_b', 'MV_repair_replace_b',
                              'tricuspid_repair_replace_b', 'pulmonic_repair_replace_b',
                              'bentall_ao_root_valve_conduit_b', 'av_sparing_root_b',
                              'non_v_conduit_Ross_homograft_b', 'plus_CABG_b',
                              'plus_other_cardiac_surg_b', 'exclude',
                              'PredMort', 'PredDeep', 'PredReop', 'PredStro', 'PredVent', 
                              'PredRenF', 'PredMM', 'Pred6D', 'Pred14D', 'PredCoefVrsn',
                              'Mt30Stat_outcome', 'CNStrokP_outcome', 'CNStrokP_Mt30Stat_outcome',
                              'reoperation_complication', 'post_creat', 'PredMort_quartile')

# Exclude and arrive at the final cohort
dt_selected_row = dt_selected_row[is.na(dt_selected_row$exclude),]
# 872936 left; this is our final cohort
write.csv(dt_selected_row, '<data file>', 
          row.names = FALSE)

