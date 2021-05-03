# This file checks balance after statistical matching and makes plots
setwd('<data file>')
treated_all = read.csv('./matched data/all_treated_data.csv')
control_all = read.csv('./matched data/all_control_data.csv')
X_treat_control = rbind(treated_all, control_all)

#############################################################################
# Table One format summary: eTable 8-9

vars = c('Age', 'BSA', 'BSA_missing', 'BMI', 'BMI_missing', 
         'sex_male', 'race_white', 'race_black',
         'admission_ER', 'admission_transfer', 'admission_others',
         'Diabetes_preexist', 'Dyslip_preexist', 'Dialysis_preexist',
         'Hypertn_preexist', 'ClassNYH_preexist_e',
         'InfEndo_preexist', 'HmO2_preexist_e', 'SlpApn_preexist',
         'LiverDis_preexist', 'Cancer_preexist', 'PVD_preexist',
         'CVD_preexist', 'CVA_preexist', 'PASYS', 'PASYS_missing',
         'HDEF', 'HDEF_missing', 'HDEF_normal',
         'CreatLst', 'CreatLst_missing', 'TotAlbumin', 'TotAlbumin_missing',
         'MELDScr', 'MELDScr_missing', 'PrCAB_prior', 'PrValve_prior', 
         'IABP_placed','CathtoSurg', 'CathtoSurg_missing',
         'PerfusTm', 'PerfusTm_missing', 
         'ORDuration', 'ORDuration_missing',
         'SIDuration', 'SIDuration_missing',
         'multiple_valve_1','PredMort', 
         'PredMort_quartile',
         'AV_repair', 'AV_replacement', 'MV_repair', 'MV_replacement',
         'T_repair_replacement', 'pulmonic_repair_replace',
         'bentall', 'av_sparing_root', 'non_v_conduit_Ross_homograft',
         'CABG', 'other_cardiac_surg', 'surg_vol', 'hosp_vol')

fVars = c('BSA_missing', 'BMI_missing', 
          'sex_male', 'race_white', 'race_black',
          'admission_ER', 'admission_transfer', 'admission_others',
          'Diabetes_preexist', 'Dyslip_preexist', 'Dialysis_preexist',
          'Hypertn_preexist', 'ClassNYH_preexist_e',
          'InfEndo_preexist', 'HmO2_preexist_e', 'SlpApn_preexist',
          'LiverDis_preexist', 'Cancer_preexist', 'PVD_preexist',
          'CVD_preexist', 'CVA_preexist', 'PASYS_missing',
          'HDEF_missing', 'HDEF_normal',
          'CreatLst_missing', 'TotAlbumin_missing',
          'MELDScr_missing', 'PrCAB_prior', 'PrValve_prior', 
          'IABP_placed','CathtoSurg_missing',
          'PerfusTm_missing', 
          'ORDuration_missing',
          'SIDuration_missing',
          'multiple_valve_1', 
          'PredMort_quartile',
          'AV_repair', 'AV_replacement', 'MV_repair', 'MV_replacement',
          'T_repair_replacement', 'pulmonic_repair_replace',
          'bentall', 'av_sparing_root', 'non_v_conduit_Ross_homograft',
          'CABG', 'other_cardiac_surg')


library(tableone)
tb_TEE = CreateTableOne(vars = vars, data = X_treat_control[X_treat_control$TEE_b == 1,], factorVars = fVars)
tb_TEE = print(tb_TEE, contDigits = 2, catDigits = 2)

tb_no_TEE = CreateTableOne(vars = vars, data = X_treat_control[X_treat_control$TEE_b == 0,], factorVars = fVars)
tb_no_TEE = print(tb_no_TEE, contDigits = 2, catDigits = 2)


tb_final = cbind(tb_no_TEE, tb_TEE)
xtable::xtable(cbind(tb_no_TEE, tb_TEE))


##################################################################
# Calculate standardized mean differences
library(data.table)
data_before = fread('<data file>')
data_before_tee = data_before[data_before$TEE == 1,]
data_before_no_tee = data_before[data_before$TEE == 0,]
library(RItools)

data_before$admission_others = 1 - data_before$admission_ER - data_before$admission_transfer
tb_before = xBalance(TEE ~ Age + BSA + BSA_missing + BMI + BMI_missing+
                       sex_male + race_white + race_black +
                       admission_ER + admission_transfer + admission_others +
                       Diabetes_preexist + Dyslip_preexist + Dialysis_preexist +
                       Hypertn_preexist + factor(ClassNYH_preexist_e) +
                       InfEndo_preexist + HmO2_preexist_e + SlpApn_preexist +
                       LiverDis_preexist + Cancer_preexist + PVD_preexist +
                       CVD_preexist + CVA_preexist+ PASYS + PASYS_missing +
                       HDEF + HDEF_missing + HDEF_normal +
                       CreatLst + CreatLst_missing + TotAlbumin + TotAlbumin_missing +
                       MELDScr + MELDScr_missing + PrCAB_prior + PrValve_prior +
                       IABP_placed + CathtoSurg + CathtoSurg_missing+
                       PerfusTm + PerfusTm_missing +
                       ORDuration + ORDuration_missing +
                       SIDuration + SIDuration_missing +
                       multiple_valve_1 + PredMort+ 
                       factor(dt.PredMort_quartile) +
                       AV_repair + AV_replacement + MV_repair + MV_replacement +
                       T_repair_replacement + pulmonic_repair_replace +
                       bentall + av_sparing_root + non_v_conduit_Ross_homograft +
                       CABG + other_cardiac_surg, data = data_before,
                     report = c('adj.means', 'std.diffs'))


tb_before = tb_before$results[,,]
mean_tee_0 = tb_before[,1]
mean_tee_1 = tb_before[,2]
std_diff_before = tb_before[,3]

# Calculate pooled standard deviation before matching
pooled_sd = (mean_tee_1 - mean_tee_0)/std_diff_before


# Data after within-surgeon matching
X_treat_control$admission_others = 1 - X_treat_control$admission_ER - X_treat_control$admission_transfer
X_treat_control$PredMort[is.na(X_treat_control$PredMort)] = mean(X_treat_control$PredMort[!is.na(X_treat_control$PredMort)])

tb_after = xBalance(TEE_b ~ Age + BSA + BSA_missing + BMI + BMI_missing+
                      sex_male + race_white + race_black +
                      admission_ER + admission_transfer + admission_others +
                      Diabetes_preexist + Dyslip_preexist + Dialysis_preexist +
                      Hypertn_preexist + factor(ClassNYH_preexist_e) +
                      InfEndo_preexist + HmO2_preexist_e + SlpApn_preexist +
                      LiverDis_preexist + Cancer_preexist + PVD_preexist +
                      CVD_preexist + CVA_preexist+ PASYS + PASYS_missing +
                      HDEF + HDEF_missing + HDEF_normal +
                      CreatLst + CreatLst_missing + TotAlbumin + TotAlbumin_missing +
                      MELDScr + MELDScr_missing + PrCAB_prior + PrValve_prior +
                      IABP_placed + CathtoSurg + CathtoSurg_missing +
                      PerfusTm + PerfusTm_missing +
                      ORDuration + ORDuration_missing +
                      SIDuration + SIDuration_missing +
                      multiple_valve_1 + PredMort+ 
                      factor(PredMort_quartile) +
                      AV_repair + AV_replacement + MV_repair + MV_replacement +
                      T_repair_replacement + pulmonic_repair_replace +
                      bentall + av_sparing_root + non_v_conduit_Ross_homograft +
                      CABG + other_cardiac_surg, data = X_treat_control,
                    report = c('adj.means', 'std.diffs'))

tb_after = tb_after$results[,,]
mean_tee_0_after = tb_after[,1]
mean_tee_1_after = tb_after[,2]

# This is the SMD after matching
std_diff_after = (mean_tee_1_after - mean_tee_0_after)/pooled_sd





