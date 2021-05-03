# Make eTable 1 in the eAppendix.


# Calculate before match standardized differences and mean differences
library(data.table)
dt = fread('<data file>')

# Calculate valve surgery volume
library(dplyr)
dt = dt%>%
  group_by(SurgID) %>%
  mutate(surg_vol = n()) %>%
  ungroup()

dt = dt%>%
  group_by(HospID) %>%
  mutate(hosp_vol = n()) %>%
  ungroup()

# Re-code variables and missing values
dt$BSA_missing = is.na(dt$BSA) + 0
dt$BSA[is.na(dt$BSA)] = mean(dt$BSA, na.rm = TRUE)

dt$HeightCm_missing = is.na(dt$HeightCm) + 0
dt$HeightCm[is.na(dt$HeightCm)] = mean(dt$HeightCm, na.rm = TRUE)

dt$WeightKg_missing = is.na(dt$WeightKg) + 0
dt$WeightKg[is.na(dt$WeightKg)] = mean(dt$WeightKg, na.rm = TRUE)

dt$BMI_missing = is.na(dt$BMI) + 0
dt$BMI[is.na(dt$BMI)] = mean(dt$BMI, na.rm = TRUE)

dt$sex_male = (dt$sex_e == 'male') + 0
dt$race_white = (dt$race_e == 'white') + 0
dt$race_black = (dt$race_e == 'black') + 0
dt$admission_ER = (dt$admit_source_e == 'ER') + 0
dt$admission_elective = (dt$admit_source_e == 'elective') + 0
dt$admission_transfer = (dt$admit_source_e == 'transfer') + 0
dt$admission_elective_others = 1 - dt$admission_ER - dt$admission_transfer

# Preexist conditions
dt$HmO2_preexist_e = (dt$HmO2_preexist_e == '+O2') + 0
dt$ClassNYH_preexist_e = as.character(dt$ClassNYH_preexist_e)
dt$ClassNYH_preexist_e[which(dt$ClassNYH_preexist_e == 'none')] = '0' # an ordinal variable
dt$ClassNYH_preexist_e = as.numeric(dt$ClassNYH_preexist_e)

# Lab measurements may have missing values
# Create a missing indicator + mean imputation
dt$PASYS_missing = is.na(dt$PASYS) + 0
dt$PASYS[is.na(dt$PASYS)] = mean(dt$PASYS, na.rm = TRUE)
dt$HDEF_missing = is.na(dt$HDEF) + 0
dt$HDEF_normal = (dt$HDEF >= 55 & dt$HDEF <= 70) + 0
dt$HDEF_normal[is.na(dt$HDEF_normal)] = 0
dt$HDEF[is.na(dt$HDEF)] = mean(dt$HDEF, na.rm = TRUE)
dt$CreatLst_missing = is.na(dt$CreatLst) + 0
dt$CreatLst[is.na(dt$CreatLst)] = mean(dt$CreatLst, na.rm = TRUE)
dt$TotAlbumin_missing = is.na(dt$TotAlbumin) + 0
dt$TotAlbumin[is.na(dt$TotAlbumin)] = mean(dt$TotAlbumin, na.rm = TRUE)
dt$MELDScr_missing = is.na(dt$MELDScr) + 0
dt$MELDScr[is.na(dt$MELDScr)] = mean(dt$MELDScr, na.rm = TRUE)
dt$PerfusTm_missing = is.na(dt$PerfusTm) + 0
dt$PerfusTm[is.na(dt$PerfusTm)] = mean(dt$PerfusTm, na.rm = TRUE)
abnormal_ORD = which(dt$ORDuration > 4000)
dt$ORDuration[abnormal_ORD] = NA
dt$ORDuration_missing = is.na(dt$ORDuration) + 0
dt$ORDuration[is.na(dt$ORDuration)] = mean(dt$ORDuration, na.rm = TRUE)
dt$SIDuration_missing = is.na(dt$SIDuration) + 0
dt$SIDuration[is.na(dt$SIDuration)] = mean(dt$SIDuration, na.rm = TRUE)


# Procedures
abnormal_cathtosurg = which(dt$CathtoSurg < 0 | dt$CathtoSurg > 720)
dt$CathtoSurg[abnormal_cathtosurg] = NA
dt$CathtoSurg_missing = is.na(dt$CathtoSurg) + 0
dt$CathtoSurg[is.na(dt$CathtoSurg)] = median(dt$CathtoSurg, na.rm = TRUE)
dt$IABP_placed = (dt$IABP_placed_e == 'Intraop IABP' | dt$IABP_placed_e == 'Preop IABP') + 0
dt$multiple_valve_1 = (dt$multiple_valve_e == '' | is.na(dt$multiple_valve_e)) + 0

dt$AV_repair = (dt$aortic_valve_repair_b) + 0
dt$AV_replacement = (dt$aortic_valve_replacement_b) + 0
dt$MV_repair = (dt$mitral_valve_repair_b) + 0
dt$MV_replacement = (dt$mitral_valve_replacement_b) + 0
dt$T_repair_replacement = (dt$tricuspid_repair_replace_b) + 0
dt$pulmonic_repair_replace = (dt$pulmonic_repair_replace_b) + 0

# Three AP subtypes
dt$bentall = (dt$bentall_ao_root_valve_conduit_b) + 0
dt$av_sparing_root = (dt$av_sparing_root_b) + 0  
dt$non_v_conduit_Ross_homograft = (dt$non_v_conduit_Ross_homograft_b) + 0

#Plus CABG 
dt$CABG = dt$plus_CABG_b
#Plus other cardiac surgery
dt$other_cardiac_surg = dt$plus_other_cardiac_surg_b

# Predicted scores
dt$PredMort_missing = is.na(dt$PredMort) + 0
dt$PredMort[is.na(dt$PredMort)] = mean(dt$PredMort, na.rm = TRUE)
dt$PredMort[dt$PredMort>=1] = 1 # predicted mortality is at largest 1
dt$PredDeep_missing = is.na(dt$PredDeep) + 0
dt$PredDeep[is.na(dt$PredDeep)] = mean(dt$PredDeep, na.rm = TRUE)
dt$PredReop_missing = is.na(dt$PredReop) + 0
dt$PredReop[is.na(dt$PredReop)] = mean(dt$PredReop, na.rm = TRUE)
dt$PredStro_missing = is.na(dt$PredStro) + 0
dt$PredStro[is.na(dt$PredStro)] = mean(dt$PredStro, na.rm = TRUE)
dt$PredVent_missing = is.na(dt$PredVent) + 0
dt$PredVent[is.na(dt$PredVent)] = mean(dt$PredVent, na.rm = TRUE)
dt$PredRenF_missing = is.na(dt$PredRenF) + 0
dt$PredRenF[is.na(dt$PredRenF)] = mean(dt$PredRenF, na.rm = TRUE)
dt$PredMM_missing = is.na(dt$PredMM) + 0
dt$PredMM[is.na(dt$PredMM)] = mean(dt$PredMM, na.rm = TRUE)
dt$Pred6D_missing = is.na(dt$Pred6D) + 0
dt$Pred6D[is.na(dt$Pred6D)] = mean(dt$Pred6D, na.rm = TRUE)
dt$Pred14D_missing = is.na(dt$Pred14D) + 0
dt$Pred14D[is.na(dt$Pred14D)] = mean(dt$Pred14D, na.rm = TRUE)


X = dt %>%
  dplyr::select(Age, BSA, BSA_missing, HeightCm, HeightCm_missing, 
                WeightKg, WeightKg_missing, BMI, BMI_missing, 
                sex_male, race_white, race_black,
                admission_ER, admission_elective, admission_transfer, 
                admission_elective_others,
                Diabetes_preexist, Dyslip_preexist, Dialysis_preexist,
                HeartFail_preexist, ClassNYH_preexist_e, Hypertn_preexist,
                InfEndo_preexist, HmO2_preexist_e, SlpApn_preexist,
                LiverDis_preexist, Cancer_preexist, PVD_preexist,
                CVD_preexist, CVA_preexist, PASYS, PASYS_missing,
                HDEF, HDEF_missing, HDEF_normal,
                CreatLst, CreatLst_missing, TotAlbumin, TotAlbumin_missing,
                MELDScr, MELDScr_missing, PrCAB_prior, PrValve_prior,
                CathtoSurg_missing, CathtoSurg,
                IABP_placed, PerfusTm, PerfusTm_missing, 
                ORDuration, ORDuration_missing,
                SIDuration, SIDuration_missing,
                multiple_valve_1,
                PredMort, PredMort_missing, PredDeep, PredDeep_missing,
                PredReop, PredReop_missing, PredStro, PredStro_missing,
                PredVent, PredVent_missing, PredRenF, PredRenF_missing,
                PredMM, PredMM_missing, Pred6D, Pred6D_missing,
                Pred14D, Pred14D_missing,
                AV_repair, AV_replacement, MV_repair, MV_replacement,
                T_repair_replacement, pulmonic_repair_replace,
                bentall, av_sparing_root, non_v_conduit_Ross_homograft,
                CABG, other_cardiac_surg, hosp_vol, surg_vol,
                TEE_b, HospID)


vars = c('Age', 'BSA', 'BSA_missing', 'HeightCm', 'HeightCm_missing', 
        'WeightKg', 'WeightKg_missing', 'BMI', 'BMI_missing', 
        'sex_male', 'race_white', 'race_black',
        'admission_ER', 'admission_elective', 'admission_transfer', 
        'admission_elective_others',
        'Diabetes_preexist', 'Dyslip_preexist', 'Dialysis_preexist',
        'HeartFail_preexist', 'ClassNYH_preexist_e', 'Hypertn_preexist',
        'InfEndo_preexist', 'HmO2_preexist_e', 'SlpApn_preexist',
        'LiverDis_preexist', 'Cancer_preexist', 'PVD_preexist',
        'CVD_preexist', 'CVA_preexist', 'PASYS', 'PASYS_missing',
        'HDEF', 'HDEF_missing',
        'CreatLst', 'CreatLst_missing', 'TotAlbumin', 'TotAlbumin_missing',
        'MELDScr', 'MELDScr_missing', 'PrCAB_prior', 'PrValve_prior', 
        'CathtoSurg_missing', 'CathtoSurg',
        'IABP_placed', 'PerfusTm', 'PerfusTm_missing', 
        'ORDuration', 'ORDuration_missing',
        'SIDuration', 'SIDuration_missing',
        'multiple_valve_1',
        'PredMort', 'PredMort_missing', 'PredDeep', 'PredDeep_missing',
        'PredReop', 'PredReop_missing', 'PredStro', 'PredStro_missing',
        'PredVent', 'PredVent_missing', 'PredRenF', 'PredRenF_missing',
        'PredMM', 'PredMM_missing', 'Pred6D', 'Pred6D_missing',
        'Pred14D', 'Pred14D_missing',
        'AV_repair', 'AV_replacement', 'MV_repair', 'MV_replacement',
        'T_repair_replacement', 'pulmonic_repair_replace',
        'bentall', 'av_sparing_root', 'non_v_conduit_Ross_homograft',
        'CABG', 'other_cardiac_surg', 'hosp_vol', 'surg_vol')

fVars = c('BSA_missing',  'HeightCm_missing', 
          'WeightKg_missing', 'BMI_missing', 
          'sex_male', 'race_white', 'race_black',
          'admission_ER', 'admission_elective', 'admission_transfer', 
          'admission_elective_others',
          'Diabetes_preexist', 'Dyslip_preexist', 'Dialysis_preexist',
          'HeartFail_preexist', 'ClassNYH_preexist_e', 'Hypertn_preexist',
          'InfEndo_preexist', 'HmO2_preexist_e', 'SlpApn_preexist',
          'LiverDis_preexist', 'Cancer_preexist', 'PVD_preexist',
          'CVD_preexist', 'CVA_preexist', 
          'PASYS_missing', 'HDEF_missing',
          'CreatLst_missing', 'TotAlbumin_missing',
          'MELDScr_missing', 'PrCAB_prior', 'PrValve_prior', 
          'CathtoSurg_missing', 'IABP_placed',  'PerfusTm_missing', 
          'ORDuration_missing', 'SIDuration_missing',
          'multiple_valve_1',
          'PredMort_missing', 'PredDeep_missing',
          'PredReop_missing',  'PredStro_missing',
          'PredVent_missing',  'PredRenF_missing',
          'PredMM_missing',  'Pred6D_missing', 'Pred14D_missing',
          'AV_repair', 'AV_replacement', 'MV_repair', 'MV_replacement',
          'T_repair_replacement', 'pulmonic_repair_replace',
          'bentall', 'av_sparing_root', 'non_v_conduit_Ross_homograft',
          'other_cardiac_surg', 'CABG')

library(tableone)
# All patients
tb = CreateTableOne(vars = vars, data = X, factorVars = fVars)
tb_primary = print(tb, contDigits = 4, catDigits = 2)

# TEE patients
tb_TEE = CreateTableOne(vars = vars, data = X[X$TEE_b == 1,], factorVars = fVars)
tb_TEE = print(tb_TEE, contDigits = 4, catDigits = 2)

# no-TEE patients
tb_no_TEE = CreateTableOne(vars = vars, data = X[X$TEE_b == 0,], factorVars = fVars)
tb_no_TEE = print(tb_no_TEE, contDigits = 4, catDigits = 2)

library(xtable)
xtable(cbind(tb_primary, tb_TEE, tb_no_TEE), digits = 4)









