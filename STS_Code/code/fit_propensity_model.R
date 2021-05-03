# This file takes in STS_data_relevant_rows.csv
# and fits a logistic regression, estimates the 
# propensity score for each unit, and outputs
# a dataset with fitted propensity score.


library(data.table)
dt = fread('<data file>')

library(dplyr)
dt = dt%>%
  group_by(SurgID) %>%
  mutate(surg_vol = n()) %>%
  ungroup()

dt = dt%>%
  group_by(HospID) %>%
  mutate(hosp_vol = n()) %>%
  ungroup()

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

# Predicted risk
dt$PredMort_missing = is.na(dt$PredMort) + 0
dt$PredMort[is.na(dt$PredMort)] = mean(dt$PredMort, na.rm = TRUE)


library(dplyr)
X = dt %>%
  dplyr::select(Age, BSA, BSA_missing, BMI, BMI_missing, 
                sex_male, race_white, race_black,
                admission_ER, admission_transfer, 
                Diabetes_preexist, Dyslip_preexist, Dialysis_preexist,
                ClassNYH_preexist_e, Hypertn_preexist,
                InfEndo_preexist, HmO2_preexist_e, SlpApn_preexist,
                LiverDis_preexist, Cancer_preexist, PVD_preexist,
                CVD_preexist, CVA_preexist, PASYS, PASYS_missing,
                HDEF, HDEF_missing, HDEF_normal,
                CreatLst, CreatLst_missing, TotAlbumin, TotAlbumin_missing,
                MELDScr, MELDScr_missing, PrCAB_prior, PrValve_prior, 
                IABP_placed, CathtoSurg_missing, CathtoSurg,
                PerfusTm, PerfusTm_missing, 
                ORDuration, ORDuration_missing,
                SIDuration, SIDuration_missing,
                multiple_valve_1, 
                PredMort, PredMort_missing,
                AV_repair, AV_replacement, MV_repair, MV_replacement,
                T_repair_replacement, pulmonic_repair_replace,
                bentall, av_sparing_root, non_v_conduit_Ross_homograft,
                CABG, other_cardiac_surg,
                surg_vol, hosp_vol)

X$ClassNYH_preexist_e = as.factor(X$ClassNYH_preexist_e)
X.mat = model.matrix(~., data = X)
Y = dt$TEE_b

# Fit a propensity score model
library(glmnet)
lambdas <- 10^seq(3, -3, by = -0.5)
ridge_glm = cv.glmnet(X.mat, Y, family = 'binomial', alpha = 0, lambda = lambdas)
opt_lambda <- ridge_glm$lambda.min
pscore <- predict(ridge_glm, s = opt_lambda, newx = X.mat, type = 'response')

output_data = data.frame(X, dt$PredMort_quartile, 
                         pscore, 
                         TEE = Y,
                         dt$Mt30Stat_outcome, dt$CNStrokP_outcome,
                         dt$CNStrokP_Mt30Stat_outcome,
                         dt$reoperation_complication,
                         dt$post_creat)

write.csv(output_data, '<data file>', 
          row.names = FALSE)

