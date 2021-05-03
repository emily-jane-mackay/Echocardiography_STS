# This file contains a function that takes in a surgeon ID and 
# perform an optimal subset match
library(dplyr)
library(optmatch)
library(rcbsubset)
library(RItools)


match_one_surgeon <- function(ID){
  dir_input = paste('<data file>', 
                    ID, '.csv', sep = '')
  dt = read.csv(dir_input)
  
  # Rename covariates, e.g., sex_e = male/female to
  # sex_male = 1/0
  
  # Demographics
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
  dt$AP = (dt$bentall | dt$non_v_conduit_Ross_homograft | dt$av_sparing_root) + 0
  
  #Plus CABG 
  dt$CABG = dt$plus_CABG_b
  #Plus other cardiac surgery
  dt$other_cardiac_surg = dt$plus_other_cardiac_surg_b
  
 
  # Extract covariates
  X = dt %>%
    dplyr::select(Age, BSA, BSA_missing, BMI, BMI_missing, 
                  sex_male, race_white, race_black,
                  admission_ER, admission_transfer, admission_elective_others, 
                  Diabetes_preexist, Dyslip_preexist, Dialysis_preexist,
                  ClassNYH_preexist_e, Hypertn_preexist,
                  InfEndo_preexist, HmO2_preexist_e, SlpApn_preexist,
                  LiverDis_preexist, Cancer_preexist, PVD_preexist,
                  CVD_preexist, CVA_preexist, PASYS, PASYS_missing,
                  HDEF, HDEF_missing, HDEF_normal,
                  CreatLst, CreatLst_missing, TotAlbumin, TotAlbumin_missing,
                  MELDScr, MELDScr_missing, PrCAB_prior, PrValve_prior, 
                  CathtoSurg, CathtoSurg_missing,
                  IABP_placed, PerfusTm, PerfusTm_missing, 
                  ORDuration, ORDuration_missing,
                  SIDuration, SIDuration_missing,
                  multiple_valve_1, PredMort, PredMort_quartile,
                  AV_repair, AV_replacement, MV_repair, MV_replacement,
                  T_repair_replacement, pulmonic_repair_replace,
                  bentall, av_sparing_root, non_v_conduit_Ross_homograft,
                  AP, CABG, other_cardiac_surg, TEE_b, TEE_missing, HospID) %>%
    dplyr::mutate(num_comob = Dialysis_preexist + InfEndo_preexist + HmO2_preexist_e + 
             SlpApn_preexist + LiverDis_preexist + Cancer_preexist + 
             PVD_preexist + CVD_preexist + CVA_preexist)
  
  
  # I match exactly on heart failure, ejection fraction being normal, hospital, ClassNYH, 
  # heart failure, and surgical procedure
  
  exact.mask <- exactMatch(TEE_b ~ HospID + HDEF_normal +
                             ClassNYH_preexist_e + 
                             PredMort_quartile + AV_repair + AV_replacement + 
                             MV_repair + MV_replacement +
                             T_repair_replacement + pulmonic_repair_replace + AP +
                             CABG + other_cardiac_surg, data = X)
  dist.matrix <- match_on(TEE_b ~ Age + BSA + BSA_missing + BMI + BMI_missing +
                            sex_male + race_white + race_black + admission_ER +
                            admission_elective_others + admission_transfer +
                            Diabetes_preexist + Dyslip_preexist + Dialysis_preexist +
                            Hypertn_preexist + InfEndo_preexist + 
                            HmO2_preexist_e + SlpApn_preexist +
                            LiverDis_preexist + Cancer_preexist + PVD_preexist +
                            CVD_preexist + CVA_preexist +
                            PASYS + PASYS_missing +
                            HDEF + HDEF_missing +
                            CreatLst + CreatLst_missing + TotAlbumin + TotAlbumin_missing +
                            MELDScr + MELDScr_missing + PrCAB_prior + PrValve_prior +
                            CathtoSurg + CathtoSurg_missing +
                            IABP_placed + PerfusTm + PerfusTm_missing +
                            ORDuration + ORDuration_missing +
                            SIDuration + SIDuration_missing +
                            multiple_valve_1 + bentall + 
                            av_sparing_root + non_v_conduit_Ross_homograft,
                          within = exact.mask, data = X)

  match_mat = rcbsubset(dist.matrix, near.exact = c('race_black'),
                        fb.list = list('num_comob'),
                        treated.info = X[X$TEE_b == 1,],
                        control.info = X[X$TEE_b == 0,])
  
  # Treated and control before match
  X_treat = X[X$TEE_b == 1,]
  X_control = X[X$TEE_b == 0,]
  
  # Matched treated and control
  X_treat_match = X_treat[as.numeric(rownames(match_mat$matches)), ]
  X_control_match = X_control[match_mat$matches, ]
  X_treat_control = rbind(X_treat_match, X_control_match)
  
  # Look at the balance
  X_treat_control = rbind(X_treat_match, X_control_match)
 
  # Save the match and balance table
  dir_output = paste0('<data file>', ID)
  dir.create(dir_output)
  write.csv(X_treat_match, 
            paste(dir_output, '/treated.csv', sep = ''), 
            row.names = TRUE)
  write.csv(X_control_match, 
            paste(dir_output, '/control.csv', sep = ''), 
            row.names = TRUE)
  write.csv(dt, paste(dir_output, '/orig_data.csv', sep = ''), 
            row.names = TRUE)
}

# A trycatch version of the function
try_match_one_surgeon <- function(ID){
  out = tryCatch(
    {
      match_one_surgeon(ID)
    },
    error=function(cond) {
      message("Here's the original error message:")
      message(cond, '\n')
      return(NA)
    }
  )
}

