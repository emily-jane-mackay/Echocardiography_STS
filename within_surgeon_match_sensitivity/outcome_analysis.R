# This file performs outcome analyses

setwd('<data file>')
treated_all = read.csv('./matched data/all_treated_data.csv')
control_all = read.csv('./matched data/all_control_data.csv')

library(exact2x2)

test_mcnemar <- function(treated_outcome, control_outcome){
  
  a = sum(treated_outcome == 1 & control_outcome == 1)
  b = sum(treated_outcome == 0 & control_outcome == 1)
  c = sum(treated_outcome == 1 & control_outcome == 0)
  d = sum(treated_outcome == 0 & control_outcome == 0)
  
  x = matrix(c(a,b,c,d), nrow = 2)
  return(list(x, mcnemar.exact(x)))
}

################################################################################
# Primary outcome: 30-day mortality
# Section 6.3.1 in eAppendix
################################################################################
treated_outcome = treated_all$death
control_outcome = control_all$death

summary(treated_outcome)
summary(control_outcome)
test_mcnemar(treated_outcome, control_outcome)

################################################################################
# Secondary outcome I: death or stroke
# Section 6.3.2 in eAppendix
################################################################################
treated_outcome = treated_all$stroke_comp
control_outcome = control_all$stroke_comp

summary(treated_outcome)
summary(control_outcome)
test_mcnemar(treated_outcome, control_outcome)


################################################################################
# Secondary outcome II: reoperation complication or death
# Section 6.3.3 in eAppendix
################################################################################
treated_outcome = (treated_all$reop_complication | treated_all$death)+0
control_outcome = (control_all$reop_complication | treated_all$death)+0

summary(treated_outcome)
summary(control_outcome)
test_mcnemar(treated_outcome, control_outcome)


################################################################################
# Negative control outcome: make two plots and a test
# eFigure 8 and Section 6.3.4 in eAppendix
################################################################################
################################################################################
library(ggplot2)
complete_id = complete.cases(treated_all$postcreat) & complete.cases(control_all$postcreat) & 
  (treated_all$CreatLst_missing ==0) & (control_all$CreatLst_missing ==0)
post_creat_t = treated_all$postcreat[complete_id]
post_creat_c = control_all$postcreat[complete_id]

pre_creat_t = treated_all$CreatLst[complete_id]
pre_creat_c = control_all$CreatLst[complete_id]


post_creat_df = data.frame(creat = c(pre_creat_t, pre_creat_c, post_creat_t, post_creat_c),
                           treated = c(rep('1. Pre-Surgery \n with TEE', length(pre_creat_t)), 
                                       rep('2. Pre-Surgery \n without TEE', length(pre_creat_c)),
                                       rep('3. Post-Surgery \n with TEE', length(post_creat_t)), 
                                       rep('4. Post-Surgery \n without TEE', length(post_creat_c))))
p = ggplot(data = post_creat_df, aes(x = treated, y = creat)) + geom_boxplot() +
  theme_bw(base_size = 25) + xlab('') + ylab('Creatinine Level') + ylim(c(0,15))


post_creat_df2 = data.frame(delta_creat = c(post_creat_t - pre_creat_t, 
                                            post_creat_c - pre_creat_c),
                            treated = c(rep('with TEE', length(pre_creat_t)), 
                                        rep('without TEE', length(pre_creat_c))))

p2 = ggplot(data = post_creat_df2, aes(x = treated, y = delta_creat)) + geom_boxplot() +
  theme_bw(base_size = 25) + xlab('') + ylab('Creatinine Level Elevation') + ylim(c(-10,10))


library(DOS)
delta_creat_t = post_creat_t - pre_creat_t
delta_creat_c = post_creat_c - pre_creat_c

dif = delta_creat_t - delta_creat_c
t.test(dif) # Test the weak null: sample ATE = 0