# This file performs unadjusted analysis
dt = read.csv('<data file>')
dt_TEE = dt[dt$TEE_b == 1,]
dt_no_TEE = dt[dt$TEE_b == 0,]

# Chi-squared test without adjusting for observed covariates
# 30-day mortality
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_death = sum(dt_TEE$Mt30Stat_outcome)
num_no_TEE_death = sum(dt_no_TEE$Mt30Stat_outcome)

M = matrix(c(num_TEE_death, num_TEE - num_TEE_death, 
               num_no_TEE_death, num_no_TEE - num_no_TEE_death), 
             ncol = 2, byrow = T)
rownames(M) <- c('TEE', 'no TEE')
colnames(M) <- c('death', 'no death')

library(questionr)
odds.ratio(M)


# 30-day mortality or stroke
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_stroke_death = sum(dt_TEE$CNStrokP_Mt30Stat_outcome)
num_no_TEE_stroke_death = sum(dt_no_TEE$CNStrokP_Mt30Stat_outcome)

M2 = matrix(c(num_TEE_stroke_death, num_TEE - num_TEE_stroke_death, 
             num_no_TEE_stroke_death, num_no_TEE - num_no_TEE_stroke_death), 
           ncol = 2, byrow = T)
rownames(M2) <- c('TEE', 'no TEE')
colnames(M2) <- c('stroke/death', 'no stroke/death')

odds.ratio(M2)


# 30-day mortality or reop
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_reop_death = sum(dt_TEE$reoperation_complication | dt_TEE$Mt30Stat_outcome)
num_no_TEE_reop_death = sum(dt_no_TEE$reoperation_complication | dt_no_TEE$Mt30Stat_outcome)

M3 = matrix(c(num_TEE_reop_death, num_TEE - num_TEE_reop_death, 
              num_no_TEE_reop_death, num_no_TEE - num_no_TEE_reop_death), 
            ncol = 2, byrow = T)
rownames(M3) <- c('TEE', 'no TEE')
colnames(M3) <- c('reop/death', 'no reop/death')

odds.ratio(M3)


# Negative control outcome
dif_TEE = dt_TEE$post_creat - dt_TEE$CreatLst
dif_no_TEE = dt_no_TEE$post_creat - dt_no_TEE$CreatLst
t.test(dif_TEE, dif_no_TEE)


################################################################################
# Sensitivity Analysis
dt = dt[dt$TEE_missing == 0,]
dt_TEE = dt[dt$TEE_b == 1,]
dt_no_TEE = dt[dt$TEE_b == 0,]

# Chi-squared test without adjusting for observed covariates
# 30-day mortality
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_death = sum(dt_TEE$Mt30Stat_outcome)
num_no_TEE_death = sum(dt_no_TEE$Mt30Stat_outcome)

M = matrix(c(num_TEE_death, num_TEE - num_TEE_death, 
             num_no_TEE_death, num_no_TEE - num_no_TEE_death), 
           ncol = 2, byrow = T)
rownames(M) <- c('TEE', 'no TEE')
colnames(M) <- c('death', 'no death')

library(questionr)
odds.ratio(M)


# 30-day mortality or stroke
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_stroke_death = sum(dt_TEE$CNStrokP_Mt30Stat_outcome)
num_no_TEE_stroke_death = sum(dt_no_TEE$CNStrokP_Mt30Stat_outcome)

M2 = matrix(c(num_TEE_stroke_death, num_TEE - num_TEE_stroke_death, 
              num_no_TEE_stroke_death, num_no_TEE - num_no_TEE_stroke_death), 
            ncol = 2, byrow = T)
rownames(M2) <- c('TEE', 'no TEE')
colnames(M2) <- c('stroke/death', 'no stroke/death')

odds.ratio(M2)


# 30-day mortality or reop
num_TEE = dim(dt_TEE)[1]
num_no_TEE = dim(dt_no_TEE)[1]
num_TEE_reop_death = sum(dt_TEE$reoperation_complication | dt_TEE$Mt30Stat_outcome)
num_no_TEE_reop_death = sum(dt_no_TEE$reoperation_complication | dt_no_TEE$Mt30Stat_outcome)

M3 = matrix(c(num_TEE_reop_death, num_TEE - num_TEE_reop_death, 
              num_no_TEE_reop_death, num_no_TEE - num_no_TEE_reop_death), 
            ncol = 2, byrow = T)
rownames(M3) <- c('TEE', 'no TEE')
colnames(M3) <- c('reop/death', 'no reop/death')

odds.ratio(M3)


# Negative control outcome
dif_TEE = dt_TEE$post_creat - dt_TEE$CreatLst
dif_no_TEE = dt_no_TEE$post_creat - dt_no_TEE$CreatLst
t.test(dif_TEE, dif_no_TEE)


















