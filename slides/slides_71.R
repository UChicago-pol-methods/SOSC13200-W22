### R code from vignette source 'slides_71.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(estimatr)
library(modelsummary)


###################################################
### code chunk number 2: slides_71.Rnw:253-256
###################################################
dat <- read.csv('../data/banerjee-et-al.csv')

str(dat)


###################################################
### code chunk number 3: table2
###################################################
area_controls <- c('area_pop_base', 
                   'area_debt_total_base', 'area_business_total_base', 'area_exp_pc_mean_base', 'area_literate_head_base', 'area_literate_base')

controls <- paste(area_controls, collapse = ' + ')
outcomes <- c('spandana_1', 'othermfi_1', 'anymfi_1', 'anybank_1', 'anyinformal_1',  'anyloan_1', 'everlate_1', 'mfi_loan_cycles_1')

lm_list <- lm_list2 <- list()
for(out in outcomes){
  ff <- formula(paste0(out, ' ~ treatment +', controls))
  lm_list[[out]] <- lm_robust(ff, data = dat, clusters = areaid, weights = w1)
  lm_list2[[out]] <- mean(dat[which(dat$treatment ==0),out], na.rm = TRUE)
}

names(lm_list) <- c('Spandana', 'Other MFI', 'Any MFI', 'Other bank', 'Informal', 'Total', 'Ever Late', 'Num Cycles')
modelsummary(lm_list, stars = TRUE,
             coef_map = 'treatment', 
             add_rows = cbind(`Control mean` = 'Control mean', as.data.frame(lm_list2)), 
             # gof_omit = 'R2',
             output = '../assets/banerjee-et-al-table2.tex')


###################################################
### code chunk number 4: slides_71.Rnw:538-548
###################################################

formula1 <- formula(paste0('spandana_1 ~ treatment', 
                           '+ area_pop_base',
                           '+ area_debt_total_base',
                           '+ area_business_total_base',
                           '+ area_exp_pc_mean_base',
                           '+ area_literate_head_base',
                           '+ area_literate_base'))

lm_robust(formula1, data = dat, clusters = areaid, weights = w1)


###################################################
### code chunk number 5: slides_71.Rnw:590-595
###################################################
# number of observations
sum(!is.na(dat$spandana_1))

# Control mean
mean(dat[which(dat$treatment ==0),'spandana_1'], na.rm = TRUE)


###################################################
### code chunk number 6: slides_71.Rnw:620-624
###################################################

formula2 <- formula('spandana_1 ~ treatment')

lm_robust(formula2, data = dat, clusters = areaid, weights = w1)


###################################################
### code chunk number 7: table2alt
###################################################

lm_list_alt<- list()
for(out in outcomes){
  ff <- formula(paste0(out, ' ~ treatment'))
  lm_list_alt[[out]] <- lm_robust(ff, data = dat, clusters = areaid, weights = w1)
}

names(lm_list_alt) <- c('Spandana', 'Other MFI', 'Any MFI', 'Other bank', 'Informal', 'Total', 'Ever Late', 'Num Cycles')
modelsummary(lm_list_alt, stars = TRUE,
             coef_map = 'treatment', 
             add_rows = cbind(`Control mean` = 'Control mean', as.data.frame(lm_list2)), 
             # gof_omit = 'R2',
             output = '../assets/banerjee-et-al-table2alt.tex')


###################################################
### code chunk number 8: slides_71.Rnw:795-796
###################################################
hist(rnorm(10))


