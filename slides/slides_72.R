### R code from vignette source 'slides_72.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(ggplot2)
library(estimatr)
library(modelsummary)


###################################################
### code chunk number 2: slides_72.Rnw:90-94
###################################################
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/SOSC13200-W22/main/data/card-krueger.csv"
dat <- read.csv(file, as.is = TRUE)

head(dat)


###################################################
### code chunk number 3: slides_72.Rnw:115-116
###################################################
str(dat)


###################################################
### code chunk number 4: slides_72.Rnw:291-322
###################################################
dat_wide <- reshape(dat, direction = 'wide', idvar = 'id', 
                    v.names = c('fte', 'ft', 'pt', 'mgrs', 'wage', 'meal', 'hrsopen', 'bonus',
                                'ncalls','inctime','firstinc','nregs'),
                    drop = c('d_nj', 'Wave'),
                    timevar = 'd')
dat_wide$Y <- dat_wide$fte.1-dat_wide$fte.0

# discuss coding of gap
dat_wide$gap <- ifelse(dat_wide$nj==1 & dat_wide$wage.0<= 5.05,((5.05-dat_wide$wage.0)/dat_wide$wage.0),0)

# conditioning variables, based on table footnote
dat_wide <- dat_wide[which( !is.na(dat_wide$Y) &
                              !is.na(dat_wide$wage.0) &
                              !is.na(dat_wide$wage.1)),]

lm1 <- lm_robust(Y ~ nj, data = dat_wide)
lm2 <- lm_robust(Y ~ nj + kfc + roys + wendys + co_owned, data = dat_wide)
lm3 <- lm_robust(Y ~ gap, data = dat_wide)
lm4 <- lm_robust(Y ~ gap + kfc + roys + wendys + co_owned, data = dat_wide)
lm5 <- lm_robust(Y ~ gap + kfc + roys + wendys + co_owned + centralj + southj + pa1 + pa2, data = dat_wide)


modelsummary(list(lm1, lm2, lm3, lm4, lm5), stars = TRUE,
             # coef_map = c('nj', 'gap'), 
             coef_rename = c(nj = 'New Jersey Dummy', gap = 'Initial Wage Gap'),
             coef_omit = 'Int|kfc|wendy|co|roys|central|south|pa',
             add_rows = as.data.frame(rbind(c('Controls for chain and ownership', 'no', 'yes', 'no', 'yes', 'yes'),
                              c('Controls for region', 'no', 'no', 'no', 'no', 'yes'))), 
             gof_omit = 'Std|R2',
             output = '../assets/card-krueger-table4.tex')



###################################################
### code chunk number 5: slides_72.Rnw:370-371
###################################################
hist(rnorm(10))


