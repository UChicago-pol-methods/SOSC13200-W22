### R code from vignette source 'slides_61.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(ggplot2)


###################################################
### code chunk number 2: slides_61.Rnw:105-119
###################################################
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/SOSC13200-W22/main/data/angrist-krueger.csv"
dat <- read.csv(file, as.is = TRUE)

dat$year_of_birth_adj <- dat$year_of_birth + 
  0.25 * (dat$quarter_of_birth-1)

states_above_16 <- c(15, 23, 32, 35, 39, 40, 41, 42, 48, 49, 51, 53)
dat$states_above_16 <- 1 * (dat$place_of_birth %in% states_above_16)

dat_agg <- aggregate(x = dat[, c('log_weekly_wage', 'education')], 
                     by = list(`year_of_birth_adj` = dat$year_of_birth_adj,
                               `quarter_of_birth` = dat$quarter_of_birth,
                               `above` = as.factor(dat$states_above_16)),
                     FUN = mean)


###################################################
### code chunk number 3: slides_61.Rnw:141-142
###################################################
head(dat_agg)


###################################################
### code chunk number 4: slides_61.Rnw:167-177
###################################################
ggplot(dat_agg, aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  # geom_smooth(method = 'lm') + # lines
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 5: slides_61.Rnw:223-233
###################################################
ggplot(dat_agg[which(dat_agg$above == 0),], aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  # geom_smooth(method = 'lm', se = FALSE) + # lines
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 6: slides_61.Rnw:258-268
###################################################
ggplot(dat_agg[which(dat_agg$above == 0),], aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  geom_smooth(method = 'lm', se = FALSE) + # lines
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 7: slides_61.Rnw:293-304
###################################################
ggplot(dat_agg[which(dat_agg$above == 1),], aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  # geom_smooth(method = 'lm', se = FALSE) + # lines
  scale_color_manual(values = '#00BFC4') +
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 8: slides_61.Rnw:329-340
###################################################
ggplot(dat_agg[which(dat_agg$above == 1),], aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  geom_smooth(method = 'lm', se = FALSE) + # lines
  scale_color_manual(values = '#00BFC4') +
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 9: slides_61.Rnw:365-375
###################################################
ggplot(dat_agg, aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  geom_smooth(method = 'lm', se = FALSE) + # lines
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 10: slides_61.Rnw:488-491
###################################################
toy_dat <- data.frame(Y = c(2, 3, 4), 
                      X = c(5, 10, 10))
toy_dat


###################################################
### code chunk number 11: slides_61.Rnw:515-519
###################################################
ggplot(toy_dat, aes(x = X,
                    y = Y)) +
  geom_point() + # points with color
  theme_bw() # plot style


###################################################
### code chunk number 12: slides_61.Rnw:545-550
###################################################
ggplot(toy_dat, aes(x = X,
                    y = Y)) +
  geom_point() + # points with color
  theme_bw() + # plot style
  geom_smooth(method = 'lm', se = FALSE) # lines


###################################################
### code chunk number 13: slides_61.Rnw:575-585
###################################################
toy_dat$fitted <- fitted(lm(Y~X, data = toy_dat))

ggplot(toy_dat, aes(x = X,
                    y = Y)) +
  geom_point() + # points with color
  theme_bw() + # plot style
  geom_smooth(method = 'lm', se = FALSE) + # lines
  geom_segment(aes(x = X, y = Y,
                   xend = X, yend = fitted), 
               color = 'grey')


###################################################
### code chunk number 14: load
###################################################
lm(Y~X, data = toy_dat)


###################################################
### code chunk number 15: slides_61.Rnw:633-640
###################################################
ggplot(toy_dat, aes(x = X,
                    y = Y)) +
  geom_point() + # points with color
  theme_bw() + # plot style
  xlim(0, NA) +
  ylim(0, NA) +
  stat_smooth(method="lm", fullrange=TRUE, se = FALSE) # lines


###################################################
### code chunk number 16: slides_61.Rnw:665-684
###################################################
lm0 <- lm(log_weekly_wage ~ education, data = dat_agg[which(dat_agg$above == 0),])
lm1 <- lm(log_weekly_wage ~ education, data = dat_agg[which(dat_agg$above == 1),])
dat_agg$fitted <- NA
dat_agg$fitted[which(dat_agg$above == 0)] <- fitted(lm0)
dat_agg$fitted[which(dat_agg$above == 1)] <- fitted(lm1)

ggplot(dat_agg, aes(x = education,
                    y = log_weekly_wage,
                    color = above)) +
  geom_point() + # points with color
  geom_smooth(method = 'lm', se = FALSE) + # lines
  theme_bw() + # plot style
  ylab('Log Weekly Earnings') +  # y-axis label
  xlab('Years of Completed Education') + # x-axis label
  ggtitle('Angrist and Krueger, Earnings on Education\nby Age of Compulsory Schooling') + 
  geom_segment(aes(x = education, y = log_weekly_wage,
                   xend = education, yend = fitted, color = above),
               alpha = 0.25) + 
  coord_cartesian(xlim = c(12.2, 13.4), ylim = c(5.88, 5.94))


###################################################
### code chunk number 17: load
###################################################
df <- read.csv('../data/butler-broockman.csv', as.is = TRUE)
head(df)


###################################################
### code chunk number 18: slides_61.Rnw:832-833
###################################################
table(df$treat_deshawn)


###################################################
### code chunk number 19: slides_61.Rnw:841-842
###################################################
table(df$reply_atall)


###################################################
### code chunk number 20: slides_61.Rnw:864-866
###################################################
df$D <- df$treat_deshawn
df$Y <- df$reply_atall


###################################################
### code chunk number 21: slides_61.Rnw:874-876
###################################################
Y1 <- df$Y[which(df$D == 1)]
Y0 <- df$Y[which(df$D == 0)]


###################################################
### code chunk number 22: slides_61.Rnw:882-883
###################################################
(dm_hat <- mean(Y1) - mean(Y0))


###################################################
### code chunk number 23: slides_61.Rnw:907-908
###################################################
lm(Y~D, data = df)


###################################################
### code chunk number 24: slides_61.Rnw:977-978
###################################################
lm(reply_atall ~ leg_party, data = df)


###################################################
### code chunk number 25: slides_61.Rnw:982-983
###################################################
lm(reply_atall ~ leg_party - 1, data = df)


###################################################
### code chunk number 26: slides_61.Rnw:1005-1012
###################################################
ggplot(df, aes(x = leg_republican,
                    y = Y)) +
  geom_point() + # points with color
  theme_bw() + # plot style
  xlim(0, NA) +
  ylim(0, NA) +
  stat_smooth(method="lm", fullrange=TRUE, se = FALSE) # lines


###################################################
### code chunk number 27: slides_61.Rnw:1034-1037
###################################################
table(df$Y[which(df$leg_republican == 0)])

mean(df$Y[which(df$leg_republican == 0)])


###################################################
### code chunk number 28: slides_61.Rnw:1040-1044
###################################################
table(df$Y[which(df$leg_republican == 1)])

mean(df$Y[which(df$leg_republican == 1)])



###################################################
### code chunk number 29: slides_61.Rnw:1066-1069
###################################################
lm1 <- lm(Y ~ D, data = df)
names(lm1)
summary(lm1)


###################################################
### code chunk number 30: slides_61.Rnw:1111-1112
###################################################
hist(rnorm(10))


