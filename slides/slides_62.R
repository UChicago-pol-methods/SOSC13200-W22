### R code from vignette source 'slides_62.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(ggplot2)
set.seed(60637)


###################################################
### code chunk number 2: slides_62.Rnw:208-232
###################################################
result_n <- rnorm(n = 10000)
plotdata <- data.frame(
  x = result_n,
  Fx = pnorm(result_n),
  fx = dnorm(result_n)
)

g <- ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,0.5)) +
  ggtitle('PDF of Standard Normal Distribution')

g +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') + 
  geom_segment(aes(x = 0, xend = -1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0, xend = 1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = 0, y = 0.2), color = 'skyblue') + 
  annotate(geom="text", x = 0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = -0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = 0.075, y = .42, label = as.character(expression(theta)), parse = TRUE, color = 'steelblue') + 
  theme_bw()


###################################################
### code chunk number 3: slides_62.Rnw:259-269
###################################################
g +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "skyblue",
                xlim = c(-1.96, 1.96)) +
  geom_vline(xintercept = 1.96, lty = 'dashed', color = 'skyblue') + 
  geom_vline(xintercept = -1.96, lty = 'dashed', color = 'skyblue') +  
  annotate(geom="text", x = -1.96, y = .2, label = round(-1.96, 3), parse = TRUE, color = 'steelblue') +  
  annotate(geom="text", x = 1.96, y = .2, label = round(1.96, 3), parse = TRUE, color = 'steelblue') + 
  theme_bw()


###################################################
### code chunk number 4: slides_62.Rnw:295-298
###################################################
X <- c(0, 1, 2)
fx <- c(1/16, 3/8, 9/16)
(Ex <- sum(X*fx))


###################################################
### code chunk number 5: slides_62.Rnw:306-310
###################################################
n <- 100
x_observed <- sample(X, prob = fx, replace = TRUE, size = n)

head(x_observed)


###################################################
### code chunk number 6: slides_62.Rnw:330-332
###################################################
(theta_hat <- mean(x_observed))
(se_hat <- sd(x_observed)/sqrt(length(x_observed)))


###################################################
### code chunk number 7: slides_62.Rnw:339-340
###################################################
(CI95 <- c(theta_hat + c(-1,1)*1.96*se_hat))


###################################################
### code chunk number 8: slides_62.Rnw:361-373
###################################################
ggplot(data.frame(conf_lower = CI95[1], conf_upper = CI95[2], mean = theta_hat),
       aes(y = 1, x = mean)) +
  geom_point(color = 'skyblue') +
  geom_linerange(aes(xmin=conf_lower,xmax=conf_upper), color = 'skyblue', alpha = 0.85) +
  coord_cartesian(xlim = c(1.25, 1.75),) +
  geom_vline(xintercept = Ex, color = 'black', lty = 'dashed') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle('95% Normal Approximation-Based CI,\n2 Weighted Coin Flips, Sample Size = 100') +
  theme_bw()



###################################################
### code chunk number 9: slides_62.Rnw:397-400
###################################################
n_iter <- 50
x_mat <- replicate(n_iter, sample(X, prob = fx, replace = TRUE,
                                  size = n))


###################################################
### code chunk number 10: slides_62.Rnw:405-415
###################################################
CI_95f <- function(x){
  theta_hat <- mean(x)
  se_hat <- sd(x)/sqrt(length(x_observed))
  CI_hat <- theta_hat +
    c('conf_lower' = -1, 'conf_upper' = 1)*1.96*se_hat
}

sample_CIs <- as.data.frame(t(apply(x_mat, 2, CI_95f)))

head(sample_CIs, 3)


###################################################
### code chunk number 11: slides_62.Rnw:435-445
###################################################
ggplot(sample_CIs,
       aes(y = seq(from = 0, to = 2, length.out = n_iter), x = 1)) +
  geom_linerange(aes(xmin=conf_lower,xmax=conf_upper), color = 'skyblue', alpha = 0.85) +
  coord_cartesian(xlim = c(1.25, 1.75)) +
  geom_vline(xintercept = Ex, color = 'black', lty = 'dashed') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle('95% Normal Approximation-Based CI,\n2 Weighted Coin Flips, Sample Size = 100') + 
  theme_bw()


###################################################
### code chunk number 12: slides_62.Rnw:472-474
###################################################
mean( (Ex >= sample_CIs$conf_lower) & 
        (Ex <= sample_CIs$conf_upper) )


###################################################
### code chunk number 13: slides_62.Rnw:496-501
###################################################
x_mat <- replicate(5000, sample(X, 
                                prob = fx, 
                                replace = TRUE,
                                size = n))
CI_n <- as.data.frame(t(apply(x_mat, 2, CI_95f)))


###################################################
### code chunk number 14: slides_62.Rnw:506-507
###################################################
mean( (Ex >= CI_n$conf_lower) & (Ex <= CI_n$conf_upper) )


###################################################
### code chunk number 15: slides_62.Rnw:572-589
###################################################
dfp <- data.frame(
  black = rep(c(0, 1), times = c(300, 400)),
  record = c(rep(c(0, 1), each = 150),
             rep(c(0, 1), each = 200)),
  call_back = c(
    # whites without criminal records
    rep(c(0, 1), times = c(99, 51)), # 150
    # whites with criminal records
    rep(c(0, 1), times = c(125, 25)), # 150; 
    # - callbacks could be 25 or 26
    # blacks without criminal records
    rep(c(0, 1), times = c(172, 28)), # 200
    # blacks with criminal records
    rep(c(0, 1), times = c(190, 10)) # 200
  )
)



###################################################
### code chunk number 16: slides_62.Rnw:609-618
###################################################
pager_agg <- aggregate(call_back~black + record, data = dfp, mean)
pager_agg$race <- factor(pager_agg$black,
                         levels = c(1, 0),
                         labels = c('Black', 'White'))
pager_agg$criminal_record <- factor(pager_agg$record,
                                    levels = c(1, 0),
                                    labels = c('Record', 'No Record'))

pager_agg


###################################################
### code chunk number 17: slides_62.Rnw:643-644
###################################################
(theta_hat <- mean(dfp$call_back[which(dfp$black == 1)]) )


###################################################
### code chunk number 18: slides_62.Rnw:655-659
###################################################
(se_hat <- sqrt(var(dfp$call_back[which(dfp$black == 1)])/
                  length(which(dfp$black == 1))))




###################################################
### code chunk number 19: slides_62.Rnw:685-686
###################################################
(CI <- c(theta_hat + c(-1,1)*1.96*se_hat))


###################################################
### code chunk number 20: slides_62.Rnw:712-714
###################################################
model <- lm(call_back ~1,
            data = dfp[which(dfp$black == 1),])


###################################################
### code chunk number 21: slides_62.Rnw:733-735
###################################################

summary(model)


###################################################
### code chunk number 22: slides_62.Rnw:754-756
###################################################

confint.default(model)


###################################################
### code chunk number 23: slides_62.Rnw:847-848
###################################################
model2 <- lm(call_back ~ black*record, data = dfp)


###################################################
### code chunk number 24: slides_62.Rnw:872-873
###################################################
summary(model2)


###################################################
### code chunk number 25: slides_62.Rnw:897-899
###################################################

confint.default(model2)


###################################################
### code chunk number 26: slides_62.Rnw:936-937
###################################################
hist(rnorm(10))


