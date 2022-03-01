### R code from vignette source 'slides_52.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(ggplot2)
set.seed(60637)


###################################################
### code chunk number 2: slides_52.Rnw:143-161
###################################################
plotdata <- data.frame(
  x = c(0, 1, 2),
  xend = c(1, 2, 3),
  fx = c(1/16, 3/8, 9/16),
  Fx = cumsum(c(1/16, 3/8, 9/16))
)

# Expected value
Ex <- sum(plotdata$x*plotdata$fx)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.8, 2.8),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = 1.5, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=1.5, y=0.75, label="E[X]") +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 3: slides_52.Rnw:195-209
###################################################

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.8, 2.8),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = Ex, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=Ex, y=0.75, label="E[X]", color = 'grey') +
  geom_segment(aes(x = Ex, xend = Ex+sign(x-Ex)*(x-Ex)^2, y = fx, yend = fx),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = Ex, y = fx), color = 'skyblue') +
  annotate(geom="text", x=1.8, y=0.72, label="Squared distance\nfrom mean") +
  annotate(geom="text", x=(plotdata$x+Ex)/2, y=(plotdata$fx-0.05), label=(plotdata$x-Ex)^2, color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 4: slides_52.Rnw:243-263
###################################################

sdx <- sqrt(sum((plotdata$x-Ex)^2* plotdata$fx))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.8, 2.8),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = Ex, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=Ex, y=0.75, label="E[X]", color = 'grey') +
  geom_segment(aes(x = Ex, xend = Ex-sdx, y = 0.5, yend = 0.5),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = Ex, y = fx), color = 'skyblue') +
  geom_segment(aes(x = Ex, xend = Ex+sdx, y = 0.5, yend = 0.5),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = Ex, y = fx), color = 'skyblue') +
  annotate(geom="text", x=1.8, y=0.72, label="Square root of average\nsquared distance\nfrom mean") +
  annotate(geom="text", x=(Ex+c(-1.05,1.05)*round(sdx, 3)/2), y=0.45,
           label=round(sdx, 3), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 5: slides_52.Rnw:299-307
###################################################
n <- 1000
X <- c(0, 1, 2)
probs <- c(1/16, 3/8, 9/16)
x_observed <- sample(X, prob = probs,
                     replace = TRUE,
                     size = n)

head(x_observed)


###################################################
### code chunk number 6: slides_52.Rnw:312-313
###################################################
mean(x_observed)


###################################################
### code chunk number 7: slides_52.Rnw:317-319
###################################################
var(x_observed)
sd(x_observed)


###################################################
### code chunk number 8: slides_52.Rnw:367-375
###################################################
x_observed <- sample(X,
                     prob = probs,
                     replace = TRUE,
                     size = n)

mean(x_observed)
var(x_observed)
sd(x_observed)


###################################################
### code chunk number 9: slides_52.Rnw:565-572
###################################################
n <- 100
x_observed <- sample(X, 
                     prob = probs, 
                     replace = TRUE,
                     size = n)

head(x_observed)


###################################################
### code chunk number 10: slides_52.Rnw:604-605
###################################################
mean(x_observed)


###################################################
### code chunk number 11: slides_52.Rnw:635-646
###################################################

n_iter <- 10000

x_mat <- replicate(n_iter, sample(X,
                                 prob = probs,
                                 replace = TRUE,
                                 size = n))

dim(x_mat)
head(x_mat[,1])
head(x_mat[,2])


###################################################
### code chunk number 12: slides_52.Rnw:665-669
###################################################

sample_means <- apply(x_mat, 2, mean)
length(sample_means)
head(sample_means)


###################################################
### code chunk number 13: slides_52.Rnw:692-696
###################################################
ggplot(data.frame(sample_means = sample_means), aes(x = sample_means)) +
  geom_histogram(bins = 20, position = 'identity', color = 'white') +
  geom_vline(xintercept = Ex, color = 'grey', lty = 'dashed')



###################################################
### code chunk number 14: slides_52.Rnw:808-813
###################################################
sample_var <- apply(x_mat, 2, var)

ggplot(data.frame(sample_var = sample_var), aes(x = sample_var)) +
  geom_histogram(bins = 20, position = 'identity', color = 'white') +
  geom_vline(xintercept = sdx^2, color = 'grey', lty = 'dashed')


###################################################
### code chunk number 15: slides_52.Rnw:997-998
###################################################
head(x_observed)


###################################################
### code chunk number 16: slides_52.Rnw:1002-1003
###################################################
var(x_observed)


###################################################
### code chunk number 17: slides_52.Rnw:1007-1010
###################################################

sum( (x_observed - mean(x_observed) )^2)/(n-1)



###################################################
### code chunk number 18: slides_52.Rnw:1176-1177
###################################################
var(unlist(sample_means))


###################################################
### code chunk number 19: slides_52.Rnw:1266-1276
###################################################
n <- 500
x_mat <- replicate(8, sample(X,
                                 prob = probs,
                                 replace = TRUE,
                                 size = n))
new_mat <- apply(x_mat, 2, function(x) cumsum(x)/seq_along(x))
new_mat <- data.frame(`Sample mean` = matrix(new_mat, byrow = FALSE, ncol = 1),
                      n = 1:n,
                      name = factor(rep(1:8, each =  n)),
                      check.names = FALSE)


###################################################
### code chunk number 20: slides_52.Rnw:1303-1309
###################################################
ggplot(new_mat[which(new_mat$n < 26),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 21: slides_52.Rnw:1336-1343
###################################################

ggplot(new_mat[which(new_mat$n < 51),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 22: slides_52.Rnw:1369-1376
###################################################

ggplot(new_mat[which(new_mat$n < 101),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 23: slides_52.Rnw:1402-1409
###################################################

ggplot(new_mat[which(new_mat$n < 201),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 24: slides_52.Rnw:1435-1442
###################################################

ggplot(new_mat[which(new_mat$n < 351),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 25: slides_52.Rnw:1469-1476
###################################################

ggplot(new_mat[which(new_mat$n < 501),], aes(x = n, y = `Sample mean`, col = name)) +
  geom_hline(yintercept = Ex, color = 'darkgrey', lty = 'dashed') +
  geom_line() +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(0, 500))



###################################################
### code chunk number 26: slides_52.Rnw:2375-2376
###################################################
hist(rnorm(10))


