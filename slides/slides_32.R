### R code from vignette source 'slides_32.Rnw'

###################################################
### code chunk number 1: slides_32.Rnw:32-35
###################################################
library(ggplot2)
library(gridExtra)
set.seed(60637)


###################################################
### code chunk number 2: slides_32.Rnw:174-181
###################################################
X <- c(0, 1, 2)
probs <- c(0.25, 0.5, 0.25)

sample(x = X,
       size = 1,
       prob = probs)



###################################################
### code chunk number 3: slides_32.Rnw:186-193
###################################################
n <- 1000
result_n <- sample(x = X,
                   size = n,
                   prob = probs,
                   replace = TRUE)

table(result_n)


###################################################
### code chunk number 4: slides_32.Rnw:199-202
###################################################

prop.table(table(result_n))



###################################################
### code chunk number 5: slides_32.Rnw:228-232
###################################################
ggplot(data.frame(result_n), aes(x = result_n)) +
  geom_histogram(bins = 3, color = 'white', fill = 'lightgreen') +
  theme_bw() + xlab('Number heads')



###################################################
### code chunk number 6: slides_32.Rnw:297-311
###################################################
plotdata <- data.frame(
  x = c(-1, 0, 1, 2),
  xend = c(0, 1, 2, 3),
  fx = c(0, 1/4, 1/2, 1/4),
  Fx = c(0, 1/4, 3/4, 1) # cumsum(fx)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 2.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0,0), xend = x, yend = fx)) +
  ggtitle('PMF of X as number of heads in 2 fair coin flips') +
  theme_bw()


###################################################
### code chunk number 7: slides_32.Rnw:383-390
###################################################
ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_segment(aes(x = x, y = Fx, xend = xend, yend = Fx)) +
  geom_point() +
  geom_point(aes(x = xend, y = Fx), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 2.5),
                  ylim = c(0,1)) +
  ggtitle('CDF of X as number of heads in 2 fair coin flips') + theme_bw()


###################################################
### code chunk number 8: slides_32.Rnw:417-424
###################################################

ggplot(data.frame(result_n), aes(x = result_n)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(-0.5, 2.5)) +
  ylab('Empirical Fx') +
  ggtitle('ECDF of X as number of heads in 2 fair coin flips') + theme_bw()



###################################################
### code chunk number 9: slides_32.Rnw:574-593
###################################################
Omega <- c('HH', 'HT', 'TH', 'TT')
probs <- c(0.25, 0.25, 0.25, 0.25)

result_n <- sample(x = Omega,
                   size = n,
                   prob = probs,
                   replace = TRUE)

result_mat <- data.frame(omega = result_n,
                         x = ifelse(result_n == 'TT', 0, 1),
                         y = ifelse(result_n == 'HH', 1, 0))

options <- list(theme(panel.grid.minor = element_blank()), scale_x_continuous(breaks = c(0, 1))) + # save some style options
  theme_bw()

p1 <- ggplot(result_mat) + geom_histogram(aes(x = x), bins = 3, position = 'identity', color = 'white') + options

p2 <- ggplot(result_mat) + geom_histogram(aes(x = y), bins = 3, position = 'identity', color = 'white') + options



###################################################
### code chunk number 10: slides_32.Rnw:613-614
###################################################
grid.arrange(p1, p2, ncol = 2)


###################################################
### code chunk number 11: slides_32.Rnw:687-708
###################################################
hist_top <- p1
empty <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

count_mat <- aggregate(
  list(count = result_mat$omega), 
  list(x = result_mat$x, y = result_mat$y), length)

scatter <- ggplot(result_mat, aes(x = x, y = y, color = omega)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0,1)) +
  theme(panel.grid.minor = element_blank(), legend.position = 'none')

hist_right <- p2 + coord_flip()

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 2), heights=c(2, 4))



###################################################
### code chunk number 12: slides_32.Rnw:879-892
###################################################
plotdata <- data.frame(
  x = c(0, 1),
  fx = c(1/2, 1/2)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = 0.5, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=0.5, y=0.75, label="E[X]") +
  ggtitle('PMF of X as number of heads in 1 fair coin flip')


###################################################
### code chunk number 13: slides_32.Rnw:1002-1017
###################################################

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = 0.5, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=0.5, y=0.75, label="E[X]", col = 'grey') +
  geom_segment(aes(x = 0.5, xend = 0.0, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0.5, xend = 1, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.58, label="Distance\nfrom mean") +
  annotate(geom="text", x=0.25, y=0.45, label="-0.5", color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.45, label="0.5", color = 'skyblue') +
  geom_point(aes(x = 0.5, y = 0.5), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 1 fair coin flip')


###################################################
### code chunk number 14: slides_32.Rnw:1044-1059
###################################################

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = 0.5, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=0.5, y=0.75, label="E[X]", col = 'grey') +
  geom_segment(aes(x = 0.5, xend = 0.25, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0.5, xend = 0.75, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.58, label="Squared distance\nfrom mean") +
  annotate(geom="text", x=0.25, y=0.45, label="0.25", color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.45, label="0.25", color = 'skyblue') +
  geom_point(aes(x = 0.5, y = 0.5), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 1 fair coin flip')


###################################################
### code chunk number 15: slides_32.Rnw:1088-1103
###################################################

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = 0.5, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=0.5, y=0.75, label="E[X]", col = 'grey') +
  geom_segment(aes(x = 0.5, xend = 0.0, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0.5, xend = 1, y = 0.5, yend = 0.5), arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.6, label="Square root of average\nsquared distance\nfrom mean") +
  annotate(geom="text", x=0.25, y=0.45, label="0.5", color = 'skyblue') +
  annotate(geom="text", x=0.75, y=0.45, label="0.5", color = 'skyblue') +
  geom_point(aes(x = 0.5, y = 0.5), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 1 fair coin flip')


###################################################
### code chunk number 16: slides_32.Rnw:1169-1187
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
### code chunk number 17: slides_32.Rnw:1231-1244
###################################################
ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.8, 2.8),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0), xend = x, yend = fx)) +
  geom_vline(xintercept = Ex, lty = 'dashed', color = 'skyblue') +
  annotate(geom="text", x=Ex, y=0.75, label="E[X]", color = 'grey') +
  geom_segment(aes(x = Ex, xend = x, y = fx, yend = fx),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = Ex, y = fx), color = 'skyblue') +
  annotate(geom="text", x=1.8, y=0.63, label="Distance\nfrom mean") +
  annotate(geom="text", x=(plotdata$x+Ex)/2, y=(plotdata$fx-0.05), label=(plotdata$x-Ex), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 18: slides_32.Rnw:1290-1304
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
  annotate(geom="text", x=1.8, y=0.63, label="Squared distance\nfrom mean") +
  annotate(geom="text", x=(plotdata$x+Ex)/2, y=(plotdata$fx-0.05), label=(plotdata$x-Ex)^2, color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 19: slides_32.Rnw:1347-1367
###################################################

sdx <- sqrt(sum((plotdata$x-Ex)^2 * plotdata$fx))

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
  annotate(geom="text", x=1.8, y=0.63, label="Square root of average\nsquared distance\nfrom mean") +
  annotate(geom="text", x=(Ex+c(-1.05,1.05)*round(sdx, 3)/2), y=0.45,
           label=round(sdx, 3), color = 'skyblue') +
  ggtitle('PMF of X as number of heads in 2 UNfair coin flips')


###################################################
### code chunk number 20: slides_32.Rnw:1531-1535
###################################################
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/IntroQSS-F21/main/data/carsonPDB.csv"
df_pdb <- read.csv(file, as.is = TRUE)

head(df_pdb)


###################################################
### code chunk number 21: slides_32.Rnw:1538-1541
###################################################
sapply(df_pdb[,c('Total_Pgs', 'maps', 'Redaction_total')], 
       function(x) c('mean' = mean(x, na.rm = TRUE),
                     'var' = var(x, na.rm = TRUE)))


###################################################
### code chunk number 22: slides_32.Rnw:1749-1750
###################################################
runif(n = 1, min = 0, max = 1)


###################################################
### code chunk number 23: slides_32.Rnw:1777-1782
###################################################
result_n <- runif(n, min = 0, max = 1)

ggplot(data.frame(result_n), aes(x = result_n)) +
  geom_histogram(breaks = seq(0, 1, length.out = 15),
                 position = 'identity', color = 'white')


###################################################
### code chunk number 24: slides_32.Rnw:1850-1862
###################################################


plotdata <- data.frame(
  x = c(-1, 0, 1, 2),
  Fx = c(0, 0, 1, 1)
)

ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('CDF of Standard Uniform Distribution')


###################################################
### code chunk number 25: slides_32.Rnw:1897-1911
###################################################
plotdata <- data.frame(
  x = c(-1, 0, 1, 1),
  xend = c(0, 1, 1, 2),
  fx = c(0, 1, 1, 0)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) +
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('PDF of Standard Uniform Distribution')


###################################################
### code chunk number 26: slides_32.Rnw:1940-1952
###################################################
datapoly <- data.frame(x = c(0, 0, 1, 1),
                       y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) +
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('PDF of Standard Uniform Distribution')



###################################################
### code chunk number 27: slides_32.Rnw:1981-1993
###################################################
datapoly <- data.frame(x = c(0, 0, 1, 1),
                       y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) +
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_polygon(data = datapoly, aes(x = x, y = y), fill = 'blue', alpha = 0.5) +
  ggtitle('PDF of Standard Uniform Distribution')


###################################################
### code chunk number 28: slides_32.Rnw:2028-2040
###################################################
datapoly <- data.frame(x = c(0, 0, 0.75, 0.75),
                       y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) +
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_polygon(data = datapoly, aes(x = x, y = y), fill = 'blue', alpha = 0.5) +
  ggtitle('PDF of Standard Uniform Distribution')


###################################################
### code chunk number 29: slides_32.Rnw:2078-2099
###################################################
result_n <- rnorm(n = 10000)
plotdata <- data.frame(
  x = result_n,
  Fx = pnorm(result_n),
  fx = dnorm(result_n)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,0.5)) +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') +
  geom_segment(aes(x = 0, xend = -1, y = 0.2, yend = 0.2),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0, xend = 1, y = 0.2, yend = 0.2),
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = 0, y = 0.2), color = 'skyblue') +
  annotate(geom="text", x = 0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'skyblue') +
  annotate(geom="text", x = -0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'skyblue') +
  annotate(geom="text", x = 0.075, y = .42, label = as.character(expression(mu)), parse = TRUE, color = 'skyblue') +
  ggtitle('PDF of Standard Normal Distribution')


###################################################
### code chunk number 30: slides_32.Rnw:2125-2131
###################################################
ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,1)) +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') +
  ggtitle('CDF of Standard Normal Distribution')


