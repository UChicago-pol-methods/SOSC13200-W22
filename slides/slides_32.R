### R code from vignette source 'slides_32.Rnw'

###################################################
### code chunk number 1: slides_32.Rnw:31-34
###################################################
library(ggplot2)
library(gridExtra)
set.seed(60637)


###################################################
### code chunk number 2: slides_32.Rnw:173-180
###################################################
X <- c(0, 1, 2)
probs <- c(0.25, 0.5, 0.25)

sample(x = X,
       size = 1,
       prob = probs)



###################################################
### code chunk number 3: slides_32.Rnw:185-192
###################################################
n <- 1000
result_n <- sample(x = X,
                   size = n,
                   prob = probs,
                   replace = TRUE)

table(result_n)


###################################################
### code chunk number 4: slides_32.Rnw:198-201
###################################################

prop.table(table(result_n))



###################################################
### code chunk number 5: slides_32.Rnw:227-231
###################################################
ggplot(data.frame(result_n), aes(x = result_n)) +
  geom_histogram(bins = 3, color = 'white', fill = 'lightgreen') +
  theme_bw() + xlab('Number heads')



###################################################
### code chunk number 6: slides_32.Rnw:296-310
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
### code chunk number 7: slides_32.Rnw:382-389
###################################################
ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_segment(aes(x = x, y = Fx, xend = xend, yend = Fx)) +
  geom_point() +
  geom_point(aes(x = xend, y = Fx), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 2.5),
                  ylim = c(0,1)) +
  ggtitle('CDF of X as number of heads in 2 fair coin flips') + theme_bw()


###################################################
### code chunk number 8: slides_32.Rnw:416-423
###################################################

ggplot(data.frame(result_n), aes(x = result_n)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(-0.5, 2.5)) +
  ylab('Empirical Fx') +
  ggtitle('ECDF of X as number of heads in 2 fair coin flips') + theme_bw()



###################################################
### code chunk number 9: slides_32.Rnw:573-592
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
### code chunk number 10: slides_32.Rnw:612-613
###################################################
grid.arrange(p1, p2, ncol = 2)


###################################################
### code chunk number 11: slides_32.Rnw:686-707
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
### code chunk number 12: slides_32.Rnw:878-891
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
### code chunk number 13: slides_32.Rnw:1001-1016
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
### code chunk number 14: slides_32.Rnw:1043-1058
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
### code chunk number 15: slides_32.Rnw:1087-1102
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
### code chunk number 16: slides_32.Rnw:1165-1183
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
### code chunk number 17: slides_32.Rnw:1220-1234
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


