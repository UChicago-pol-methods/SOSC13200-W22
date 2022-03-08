### R code from vignette source 'slides_91.Rnw'

###################################################
### code chunk number 1: slides_91.Rnw:40-41
###################################################
options(scipen=1, digits=2)


###################################################
### code chunk number 2: packages
###################################################
set.seed(60637)
library(ggplot2)


###################################################
### code chunk number 3: slides_91.Rnw:229-238
###################################################
x <- runif(15, 1, 10)
y <- 3*x + rnorm(15, sd = 2)


ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    theme_bw()




###################################################
### code chunk number 4: slides_91.Rnw:263-269
###################################################
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw()




###################################################
### code chunk number 5: slides_91.Rnw:294-300
###################################################
ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    geom_line(color = '#3366FF', lwd = 1.1) +
    theme_bw()




###################################################
### code chunk number 6: slides_91.Rnw:437-442
###################################################

ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    theme_bw()



###################################################
### code chunk number 7: slides_91.Rnw:466-483
###################################################
minc <- function(c){
    sum((y[which(x <=c)] - mean(y[which(x <=c)]))^2) + 
        sum((y[which(x >c)] - mean(y[which(x >c)]))^2)
}

newmin <- x[which.min(sapply(x, minc))]+.1

ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_point() +
    theme_bw() +
    geom_vline(xintercept = newmin, color = 'orange') +
    annotate('text', y = 30, x = newmin-.2, label = 'c', color = 'orange') +
    annotate('text', y = c(20, 20), x = c(2.5, 7.5),
             label = paste0(expression(bar(Y)),'==',
                            round(c(mean(y[which(x <=newmin)]), mean(y[which(x >newmin)])), 3) ),
             color = 'orange', parse = TRUE)



###################################################
### code chunk number 8: slides_91.Rnw:643-644
###################################################
hist(rnorm(10))


