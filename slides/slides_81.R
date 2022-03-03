### R code from vignette source 'slides_81.Rnw'

###################################################
### code chunk number 1: slides_81.Rnw:34-35
###################################################
 options(scipen=1, digits=2)


###################################################
### code chunk number 2: packages
###################################################
library(ggplot2)
library(estimatr)
library(gridExtra)
set.seed(60637)


###################################################
### code chunk number 3: slides_81.Rnw:197-209
###################################################
n <- 50000

x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.05) +
  geom_abline(slope = 2, intercept = 3, color = 'turquoise', lwd = 1.5) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,7)) +
  theme_bw()



###################################################
### code chunk number 4: slides_81.Rnw:241-267
###################################################
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(X), 
                   list(a = format(unname(coef(m)[1]), digits = 3),
                        b = format(unname(coef(m)[2]), digits = 3)))
  as.character(as.expression(eq));
}

n <- 100

x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0a <- lm(y~x)
lm0laba <- lm_eqn(df)

g <- ggplot(df) +
  coord_cartesian(xlim = c(0,1), ylim = c(1,7)) +
  geom_abline(slope = 2, intercept = 3, color = 'turquoise', lwd = 1.5, alpha = 0.5) +
  theme_bw()

g <- g + geom_abline(intercept = coef(lm0a)[1], slope = coef(lm0a)[2], color = 'blue') +
  geom_text(aes(x = 0.25, y = 6.25, label = lm0laba), parse = TRUE, data.frame(), color = 'blue')
g + geom_point(aes(x = df$x, y = df$y), alpha = 0.5)



###################################################
### code chunk number 5: slides_81.Rnw:300-311
###################################################
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0b <- lm(y~x)
lm0labb <- lm_eqn(df)

g <- g + geom_abline(intercept = coef(lm0b)[1], slope = coef(lm0b)[2], color = 'darkgreen') +
  geom_text(aes(x = 0.25, y = 6, label = lm0labb), parse = TRUE, data.frame(), color = 'darkgreen')
g + geom_point(aes(x = df$x, y = df$y), alpha = 0.5)



###################################################
### code chunk number 6: slides_81.Rnw:344-356
###################################################
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0c <- lm(y~x)
lm0labc <- lm_eqn(df)


g <- g + geom_abline(intercept = coef(lm0c)[1], slope = coef(lm0c)[2], color = 'orange') +
  geom_text(aes(x = 0.25, y = 5.75, label = lm0labc), parse = TRUE, data.frame(), color = 'orange')
g + geom_point(aes(x = df$x, y = df$y), alpha = 0.5)



###################################################
### code chunk number 7: slides_81.Rnw:389-402
###################################################
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0d <- lm(y~x)
lm0labd <- lm_eqn(df)



g <- g + geom_abline(intercept = coef(lm0d)[1], slope = coef(lm0d)[2], color = 'pink') +
  geom_text(aes(x = 0.25, y = 5.5, label = lm0labd), parse = TRUE, data.frame(), color = 'pink')
g + geom_point(aes(x = df$x, y = df$y), alpha = 0.5)



###################################################
### code chunk number 8: slides_81.Rnw:435-445
###################################################
x <- sample(seq(0,1, .05), n, replace = TRUE)
y <- 3 + 2*x + rnorm(n)
df <- data.frame(x = x,y = y)

lm0e <- lm(y~x)
lm0labe <- lm_eqn(df)

g <- g + geom_abline(intercept = coef(lm0e)[1], slope = coef(lm0e)[2], color = 'purple') +
  geom_text(aes(x = 0.25, y = 5.25, label = lm0labe), parse = TRUE, data.frame(), color = 'purple')
g + geom_point(aes(x = df$x, y = df$y), alpha = 0.5)


###################################################
### code chunk number 9: slides_81.Rnw:515-534
###################################################
X <- runif(1000)
Y <- 3*X + rnorm(1000, sd = 0.6)
Y2 <- 3*X + rnorm(1000)*X


df <- data.frame(X, Y, Y2)

g1 <- ggplot(df, aes(x = X, y = Y)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(ylim = c(-0.25, 5.25)) + 
  ggtitle('Homoskedastic data') + theme_bw()

g2 <- ggplot(df, aes(x = X, y = Y2)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(ylim = c(-0.25, 5.25)) +
  ggtitle('Heteroskedastic data') + theme_bw()


grid.arrange(g1, g2, ncol=2)


###################################################
### code chunk number 10: slides_81.Rnw:580-597
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
### code chunk number 11: slides_81.Rnw:627-628
###################################################
model2 <- lm_robust(call_back ~ black*record, data = dfp)


###################################################
### code chunk number 12: slides_81.Rnw:652-653
###################################################
summary(model2)


###################################################
### code chunk number 13: slides_81.Rnw:741-742
###################################################
confint(model2)


###################################################
### code chunk number 14: slides_81.Rnw:933-935
###################################################
summary(model2)
round(model2$p.value,5)


###################################################
### code chunk number 15: slides_81.Rnw:1050-1062
###################################################
outmat <- replicate(1000, # do this 1000 times
                    {
                      # Take a sample of size n with replacemetn from the data
                      idx <- sample(1:nrow(dfp), replace = TRUE) 
                      # fit the model on the sampled data
                      lmx <- lm_robust(call_back ~ black*record, 
                                       data = dfp[idx,])
                      coef(lmx)
                    })
outmat <- t(outmat)
dim(outmat)
head(outmat, 4)


###################################################
### code chunk number 16: slides_81.Rnw:1085-1086
###################################################
apply(outmat, 2, sd)


###################################################
### code chunk number 17: slides_81.Rnw:1093-1094
###################################################
model2$std.error


###################################################
### code chunk number 18: slides_81.Rnw:1123-1124
###################################################
t(apply(outmat, 2, quantile, probs = c(0.025, 0.075)))


###################################################
### code chunk number 19: slides_81.Rnw:1131-1132
###################################################
confint(model2)


###################################################
### code chunk number 20: slides_81.Rnw:1194-1195
###################################################
hist(rnorm(10))


