### R code from vignette source 'slides_51.Rnw'

###################################################
### code chunk number 1: packages
###################################################
library(ri)
library(ggplot2)
set.seed(60637)


###################################################
### code chunk number 2: slides_51.Rnw:585-595
###################################################
df <- data.frame(
        # our initial treatment vector
        D = c(1, 0, 0, 0, 0, 0, 1),
        # our initial response vector
        Y = c(15, 15, 20, 20, 10, 15, 30),
        # treatment assignment probability
        probs = rep(2/7, 7)
)

df


###################################################
### code chunk number 3: slides_51.Rnw:619-623
###################################################
Y1 <- df$Y[which(df$D == 1)]
Y0 <- df$Y[which(df$D == 0)]

(dm_hat <- mean(Y1) - mean(Y0))


###################################################
### code chunk number 4: slides_51.Rnw:644-652
###################################################
df <-  cbind( # binds the columns together
        df,
        # Y(0) under the sharp null of no effect
        Y0 = df$Y,
        # Y(1) under the sharp null of no effect
        Y1 = df$Y)

df


###################################################
### code chunk number 5: slides_51.Rnw:674-675
###################################################
(perms <- genperms(df$D))


###################################################
### code chunk number 6: slides_51.Rnw:695-704
###################################################
Ys_null <- list(
        Y0 = df$Y0,
        Y1 = df$Y1
)

dm <- gendist(Ys_null,
              perms, 
              prob=df$probs)
dm


###################################################
### code chunk number 7: slides_51.Rnw:726-727
###################################################
mean(dm)


###################################################
### code chunk number 8: slides_51.Rnw:750-759
###################################################
gg_bins <- aggregate(list(rf = dm), by = list(dm = dm), length)
gg_bins$col <- abs(gg_bins$dm) >= dm_hat

ggplot(gg_bins, aes(x = dm, y = rf)) +
        geom_col() +
        geom_vline(xintercept = 0, color = 'red') +
        xlab('Difference in means estimates') + 
        ylab('Frequency') +
        ggtitle('Distribution of difference in means estimates under\nthe sharp null')


###################################################
### code chunk number 9: slides_51.Rnw:764-766
###################################################
prop.table(table(dm))
(pval <- mean(abs(dm) >= dm_hat))


###################################################
### code chunk number 10: slides_51.Rnw:789-797
###################################################
ggplot(gg_bins, aes(x = dm, y = rf, fill = col)) +
        geom_col() +
        geom_vline(xintercept = 0, color = 'red') + 
        scale_fill_manual(values=c("grey35", "#619CFF")) +
        theme(legend.position = 'none') +
        xlab('Difference in means estimates') + 
        ylab('Frequency') +
        ggtitle('Distribution of difference in means estimates under\nthe sharp null')


###################################################
### code chunk number 11: slides_51.Rnw:802-804
###################################################
prop.table(table(dm))
(pval <- mean(abs(dm) >= dm_hat))


###################################################
### code chunk number 12: slides_51.Rnw:849-852
###################################################
dispdist(distout = dm, 
         ate = dm_hat, 
         display.plot = FALSE)$two.tailed.p.value.abs


###################################################
### code chunk number 13: load
###################################################
df <- read.csv('../data/butler-broockman.csv', as.is = TRUE)
head(df)


###################################################
### code chunk number 14: summary
###################################################
str(df)


###################################################
### code chunk number 15: slides_51.Rnw:1130-1131
###################################################
table(df$treat_deshawn)


###################################################
### code chunk number 16: slides_51.Rnw:1139-1140
###################################################
table(df$reply_atall)


###################################################
### code chunk number 17: slides_51.Rnw:1162-1164
###################################################
df$D <- df$treat_deshawn
df$Y <- df$reply_atall


###################################################
### code chunk number 18: slides_51.Rnw:1172-1174
###################################################
Y1 <- df$Y[which(df$D == 1)]
Y0 <- df$Y[which(df$D == 0)]


###################################################
### code chunk number 19: slides_51.Rnw:1180-1181
###################################################
(dm_hat <- mean(Y1) - mean(Y0))


###################################################
### code chunk number 20: slides_51.Rnw:1225-1235
###################################################
# randomization inference function
my_ri <- function(df){
        df_ri <- df
        df_ri$newD <- sample(df$D)
        Y1_ri <- df$Y[which(df_ri$newD == 1)]
        Y0_ri <- df$Y[which(df_ri$newD == 0)]
        ate_hat <- mean(Y1_ri)-mean(Y0_ri)
  return(ate_hat)
}



###################################################
### code chunk number 21: slides_51.Rnw:1256-1257
###################################################
my_ri(df)


###################################################
### code chunk number 22: slides_51.Rnw:1262-1263
###################################################
my_ri(df)


###################################################
### code chunk number 23: slides_51.Rnw:1287-1293
###################################################
# number of iterations
n_iter <- 1000

# replicate does the same (random) function many times
dm <- replicate(n = n_iter, my_ri(df))
head(dm)


###################################################
### code chunk number 24: slides_51.Rnw:1317-1328
###################################################
null_dist <- data.frame(dm)
null_dist$bins <- cut(dm, breaks = 50)
null_dist <- aggregate(list(count = null_dist$dm), by = list(bins = null_dist$bins), length)
null_dist$bin_max <- as.numeric(gsub(".?(-?[0-9]+[.]+[0-9]+).*", "\\1", null_dist$bins))
null_dist$bin_min <- as.numeric(gsub(".*,(-?[0-9]+[.]+[0-9]+(e-[0-9]+)?)]$", "\\1", null_dist$bins))
null_dist$bin_mid <- (null_dist$bin_max - null_dist$bin_min)/2 + null_dist$bin_min
null_dist$col <- abs(null_dist$bin_min) >= abs(dm_hat)

ggplot(null_dist, aes(x = bin_mid, y = count)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red')


###################################################
### code chunk number 25: slides_51.Rnw:1335-1336
###################################################
(pval <- mean(abs(dm) >= abs(dm_hat)))


###################################################
### code chunk number 26: slides_51.Rnw:1360-1366
###################################################

ggplot(null_dist, aes(x = bin_mid, y = count, fill = col)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 27: slides_51.Rnw:1373-1374
###################################################
(pval <- mean(abs(dm) >= abs(dm_hat)))


###################################################
### code chunk number 28: slides_51.Rnw:1476-1483
###################################################
null_dist$col2 <- null_dist$bin_min <= dm_hat

ggplot(null_dist, aes(x = bin_mid, y = count)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 29: slides_51.Rnw:1490-1491
###################################################
(pval <- mean(dm <= dm_hat))


###################################################
### code chunk number 30: slides_51.Rnw:1515-1520
###################################################
ggplot(null_dist, aes(x = bin_mid, y = count, fill = col2)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 31: slides_51.Rnw:1526-1527
###################################################
(pval <- mean(dm <= dm_hat))


###################################################
### code chunk number 32: slides_51.Rnw:1577-1582
###################################################
ggplot(null_dist, aes(x = bin_mid, y = count, fill = col2)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 33: slides_51.Rnw:1615-1621
###################################################
null_dist$col3 <- null_dist$bin_min >= dm_hat
ggplot(null_dist, aes(x = bin_mid, y = count, fill = col3)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 34: slides_51.Rnw:1653-1658
###################################################
ggplot(null_dist, aes(x = bin_mid, y = count, fill = col)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


###################################################
### code chunk number 35: slides_51.Rnw:1698-1699
###################################################
hist(rnorm(10))


