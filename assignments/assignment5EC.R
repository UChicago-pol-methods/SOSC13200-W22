library(ri)

df <- data.frame(
  # our initial treatment vector
  D = c(1, 0, 0, 0, 0, 0, 1),
  # our initial response vector
  Y = c(15, 15, 20, 20, 10, 15, 30),
  # treatment assignment probability
  probs = rep(2/7, 7)
)

# create potential outcomes under null
df$Y0 <- df$Y1 <- df$Y

Ys_null <- list(
  Y0 = df$Y0,
  Y1 = df$Y1
)



treat_mat <- combn(length(df$D), sum(df$D))
perms0 <- matrix(0, nrow = nrow(df), ncol = ncol(treat_mat))

for(i in 1:ncol(perms0)){
  
  id1 <- treat_mat[,i]
  perms0[id1, i] <- 1
  
}

perms0


numiter <- ncol(perms0)
distout <- rep(NA, numiter)

for (iter in 1:numiter) {
  Zri <- perms0[, iter]
  Yri <- Ys_null$Y0
  Yri[which(Zri == 1)] <- Ys_null$Y1[Zri == 1]
  distout[iter] <- mean(Yri[which(Zri == 1)]) - mean(Yri[which(Zri == 0)])
}
hist(distout)


# package based solution

(perms <- genperms(df$D))
# dm <- gendist(Ys_null,
#               perms, 
#               prob=df$probs)
# dm

hist(dm)