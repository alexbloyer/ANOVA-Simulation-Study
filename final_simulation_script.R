set.seed(12122024)

# Permutation Test Construction
permutation.test <- function(df, reps){
  perm_F <- NA
  colnames(df) <- c("y", "x")
  
  for(i in 1:reps){
    df_perm <- df
    df_perm$y <- sample(df_perm$y)
    perm_F[i] <- summary(aov(y ~ x, data=df_perm))[[1]][1, 4]
  }
  
  F_0 <- summary(aov(y ~ x, data=df))[[1]][1, 4]
  
  return((sum(perm_F >= abs(F_0)) + sum(perm_F <= -abs(F_0))) / reps)
}

# Simulation Study
reps <- 1000
iters <- 100



# Mean, Sample Size, Variance
# equal, equal, equal
# Means: 1, 1, 1
# Size:  4, 4, 4
# Vars:  1, 1, 1
aov_pvals_1 <- NA
perm_pvals_1 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 1, scale = 1)
  group2 <- rgamma(n = 4, shape = 1, scale = 1)
  group3 <- rgamma(n = 4, shape = 1, scale = 1)
  
  data_1 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_1[i] <- summary(aov(y ~ x, data = data_1))[[1]][1, 5]
  perm_pvals_1[i] <- permutation.test(df = data_1, reps)
}

result_aov_1 <- sum(aov_pvals_1 < 0.05) / iters
result_perm_1 <- sum(perm_pvals_1 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, equal, slight variation
# Means: 1, 1, 1
# Size:  4, 4, 4
# Vars:  1, 2, 4
aov_pvals_2 <- NA
perm_pvals_2 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 1, scale = 1)
  group2 <- rgamma(n = 4, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 4, shape = 0.25, scale = 4)
  
  data_2 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_2[i] <- summary(aov(y ~ x, data = data_2))[[1]][1, 5]
  perm_pvals_2[i] <- permutation.test(df = data_2, reps)
}

result_aov_2 <- sum(aov_pvals_2 < 0.05) / iters
result_perm_2 <- sum(perm_pvals_2 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, equal, large variation
# Means: 1, 1, 1
# Size:  4, 4, 4
# Vars:  1, 3, 9
aov_pvals_3 <- NA
perm_pvals_3 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 1, scale = 1)
  group2 <- rgamma(n = 4, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 4, shape = 1/9, scale = 9)
  
  data_3 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_3[i] <- summary(aov(y ~ x, data = data_3))[[1]][1, 5]
  perm_pvals_3[i] <- permutation.test(df = data_3, reps)
}

result_aov_3 <- sum(aov_pvals_3 < 0.05) / iters
result_perm_3 <- sum(perm_pvals_3 < 0.05) / iters


# Mean, Sample Size, Variance
# equal, slight imbalance, equal
# Means: 1, 1, 1
# Size:  3, 4, 5
# Vars:  1, 1, 1
aov_pvals_4 <- NA
perm_pvals_4 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 1, scale = 1)
  group2 <- rgamma(n = 4, shape = 1, scale = 1)
  group3 <- rgamma(n = 5, shape = 1, scale = 1)
  
  data_4 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_4[i] <- summary(aov(y ~ x, data = data_4))[[1]][1, 5]
  perm_pvals_4[i] <- permutation.test(df = data_4, reps)
}

result_aov_4 <- sum(aov_pvals_4 < 0.05) / iters
result_perm_4 <- sum(perm_pvals_4 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, slight imbalance, slight variation
# Means: 1, 1, 1
# Size:  3, 4, 5
# Vars:  1, 2, 4
aov_pvals_5 <- NA
perm_pvals_5 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 1, scale = 1)
  group2 <- rgamma(n = 4, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 5, shape = 0.25, scale = 4)
  
  data_5 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_5[i] <- summary(aov(y ~ x, data = data_5))[[1]][1, 5]
  perm_pvals_5[i] <- permutation.test(df = data_5, reps)
}

result_aov_5 <- sum(aov_pvals_5 < 0.05) / iters
result_perm_5 <- sum(perm_pvals_5 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, slight imbalance, large variation
# Means: 1, 1, 1
# Size:  3, 4, 5
# Vars:  1, 2, 4
aov_pvals_6 <- NA
perm_pvals_6 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 1, scale = 2)
  group2 <- rgamma(n = 4, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 5, shape = 0.25, scale = 4)
  
  data_6 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_6[i] <- summary(aov(y ~ x, data = data_6))[[1]][1, 5]
  perm_pvals_6[i] <- permutation.test(df = data_6, reps)
}

result_aov_6 <- sum(aov_pvals_6 < 0.05) / iters
result_perm_6 <- sum(perm_pvals_6 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance fwd, equal
# Means: 1, 1, 1
# Size:  2, 5, 8
# Vars:  1, 1, 1
aov_pvals_7 <- NA
perm_pvals_7 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 1, scale = 1)
  group3 <- rgamma(n = 8, shape = 1, scale = 1)
  
  data_7 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_7[i] <- summary(aov(y ~ x, data = data_7))[[1]][1, 5]
  perm_pvals_7[i] <- permutation.test(df = data_7, reps)
}

result_aov_7 <- sum(aov_pvals_7 < 0.05) / iters
result_perm_7 <- sum(perm_pvals_7 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance fwd, slight variation
# Means: 1, 1, 1
# Size:  2, 5, 8
# Vars:  1, 2, 4
aov_pvals_8 <- NA
perm_pvals_8 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 8, shape = 0.25, scale = 4)
  
  data_8 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_8[i] <- summary(aov(y ~ x, data = data_8))[[1]][1, 5]
  perm_pvals_8[i] <- permutation.test(df = data_8, reps)
}

result_aov_8 <- sum(aov_pvals_8 < 0.05) / iters
result_perm_8 <- sum(perm_pvals_8 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance fwd, large variation
# Means: 1, 1, 1
# Size:  2, 5, 8
# Vars:  1, 3, 9
aov_pvals_9 <- NA
perm_pvals_9 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 8, shape = 1/9, scale = 9)
  
  data_9 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_9[i] <- summary(aov(y ~ x, data = data_9))[[1]][1, 5]
  perm_pvals_9[i] <- permutation.test(df = data_9, reps)
}

result_aov_9 <- sum(aov_pvals_9 < 0.05) / iters
result_perm_9 <- sum(perm_pvals_9 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance bwd, equal
# Means: 1, 1, 1
# Size:  8, 5, 2
# Vars:  1, 1, 1
aov_pvals_10 <- NA
perm_pvals_10 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 1, scale = 1)
  group3 <- rgamma(n = 2, shape = 1, scale = 1)
  
  data_10 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_10[i] <- summary(aov(y ~ x, data = data_10))[[1]][1, 5]
  perm_pvals_10[i] <- permutation.test(df = data_10, reps)
}

result_aov_10 <- sum(aov_pvals_10 < 0.05) / iters
result_perm_10 <- sum(perm_pvals_10 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance bwd, slight variation
# Means: 1, 1, 1
# Size:  8, 5, 2
# Vars:  1, 2, 4
aov_pvals_11 <- NA
perm_pvals_11 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 2, shape = 0.25, scale = 4)
  
  data_11 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_11[i] <- summary(aov(y ~ x, data = data_11))[[1]][1, 5]
  perm_pvals_11[i] <- permutation.test(df = data_11, reps)
}

result_aov_11 <- sum(aov_pvals_11 < 0.05) / iters
result_perm_11 <- sum(perm_pvals_11 < 0.05) / iters



# Mean, Sample Size, Variance
# equal, large imbalance bwd, large variation
# Means: 1, 1, 1
# Size:  8, 5, 2
# Vars:  1, 3, 9
aov_pvals_12 <- NA
perm_pvals_12 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 1, scale = 1)
  group2 <- rgamma(n = 5, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 2, shape = 1/9, scale = 9)
  
  data_12 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_12[i] <- summary(aov(y ~ x, data = data_12))[[1]][1, 5]
  perm_pvals_12[i] <- permutation.test(df = data_12, reps)
}

result_aov_12 <- sum(aov_pvals_12 < 0.05) / iters
result_perm_12 <- sum(perm_pvals_12 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, equal, equal
# Means: 3, 1, 1
# Size:  4, 4, 4
# Vars:  1, 1, 1
aov_pvals_13 <- NA
perm_pvals_13 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 3, scale = 1)
  group2 <- rgamma(n = 4, shape = 1, scale = 1)
  group3 <- rgamma(n = 4, shape = 1, scale = 1)
  
  data_13 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_13[i] <- summary(aov(y ~ x, data = data_13))[[1]][1, 5]
  perm_pvals_13[i] <- permutation.test(df = data_13, reps)
}

result_aov_13 <- sum(aov_pvals_13 < 0.05) / iters
result_perm_13 <- sum(perm_pvals_13 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, equal, slight variation
# Means: 3, 1, 1
# Size:  4, 4, 4
# Vars:  1, 2, 4
aov_pvals_14 <- NA
perm_pvals_14 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 3, scale = 1)
  group2 <- rgamma(n = 4, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 4, shape = 0.25, scale = 4)
  
  data_14 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_14[i] <- summary(aov(y ~ x, data = data_14))[[1]][1, 5]
  perm_pvals_14[i] <- permutation.test(df = data_14, reps)
}

result_aov_14 <- sum(aov_pvals_14 < 0.05) / iters
result_perm_14 <- sum(perm_pvals_14 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, equal, large variation
# Means: 3, 1, 1
# Size:  4, 4, 4
# Vars:  1, 3, 9
aov_pvals_15 <- NA
perm_pvals_15 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 3, scale = 1)
  group2 <- rgamma(n = 4, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 4, shape = 1/9, scale = 9)
  
  data_15 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_15[i] <- summary(aov(y ~ x, data = data_15))[[1]][1, 5]
  perm_pvals_15[i] <- permutation.test(df = data_15, reps)
}

result_aov_15 <- sum(aov_pvals_15 < 0.05) / iters
result_perm_15 <- sum(perm_pvals_15 < 0.05) / iters


# Mean, Sample Size, Variance
# one different, slight imbalance, equal
# Means: 3, 1, 1
# Size:  3, 4, 5
# Vars:  1, 1, 1
aov_pvals_16 <- NA
perm_pvals_16 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 1, scale = 1)
  group3 <- rgamma(n = 5, shape = 1, scale = 1)
  
  data_16 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_16[i] <- summary(aov(y ~ x, data = data_16))[[1]][1, 5]
  perm_pvals_16[i] <- permutation.test(df = data_16, reps)
}

result_aov_16 <- sum(aov_pvals_16 < 0.05) / iters
result_perm_16 <- sum(perm_pvals_16 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, slight imbalance, slight variation
# Means: 3, 1, 1
# Size:  3, 4, 5
# Vars:  1, 2, 4
aov_pvals_17 <- NA
perm_pvals_17 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 5, shape = 0.25, scale = 4)
  
  data_17 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_17[i] <- summary(aov(y ~ x, data = data_17))[[1]][1, 5]
  perm_pvals_17[i] <- permutation.test(df = data_17, reps)
}

result_aov_17 <- sum(aov_pvals_17 < 0.05) / iters
result_perm_17 <- sum(perm_pvals_17 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, slight imbalance, large variation
# Means: 3, 1, 1
# Size:  3, 4, 5
# Vars:  1, 3, 9
aov_pvals_18 <- NA
perm_pvals_18 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 5, shape = 1/9, scale = 9)
  
  data_18 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_18[i] <- summary(aov(y ~ x, data = data_18))[[1]][1, 5]
  perm_pvals_18[i] <- permutation.test(df = data_18, reps)
}

result_aov_18 <- sum(aov_pvals_18 < 0.05) / iters
result_perm_18 <- sum(perm_pvals_18 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance fwd, equal
# Means: 3, 1, 1
# Size:  2, 5, 8
# Vars:  1, 1, 1
aov_pvals_19 <- NA
perm_pvals_19 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 1, scale = 1)
  group3 <- rgamma(n = 8, shape = 1, scale = 1)
  
  data_19 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_19[i] <- summary(aov(y ~ x, data = data_19))[[1]][1, 5]
  perm_pvals_19[i] <- permutation.test(df = data_19, reps)
}

result_aov_19 <- sum(aov_pvals_19 < 0.05) / iters
result_perm_19 <- sum(perm_pvals_19 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance fwd, slight variation
# Means: 3, 1, 1
# Size:  2, 5, 8
# Vars:  1, 2, 4
aov_pvals_20 <- NA
perm_pvals_20 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 8, shape = 0.25, scale = 4)
  
  data_20 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_20[i] <- summary(aov(y ~ x, data = data_20))[[1]][1, 5]
  perm_pvals_20[i] <- permutation.test(df = data_20, reps)
}

result_aov_20 <- sum(aov_pvals_20 < 0.05) / iters
result_perm_20 <- sum(perm_pvals_20 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance fwd, large variation
# Means: 3, 1, 1
# Size:  2, 5, 8
# Vars:  1, 3, 9
aov_pvals_21 <- NA
perm_pvals_21 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 8, shape = 1/9, scale = 9)
  
  data_21 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_21[i] <- summary(aov(y ~ x, data = data_21))[[1]][1, 5]
  perm_pvals_21[i] <- permutation.test(df = data_21, reps)
}

result_aov_21 <- sum(aov_pvals_21 < 0.05) / iters
result_perm_21 <- sum(perm_pvals_21 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance bwd, equal
# Means: 3, 1, 1
# Size:  8, 5, 2
# Vars:  1, 1, 1
aov_pvals_22 <- NA
perm_pvals_22 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 1, scale = 1)
  group3 <- rgamma(n = 2, shape = 1, scale = 1)
  
  data_22 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_22[i] <- summary(aov(y ~ x, data = data_22))[[1]][1, 5]
  perm_pvals_22[i] <- permutation.test(df = data_22, reps)
}

result_aov_22 <- sum(aov_pvals_22 < 0.05) / iters
result_perm_22 <- sum(perm_pvals_22 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance bwd, slight variation
# Means: 3, 1, 1
# Size:  8, 5, 2
# Vars:  1, 2, 4
aov_pvals_23 <- NA
perm_pvals_23 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 0.5, scale = 2)
  group3 <- rgamma(n = 2, shape = 0.25, scale = 4)
  
  data_23 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_23[i] <- summary(aov(y ~ x, data = data_23))[[1]][1, 5]
  perm_pvals_23[i] <- permutation.test(df = data_23, reps)
}

result_aov_23 <- sum(aov_pvals_23 < 0.05) / iters
result_perm_23 <- sum(perm_pvals_23 < 0.05) / iters



# Mean, Sample Size, Variance
# one different, large imbalance bwd, large variation
# Means: 3, 1, 1
# Size:  8, 5, 2
# Vars:  1, 3, 9
aov_pvals_24 <- NA
perm_pvals_24 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 1/3, scale = 3)
  group3 <- rgamma(n = 2, shape = 1/9, scale = 9)
  
  data_24 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_24[i] <- summary(aov(y ~ x, data = data_24))[[1]][1, 5]
  perm_pvals_24[i] <- permutation.test(df = data_24, reps)
}

result_aov_24 <- sum(aov_pvals_24 < 0.05) / iters
result_perm_24 <- sum(perm_pvals_24 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, equal, equal
# Means: 3, 2, 1
# Size:  4, 4, 4
# Vars:  1, 1, 1
aov_pvals_25 <- NA
perm_pvals_25 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 4, scale = 1/2)
  group3 <- rgamma(n = 4, shape = 1, scale = 1)
  
  data_25 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_25[i] <- summary(aov(y ~ x, data = data_25))[[1]][1, 5]
  perm_pvals_25[i] <- permutation.test(df = data_25, reps)
}

result_aov_25 <- sum(aov_pvals_25 < 0.05) / iters
result_perm_25 <- sum(perm_pvals_25 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, equal, slight variation
# Means: 3, 2, 1
# Size:  4, 4, 4
# Vars:  1, 2, 4
aov_pvals_26 <- NA
perm_pvals_26 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 2, scale = 1)
  group3 <- rgamma(n = 4, shape = 1/4, scale = 4)
  
  data_26 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_26[i] <- summary(aov(y ~ x, data = data_26))[[1]][1, 5]
  perm_pvals_26[i] <- permutation.test(df = data_26, reps)
}

result_aov_26 <- sum(aov_pvals_26 < 0.05) / iters
result_perm_26 <- sum(perm_pvals_26 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, equal, large variation
# Means: 3, 2, 1
# Size:  4, 4, 4
# Vars:  1, 3, 9
aov_pvals_27 <- NA
perm_pvals_27 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 4/3, scale = 1.5)
  group3 <- rgamma(n = 4, shape = 1/9, scale = 9)
  
  data_27 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_27[i] <- summary(aov(y ~ x, data = data_27))[[1]][1, 5]
  perm_pvals_27[i] <- permutation.test(df = data_27, reps)
}

result_aov_27 <- sum(aov_pvals_27 < 0.05) / iters
result_perm_27 <- sum(perm_pvals_27 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, slight imbalance, equal
# Means: 3, 2, 1
# Size:  3, 4, 5
# Vars:  1, 1, 1
aov_pvals_28 <- NA
perm_pvals_28 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 4, scale = 0.5)
  group3 <- rgamma(n = 5, shape = 1, scale = 1)
  
  data_28 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_28[i] <- summary(aov(y ~ x, data = data_28))[[1]][1, 5]
  perm_pvals_28[i] <- permutation.test(df = data_28, reps)
}

result_aov_28 <- sum(aov_pvals_28 < 0.05) / iters
result_perm_28 <- sum(perm_pvals_28 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, slight imbalance, slight variation
# Means: 3, 2, 1
# Size:  3, 4, 5
# Vars:  1, 2, 4
aov_pvals_29 <- NA
perm_pvals_29 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 2, scale = 1)
  group3 <- rgamma(n = 5, shape = 4, scale = 1/4)
  
  data_29 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_29[i] <- summary(aov(y ~ x, data = data_29))[[1]][1, 5]
  perm_pvals_29[i] <- permutation.test(df = data_29, reps)
}

result_aov_29 <- sum(aov_pvals_29 < 0.05) / iters
result_perm_29 <- sum(perm_pvals_29 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, slight imbalance, large variation
# Means: 3, 2, 1
# Size:  3, 4, 5
# Vars:  1, 3, 9
aov_pvals_30 <- NA
perm_pvals_30 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 4, shape = 4/3, scale = 1.5)
  group3 <- rgamma(n = 5, shape = 1/9, scale = 9)
  
  data_30 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_30[i] <- summary(aov(y ~ x, data = data_30))[[1]][1, 5]
  perm_pvals_30[i] <- permutation.test(df = data_30, reps)
}

result_aov_30 <- sum(aov_pvals_30 < 0.05) / iters
result_perm_30 <- sum(perm_pvals_30 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance fwd, equal
# Means: 3, 2, 1
# Size:  2, 5, 8
# Vars:  1, 1, 1
aov_pvals_31 <- NA
perm_pvals_31 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 4, scale = 0.5)
  group3 <- rgamma(n = 8, shape = 1, scale = 1)
  
  data_31 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_31[i] <- summary(aov(y ~ x, data = data_31))[[1]][1, 5]
  perm_pvals_31[i] <- permutation.test(df = data_31, reps)
}

result_aov_31 <- sum(aov_pvals_31 < 0.05) / iters
result_perm_31 <- sum(perm_pvals_31 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance fwd, slight variation
# Means: 3, 2, 1
# Size:  2, 5, 8
# Vars:  1, 2, 4
aov_pvals_32 <- NA
perm_pvals_32 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 2, scale = 1)
  group3 <- rgamma(n = 8, shape = 1/4, scale = 4)
  
  data_32 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_32[i] <- summary(aov(y ~ x, data = data_32))[[1]][1, 5]
  perm_pvals_32[i] <- permutation.test(df = data_32, reps)
}

result_aov_32 <- sum(aov_pvals_32 < 0.05) / iters
result_perm_32 <- sum(perm_pvals_32 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance fwd, large variation
# Means: 3, 2, 1
# Size:  2, 5, 8
# Vars:  1, 3, 9
aov_pvals_33 <- NA
perm_pvals_33 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 4/3, scale = 1.5)
  group3 <- rgamma(n = 8, shape = 1/9, scale = 9)
  
  data_33 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_33[i] <- summary(aov(y ~ x, data = data_33))[[1]][1, 5]
  perm_pvals_33[i] <- permutation.test(df = data_33, reps)
}

result_aov_33 <- sum(aov_pvals_33 < 0.05) / iters
result_perm_33 <- sum(perm_pvals_33 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance bwd, equal
# Means: 3, 2, 1
# Size:  8, 5, 2
# Vars:  1, 1, 1
aov_pvals_34 <- NA
perm_pvals_34 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 2, scale = 0.5)
  group3 <- rgamma(n = 2, shape = 1, scale = 1)
  
  data_34 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_34[i] <- summary(aov(y ~ x, data = data_34))[[1]][1, 5]
  perm_pvals_34[i] <- permutation.test(df = data_34, reps)
}

result_aov_34 <- sum(aov_pvals_34 < 0.05) / iters
result_perm_34 <- sum(perm_pvals_34 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance bwd, slight variation
# Means: 3, 2, 1
# Size:  8, 5, 2
# Vars:  1, 2, 4
aov_pvals_35 <- NA
perm_pvals_35 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 2, scale = 1)
  group3 <- rgamma(n = 2, shape = 1/4, scale = 4)
  
  data_35 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_35[i] <- summary(aov(y ~ x, data = data_35))[[1]][1, 5]
  perm_pvals_35[i] <- permutation.test(df = data_35, reps)
}

result_aov_35 <- sum(aov_pvals_35 < 0.05) / iters
result_perm_35 <- sum(perm_pvals_35 < 0.05) / iters



# Mean, Sample Size, Variance
# all slightly different, large imbalance bwd, large variation
# Means: 3, 2, 1
# Size:  8, 5, 2
# Vars:  1, 3, 9
aov_pvals_36 <- NA
perm_pvals_36 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 9, scale = 1/3)
  group2 <- rgamma(n = 5, shape = 4/3, scale = 1.5)
  group3 <- rgamma(n = 2, shape = 1/9, scale = 9)
  
  data_36 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_36[i] <- summary(aov(y ~ x, data = data_36))[[1]][1, 5]
  perm_pvals_36[i] <- permutation.test(df = data_36, reps)
}

result_aov_36 <- sum(aov_pvals_36 < 0.05) / iters
result_perm_36 <- sum(perm_pvals_36 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, equal, equal
# Means: 7, 4, 2
# Size:  4, 4, 4
# Vars:  1, 1, 1
aov_pvals_37 <- NA
perm_pvals_37 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 16, scale = 0.25)
  group3 <- rgamma(n = 4, shape = 4, scale = 0.5)
  
  data_37 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_37[i] <- summary(aov(y ~ x, data = data_37))[[1]][1, 5]
  perm_pvals_37[i] <- permutation.test(df = data_37, reps)
}

result_aov_37 <- sum(aov_pvals_37 < 0.05) / iters
result_perm_37 <- sum(perm_pvals_37 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, equal, slight variation
# Means: 7, 4, 2
# Size:  4, 4, 4
# Vars:  1, 2, 4
aov_pvals_38 <- NA
perm_pvals_38 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 16, scale = 0.25)
  group3 <- rgamma(n = 4, shape = 4, scale = 0.5)
  
  data_38 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_38[i] <- summary(aov(y ~ x, data = data_38))[[1]][1, 5]
  perm_pvals_38[i] <- permutation.test(df = data_38, reps)
}

result_aov_38 <- sum(aov_pvals_38 < 0.05) / iters
result_perm_38 <- sum(perm_pvals_38 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, equal, large variation
# Means: 7, 4, 2
# Size:  4, 4, 4
# Vars:  1, 3, 9
aov_pvals_39 <- NA
perm_pvals_39 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 4, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 16/3, scale = 0.75)
  group3 <- rgamma(n = 4, shape = 4/9, scale = 4.5)
  
  data_39 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_39[i] <- summary(aov(y ~ x, data = data_39))[[1]][1, 5]
  perm_pvals_39[i] <- permutation.test(df = data_39, reps)
}

result_aov_39 <- sum(aov_pvals_39 < 0.05) / iters
result_perm_39 <- sum(perm_pvals_39 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, slight imbalance, equal
# Means: 7, 4, 2
# Size:  3, 4, 5
# Vars:  1, 1, 1
aov_pvals_40 <- NA
perm_pvals_40 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 16, scale = 0.25)
  group3 <- rgamma(n = 5, shape = 4, scale = 0.5)
  
  data_40 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_40[i] <- summary(aov(y ~ x, data = data_40))[[1]][1, 5]
  perm_pvals_40[i] <- permutation.test(df = data_40, reps)
}

result_aov_40 <- sum(aov_pvals_40 < 0.05) / iters
result_perm_40 <- sum(perm_pvals_40 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, slight imbalance, slight variation
# Means: 7, 4, 2
# Size:  3, 4, 5
# Vars:  1, 2, 4
aov_pvals_41 <- NA
perm_pvals_41 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 8, scale = 0.5)
  group3 <- rgamma(n = 5, shape = 1, scale = 2)
  
  data_41 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_41[i] <- summary(aov(y ~ x, data = data_41))[[1]][1, 5]
  perm_pvals_41[i] <- permutation.test(df = data_41, reps)
}

result_aov_41 <- sum(aov_pvals_41 < 0.05) / iters
result_perm_41 <- sum(perm_pvals_41 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, slight imbalance, large variation
# Means: 7, 4, 2
# Size:  3, 4, 5
# Vars:  1, 3, 9
aov_pvals_42 <- NA
perm_pvals_42 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 3, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 4, shape = 16/3, scale = 0.75)
  group3 <- rgamma(n = 5, shape = 4/9, scale = 4.5)
  
  data_42 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_42[i] <- summary(aov(y ~ x, data = data_42))[[1]][1, 5]
  perm_pvals_42[i] <- permutation.test(df = data_42, reps)
}

result_aov_42 <- sum(aov_pvals_42 < 0.05) / iters
result_perm_42 <- sum(perm_pvals_42 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance fwd, equal
# Means: 7, 4, 2
# Size:  2, 5, 8
# Vars:  1, 1, 1
aov_pvals_43 <- NA
perm_pvals_43 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 16, scale = 0.25)
  group3 <- rgamma(n = 8, shape = 4, scale = 0.5)
  
  data_43 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_43[i] <- summary(aov(y ~ x, data = data_43))[[1]][1, 5]
  perm_pvals_43[i] <- permutation.test(df = data_43, reps)
}

result_aov_43 <- sum(aov_pvals_43 < 0.05) / iters
result_perm_43 <- sum(perm_pvals_43 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance fwd, slight variation
# Means: 7, 4, 2
# Size:  2, 5, 8
# Vars:  1, 2, 4
aov_pvals_44 <- NA
perm_pvals_44 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 8, scale = 0.5)
  group3 <- rgamma(n = 8, shape = 1, scale = 2)
  
  data_44 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_44[i] <- summary(aov(y ~ x, data = data_44))[[1]][1, 5]
  perm_pvals_44[i] <- permutation.test(df = data_44, reps)
}

result_aov_44 <- sum(aov_pvals_44 < 0.05) / iters
result_perm_44 <- sum(perm_pvals_44 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance fwd, large variation
# Means: 7, 4, 2
# Size:  2, 5, 8
# Vars:  1, 3, 9
aov_pvals_45 <- NA
perm_pvals_45 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 2, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 16/3, scale = 0.75)
  group3 <- rgamma(n = 8, shape = 4/9, scale = 4.5)
  
  data_45 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_45[i] <- summary(aov(y ~ x, data = data_45))[[1]][1, 5]
  perm_pvals_45[i] <- permutation.test(df = data_45, reps)
}

result_aov_45 <- sum(aov_pvals_45 < 0.05) / iters
result_perm_45 <- sum(perm_pvals_45 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance bwd, equal
# Means: 7, 4, 2
# Size:  8, 5, 2
# Vars:  1, 1, 1
aov_pvals_46 <- NA
perm_pvals_46 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 16, scale = 0.25)
  group3 <- rgamma(n = 2, shape = 4, scale = 0.5)
  
  data_46 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_46[i] <- summary(aov(y ~ x, data = data_46))[[1]][1, 5]
  perm_pvals_46[i] <- permutation.test(df = data_46, reps)
}

result_aov_46 <- sum(aov_pvals_46 < 0.05) / iters
result_perm_46 <- sum(perm_pvals_46 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance bwd, slight variation
# Means: 7, 4, 2
# Size:  8, 5, 2
# Vars:  1, 2, 4
aov_pvals_47 <- NA
perm_pvals_47 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 8, scale = 0.5)
  group3 <- rgamma(n = 2, shape = 1, scale = 2)
  
  data_47 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_47[i] <- summary(aov(y ~ x, data = data_47))[[1]][1, 5]
  perm_pvals_47[i] <- permutation.test(df = data_47, reps)
}

result_aov_47 <- sum(aov_pvals_47 < 0.05) / iters
result_perm_47 <- sum(perm_pvals_47 < 0.05) / iters



# Mean, Sample Size, Variance
# all very different, large imbalance bwd, large variation
# Means: 7, 4, 2
# Size:  8, 5, 2
# Vars:  1, 3, 9
aov_pvals_48 <- NA
perm_pvals_48 <- NA

for(i in 1:iters){
  group1 <- rgamma(n = 8, shape = 49, scale = 1/7)
  group2 <- rgamma(n = 5, shape = 16/3, scale = 0.75)
  group3 <- rgamma(n = 2, shape = 4/9, scale = 4.5)
  
  data_48 <- data.frame(
    y = c(group1, group2, group3),
    x = c(rep("group1", length(group1)),
          rep("group2", length(group2)),
          rep("group3", length(group3)))
  )
  aov_pvals_48[i] <- summary(aov(y ~ x, data = data_48))[[1]][1, 5]
  perm_pvals_48[i] <- permutation.test(df = data_48, reps)
}

result_aov_48 <- sum(aov_pvals_48 < 0.05) / iters
result_perm_48 <- sum(perm_pvals_48 < 0.05) / iters

result <- data.frame(
  metric = c(rep("type_I_error", 12),
             rep("power", 36)),
  aov = c(result_aov_1, result_aov_2, result_aov_3,
          result_aov_4, result_aov_5, result_aov_6,
          result_aov_7, result_aov_8, result_aov_9,
          result_aov_10, result_aov_11, result_aov_12,
          result_aov_13, result_aov_14, result_aov_15,
          result_aov_16, result_aov_17, result_aov_18,
          result_aov_19, result_aov_20, result_aov_21,
          result_aov_22, result_aov_23, result_aov_24,
          result_aov_25, result_aov_26, result_aov_27,
          result_aov_28, result_aov_29, result_aov_30,
          result_aov_31, result_aov_32, result_aov_33,
          result_aov_34, result_aov_35, result_aov_36,
          result_aov_37, result_aov_38, result_aov_39,
          result_aov_40, result_aov_41, result_aov_42,
          result_aov_43, result_aov_44, result_aov_45,
          result_aov_46, result_aov_47, result_aov_48),
  perm = c(result_perm_1, result_perm_2, result_perm_3,
           result_perm_4, result_perm_5, result_perm_6,
           result_perm_7, result_perm_8, result_perm_9,
           result_perm_10, result_perm_11, result_perm_12,
           result_perm_13, result_perm_14, result_perm_15,
           result_perm_16, result_perm_17, result_perm_18,
           result_perm_19, result_perm_20, result_perm_21,
           result_perm_22, result_perm_23, result_perm_24,
           result_perm_25, result_perm_26, result_perm_27,
           result_perm_28, result_perm_29, result_perm_30,
           result_perm_31, result_perm_32, result_perm_33,
           result_perm_34, result_perm_35, result_perm_36,
           result_perm_37, result_perm_38, result_perm_39,
           result_perm_40, result_perm_41, result_perm_42,
           result_perm_43, result_perm_44, result_perm_45,
           result_perm_46, result_perm_47, result_perm_48),
  mean = c(rep("equal", 12), rep("one_different", 12),
           rep("all_slightly_different", 12), rep("all_very_different", 12)),
  sample_size = rep(c(rep("equal", 3),
                      rep("slight_imbalance", 3),
                      rep("large_imbalance_fwd", 3),
                      rep("large_imbalance_bwd", 3)), 4),
  variance = rep(c("equal", "slight_variation", "large_variation"), 16)
)