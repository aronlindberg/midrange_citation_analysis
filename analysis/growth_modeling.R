# This uses the crude parsing of exploration and exploitation where exploration is simply extension+modification, whereas exploitation is instantiation.

# Using the "from year1 data"
exploitation_growth <- exploitation
exploitation_growth <- exploitation_growth[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- exploration
exploration_growth <- exploration_growth[,2:21]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

# Now convert to cumulative sums for exploration
cumsum_exploration <- matrix(nrow = nrow(exploration_growth), ncol = ncol(exploration_growth)-1)
for (i in 1:nrow(exploration_growth)){
  cumsum_exploration[i,] <- cumsum(as.numeric(exploration_growth[i,2:ncol(exploration_growth)]))
}
cumsum_exploration_df <- as.data.frame(cbind(exploration_growth[,1], cumsum_exploration))
colnames(cumsum_exploration_df) <- colnames(exploration_growth)

# Now convert to cumulative sums for exploitation
cumsum_exploitation <- matrix(nrow = nrow(exploitation_growth), ncol = ncol(exploitation_growth)-1)
for (i in 1:nrow(exploitation)){
  cumsum_exploitation[i,] <- cumsum(as.numeric(exploitation_growth[i,2:ncol(exploitation_growth)]))
}
cumsum_exploitation_df <- as.data.frame(cbind(exploitation_growth[,1], cumsum_exploitation))
colnames(cumsum_exploitation_df) <- colnames(exploitation_growth)

# Now we do the latent growth analysis using cumulative sums
exploitation_growth <- cumsum_exploitation_df
exploitation_growth <- exploitation_growth[,1:20]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- cumsum_exploration_df
exploration_growth <- exploration_growth[,1:20]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)
h_index <- c(data$h1_initial_2013_average_sum[data$X4_Classification..Exploitation.Exploration=="Exploration"], data$h1_initial_2013_average_sum[data$X4_Classification..Exploitation.Exploration=="Exploitation"])

# To avoid a Heywood case, I rescale the data here
# growth_data[1:20] <- scale(growth_data[1:20])
growth_data <- cbind(growth_data, h_index)

# Multi-group Growth model
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20
t8 ~~ 0.01*t8
t17 ~~ 0.01*t17
t18 ~~ 0.01*t18
# regressions
s ~ h_index
i ~ h_index'
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_UNconstrained)

fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("lv.variances"), estimator = "ML")
fit_constrained_variances <- growth(model, data=growth_data, group = "type", group.equal = c("residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_variances_and_means <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_means <- growth(model, data=growth_data, group = "type", group.equal = c("means"))
fit_constrained_intercepts <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts"))

summary(fit_constrained)
summary(fit_constrained_variances)
summary(fit_constrained_variances_and_means)
summary(fit_constrained_means)
summary(fit_constrained_intercepts)
