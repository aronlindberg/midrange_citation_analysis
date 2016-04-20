# This uses the crude parsing of exploration and exploitation where exploration is simply extension+modification, whereas exploitation is instantiation.

# Using the "from year1 data"
exploitation_growth <- exploitation[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- exploration[,2:21]
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
exploration_growth <- cumsum_exploration_df

# Insert exploration
a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)

data <- arrange(data, desc(X4_Classification..Exploitation.Exploration))

h_index <- c(data$h1_initial_2013_average_sum[data$X4_Classification..Exploitation.Exploration=="Exploration"], data$h1_initial_2013_average_sum[data$X4_Classification..Exploitation.Exploration=="Exploitation"])

h_index_max_author <- c(data$h1_2013_max_author[data$X4_Classification..Exploitation.Exploration=="Exploration"], data$h1_2013_max_author[data$X4_Classification..Exploitation.Exploration=="Exploitation"])

# To avoid a Heywood case, I rescale the data here
growth_data <- cbind(growth_data, h_index, h_index_max_author)

# Multi-group Growth model
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20
# regressions
s ~ h_index
i ~ h_index
# fixed parameters
t8 ~~ 0.01*t8
t17 ~~ 0.01*t17
t18 ~~ 0.01*t18'
fit_UNconstrained <- growth(model, data=growth_data, group = "type")

fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("lv.variances"), estimator = "ML")
fit_constrained_variances <- growth(model, data=growth_data, group = "type", group.equal = c("residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_variances_and_means <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_means <- growth(model, data=growth_data, group = "type", group.equal = c("means"))
fit_constrained_intercepts <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts"))

summary(fit_UNconstrained)
summary(fit_constrained)
summary(fit_constrained_variances)
summary(fit_constrained_variances_and_means)
summary(fit_constrained_means)
summary(fit_constrained_intercepts)

# Comparing the effect of h-index and group
growth_data_standardized <- scale(growth_data[1:20])
growth_data_standardized <- cbind(as.data.frame(growth_data_standardized), growth_data$type, h_index, h_index_max_author)
names(growth_data_standardized)[21] <- "type"
growth_data_standardized$type_dummy <- as.numeric(growth_data_standardized$type)-1

model_regressions <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20

logit =~ 0*t1 + logit(1)*t2 + logit(2)*t3 + logit(3)*t4 + logit(4)*t5 + logit(5)*t6 + logit(6)*t7 + logit(7)*t8 + logit(8)*t9 + logit(9)*t10 + logit(10)*t11 + logit(11)*t12 + logit(12)*t13 + logit(13)*t14 + logit(14)*t15 + logit(15)*t16 + logit(16)*t17 + logit(17)*t18 + logit(18)*t19 + logit(19)*t20

# fixing error-variances
t8 ~~ 0.01*t8
t17 ~~ 0.01*t17
t18 ~~ 0.01*t18
# regressions
logit ~ h_index
i ~ h_index'

fit_UNconstrained_no_moderation <- growth(model_regressions, data=growth_data_standardized, group = NULL)
summary(fit_UNconstrained_no_moderation)

fit_UNconstrained_standardized <- growth(model_regressions, data=growth_data_standardized, group = "type")


fit_constrained_standardized <- growth(model, data=growth_data_standardized, group = "type", group.equal = c("lv.variances"), estimator = "ML")
fit_constrained_variances_standardized <- growth(model, data=growth_data_standardized, group = "type", group.equal = c("residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_variances_and_means_standardized <- growth(model, data=growth_data_standardized, group = "type", group.equal = c("intercepts", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_means_standardized <- growth(model, data=growth_data_standardized, group = "type", group.equal = c("means"))
fit_constrained_intercepts_standardized <- growth(model, data=growth_data_standardized, group = "type", group.equal = c("intercepts"))

summary(fit_UNconstrained_standardized)

summary(fit_constrained_standardized)
summary(fit_constrained_variances_standardized)
summary(fit_constrained_variances_and_means_standardized)
summary(fit_constrained_means_standardized)
summary(fit_constrained_intercepts_standardized)
