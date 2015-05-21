# This uses the neat parsing of exploration and exploitation where exploration is simply extension+modification (medium change), whereas exploitation is instantiation+modification (minor change).

# Latent growth curve modelling
# First using the raw data
exploitation_growth <- exploitation_neat
exploitation_growth <- exploitation_growth[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- exploration_neat
exploration_growth <- exploration_growth[,2:21]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)

# Multi-group Growth model
model <- ' i =~ 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
s =~ 0*t2 + 1*t3 + 2*t4 + 3*t5 + 4*t6 + 5*t7 + 6*t8 + 7*t9 + 8*t10 + 9*t11 + 10*t12 + 11*t13 + 12*t14 + 13*t15 + 14*t16 + 15*t17 + 16*t18 + 17*t19 + 18*t20 
t2 ~~ 0.01*t2
t16 ~~ 0.01*t16'
fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("means"))
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_constrained)
summary(fit_UNconstrained)


# Then using the "from year1 data"
exploitation_growth <- neat_exploitation
exploitation_growth <- exploitation_growth[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- neat_exploration
exploration_growth <- exploration_growth[,2:21]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)
# To avoid a Heywood case, I rescale the data here
growth_data[1:21] <- scale(growth_data[1:21])

# Multi-group Growth model
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20
t17 ~~ 0.001*t17
t18 ~~ 0.001*t18'
fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("lv.variances"))
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_constrained)
summary(fit_UNconstrained)

# Now convert to cumulative sums for exploration
cumsum_exploration <- matrix(nrow = nrow(neat_exploration), ncol = ncol(exploration)-1)
for (i in 1:nrow(neat_exploration)){
  cumsum_exploration[i,] <- cumsum(as.numeric(neat_exploration[i,2:ncol(neat_exploration)]))
}
cumsum_exploration_df <- as.data.frame(cbind(neat_exploration[,1], cumsum_exploration))
colnames(cumsum_exploration_df) <- colnames(neat_exploration)

# Now convert to cumulative sums for exploitation
cumsum_exploitation <- matrix(nrow = nrow(neat_exploitation), ncol = ncol(neat_exploitation)-1)
for (i in 1:nrow(neat_exploitation)){
  cumsum_exploitation[i,] <- cumsum(as.numeric(neat_exploitation[i,2:ncol(neat_exploitation)]))
}
cumsum_exploitation_df <- as.data.frame(cbind(neat_exploitation[,1], cumsum_exploitation))
colnames(cumsum_exploitation_df) <- colnames(neat_exploitation)

# Save the cumulative sum datasets
# write.csv(cumsum_exploitation_df, file = "cumsum_exploitation_df.csv")
# write.csv(cumsum_exploration_df, file = "cumsum_exploration_df.csv")


# Now we redo the latent growth analysis using cumulative sums
exploitation_growth <- cumsum_exploitation_df
exploitation_growth <- exploitation_growth[,2:22]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21")

exploration_growth <- cumsum_exploration_df
exploration_growth <- exploration_growth[,2:22]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21")

a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)

# Multi-group Growth model
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20 + 1*t21
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 + 20*t21'
fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("means"))
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_constrained)
summary(fit_UNconstrained)

fit_constrained_variances <- growth(model, data=growth_data, group = "type", group.equal = c("residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_variances_and_means <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(fit_constrained_variances)
summary(fit_constrained_variances_and_means)