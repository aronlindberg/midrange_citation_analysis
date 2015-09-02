setwd("/Users/Aron/dropbox/Middle-Range Theorizing/Second Paper/")
library(gdata)
library(poweRlaw)
library(dunn.test)

data <- read.csv("middle_range_coding_v4_w_borrowing.csv", header = TRUE, fill = FALSE, fileEncoding = "latin1")
data <- data[data$Year > 1994,]

data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Indiidual", replacement = "Individual")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Org$", replacement = "Organization")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Firm", replacement = "Organization")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Project", replacement = "Group")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Group/Network", replacement = "Network")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Organization/Network", replacement = "Network")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Interorg", replacement = "Network")

data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Technical", replacement = "Artifact")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Database", replacement = "Artifact")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Organization/Government/national level", replacement = "Government")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Multilevel", replacement = "Other")
data$Unit.of.Analysis <- gsub(data$Unit.of.Analysis, pattern = "Online Auction", replacement = "Other")

data$Treatment.of.IT <- gsub(data$Treatment.of.IT, pattern = "Computational ", replacement = "Computational")
data$Treatment.of.IT <- gsub(data$Treatment.of.IT, pattern = "Tool ", replacement = "Tool")
data$Treatment.of.IT <- gsub(data$Treatment.of.IT, pattern = "Nominal ", replacement = "Nominal")
data$Treatment.of.IT <- gsub(data$Treatment.of.IT, pattern = "Proxy ", replacement = "Proxy")
data$Treatment.of.IT <- gsub(data$Treatment.of.IT, pattern = "Ensemble ", replacement = "Ensemble")

data$Classification..Exploitation.Exploration[data$Primary..outside.of.IS..or.Secondary..inside.of.IS..borrowing.=="Secondary"] <- "Exploitation"

## TOTAL CITES
extending_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Extending")

modifying_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Modifying")

instantiation_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Instantiation")

exploitation_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..Exploitation.Exploration == "Exploitation")

exploration_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..Exploitation.Exploration == "Exploration")


# Take out zeroes
instantiation_total_cites <- subset(instantiation_total_cites, instantiation_total_cites != 0)
modifying_total_cites <- subset(modifying_total_cites, modifying_total_cites != 0)
extending_total_cites <- subset(extending_total_cites, extending_total_cites != 0)

# Plotting
hist(instantiation_total_cites, breaks = 50, ylim = c(0, 30), xlim = c(0, 750))
hist(modifying_total_cites, breaks = 50, ylim = c(0, 30), xlim = c(0, 750))
hist(extending_total_cites, breaks = 50, ylim = c(0, 30), xlim = c(0, 750))

# Non-parametric t-test
wilcox.test(extending_total_cites, instantiation_total_cites, probs = 0.05, alternative = c("greater"))
wilcox.test(extending_total_cites, modifying_total_cites, probs = 0.05, alternative = c("greater"))
wilcox.test(instantiation_total_cites, modifying_total_cites, probs = 0.05, alternative = c("less"))

# Transform to normal
log_instantiation_total_cites <- log(instantiation_total_cites)
log_modifying_total_cites <- log(modifying_total_cites)
log_extending_total_cites <- log(extending_total_cites)

# Inspect visually
hist(log_instantiation_total_cites)
hist(log_modifying_total_cites)
hist(log_extending_total_cites)

length(log_instantiation_total_cites)
length(log_modifying_total_cites)
length(log_extending_total_cites)

t.test(log_extending_total_cites, log_instantiation_total_cites, var.equal = FALSE)
t.test(log_modifying_total_cites, log_instantiation_total_cites, var.equal = FALSE)
t.test(log_modifying_total_cites, log_extending_total_cites, var.equal = FALSE)

a1 <- aov(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Classification..instantiation..modifying..or.extending)
posthoc <- TukeyHSD(x=a1, data$Classification..instantiation..modifying..or.extending, conf.level=0.95)

data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Total.Citations.by..ISI.Web.of.Science...SSCI. != "-")
data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Total.Citations.by..ISI.Web.of.Science...SSCI. != "")

a0 <- aov(log(data$Total.Citations.by..ISI.Web.of.Science...SSCI.) ~ data$Classification..instantiation..modifying..or.extending)
posthoc <- TukeyHSD(x=a0, 'data$Classification..instantiation..modifying..or.extending', conf.level=0.95)

## IS CITES
extending_IS_cites <- subset(data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Extending")

modifying_IS_cites <- subset(data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI.
, data$Classification..instantiation..modifying..or.extending == "Modifying")

instantiation_IS_cites <- subset(data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI.
, data$Classification..instantiation..modifying..or.extending == "Instantiation")

# Take out zeroes
instantiation_IS_cites <- subset(instantiation_IS_cites, instantiation_IS_cites != 0)
modifying_IS_cites <- subset(modifying_IS_cites, modifying_IS_cites != 0)
extending_IS_cites <- subset(extending_IS_cites, extending_IS_cites != 0)

# Non-parametric t-test
wilcox.test(extending_IS_cites, instantiation_IS_cites, probs = 0.05, alternative = c("greater"))
wilcox.test(extending_IS_cites, modifying_IS_cites, probs = 0.05, alternative = c("greater"))
wilcox.test(instantiation_IS_cites, modifying_IS_cites, probs = 0.05, alternative = c("less"))

# Transform to normal
log_instantiation_IS_cites <- log10(instantiation_IS_cites)
log_modifying_IS_cites <- log10(modifying_IS_cites)
log_extending_IS_cites <- log10(extending_IS_cites)

# or...

sqrt_instantiation_IS_cites <- sqrt(instantiation_IS_cites)
sqrt_modifying_IS_cites <- sqrt(modifying_IS_cites)
sqrt_extending_IS_cites <- sqrt(extending_IS_cites)

# Inspect visually
hist(log_instantiation_IS_cites)
hist(log_modifying_IS_cites)
hist(log_extending_IS_cites)

hist(sqrt_instantiation_IS_cites)
hist(sqrt_modifying_IS_cites)
hist(sqrt_extending_IS_cites)

# test
t.test(log_modifying_IS_cites, log_extending_IS_cites, var.equal = FALSE)

## IT Artifact Cites
# Take out duplicates
library(plyr)
data$Treatment.of.IT <- revalue(data$Treatment.of.IT, c("Ensemble "="Ensemble", "Tool "="Tool", "Computational "="Computational", "Nominal "="Nominal", "Proxy "="Proxy"))

oneway.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Treatment.of.IT)
kruskal.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Treatment.of.IT)
model <- aov(data = data, formula = Total.Citations.by..ISI.Web.of.Science...SSCI. ~ Treatment.of.IT)
TukeyHSD(model, which = "Treatment.of.IT", ordered = FALSE, conf.level = 0.95)

data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- mapvalues(data$Total.Citations.by..ISI.Web.of.Science...SSCI., NA, 1)

data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- mapvalues(data$Total.Citations.by..ISI.Web.of.Science...SSCI., 0, 1)

a1 <- aov(data$Total.Citations.by..ISI.Web.of.Science...SSCI ~ data$Classification..Exploitation.Exploration)
posthoc1 <- TukeyHSD(x=a1, 'data$Classification..Exploitation.Exploration', conf.level=0.95)

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Group"])
mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Artifact"])

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Individual"])
mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Artifact"])

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Market"])
mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Group"])

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Individual"])
mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Unit.of.Analysis=="Market"])

a2 <- aov(data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI. ~ data$Treatment.of.IT)
posthoc2 <- TukeyHSD(x=a2, 'data$Treatment.of.IT', conf.level=0.95)

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Treatment.of.IT=="Computational"])

mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Treatment.of.IT=="Proxy"])
mean(data$Total.Citations.by..ISI.Web.of.Science...SSCI[data$Treatment.of.IT=="Nominal"])


a3 <- aov(data$Total.Citations.by..ISI.Web.of.Science...SSCI ~ data$Classification..Exploitation.Exploration)
posthoc3 <- TukeyHSD(x=a3, 'data$Classification..Exploitation.Exploration', conf.level=0.95)

# Comparing power-law distributions
# http://cran.r-project.org/web/packages/poweRlaw/vignettes/compare_distributions.pdf


instantiation_total_cites[instantiation_total_cites==0] <- 1
m1 <- displ$new(instantiation_total_cites)
m1 = displ$new(instantiation_total_cites)
m1$setPars(estimate_pars(m1))

modifying_total_cites[modifying_total_cites==0] <- 1
m2 <- displ$new(modifying_total_cites)
m2 = displ$new(modifying_total_cites)
m2$setPars(estimate_pars(m2))

extending_total_cites[extending_total_cites==0] <- 1
m3 <- displ$new(extending_total_cites)
m3 = displ$new(extending_total_cites)
m3$setPars(estimate_pars(m3))

comp12 <- compare_distributions(m1, m2)
comp13 <- compare_distributions(m1, m3)
comp23 <- compare_distributions(m2, m3)

comp12$p_two_sided
comp13$p_two_sided
comp23$p_two_sided


kruskal.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Classification..instantiation..modifying..or.extending)

dunn <- dunn.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI., g=data$Classification..instantiation..modifying..or.extending)

# Analysis for exploration/exploitation
kruskal.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Classification..Exploitation.Exploration)

dunn <- dunn.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI., g=data$Classification..Exploitation.Exploration)

# Extracting alphas
exploitation_total_cites[exploitation_total_cites==0] <- 1
m1 <- displ$new(exploitation_total_cites)
m1 = displ$new(exploitation_total_cites)
m1$setPars(estimate_pars(m1))

exploration_total_cites[exploration_total_cites==0] <- 1
m2 <- displ$new(exploration_total_cites)
m2 = displ$new(exploration_total_cites)
m2$setPars(estimate_pars(m2))

m1$pars
m2$pars

# Plotting
as.data.frame(exploitation_total_cites) %>% ggvis(~exploitation_total_cites) %>% layer_histograms() %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE) %>% scale_numeric("y", domain = c(0, 25), nice = FALSE, clamp = TRUE)
as.data.frame(exploration_total_cites) %>% ggvis(~exploration_total_cites) %>% layer_histograms() %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE) %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE)

# Make boxplots
boxplot(exploitation_total_cites, ylim=c(0, 800), axes=FALSE, frame.plot=FALSE)
axis(1,at=0:2) 
axis(2,at=(0:800)*100) 

boxplot(exploration_total_cites, ylim=c(0, 800), axes=FALSE, frame.plot=FALSE)
axis(1,at=0:2) 
axis(2,at=(0:800)*100) 

# Reversing the data
library("devtools"); library(dplyr); library(tidyr); library(stringi) 

temp <- read.csv("exploration.csv", header = TRUE, fill = FALSE, fileEncoding = "latin1")
exploration_raw <- temp
temp <- temp %>% 
  gather(new.Years, X, -Year) %>%  # convert rows to one column
  mutate(Year.temp=paste0(rownames(temp), "-", Year)) %>% # concatenate the Year with row number to make them unique
  mutate(new.Years = as.numeric(gsub("X", "", new.Years)), diff = new.Years-Year+1) %>% # calculate the difference to get the yr0 yr1 and so on
  mutate(diff=paste0("yr", stri_sub(paste0("0", (ifelse(diff>0, diff, 0))), -2, -1))) %>% # convert the differences in Yr01 ...
  select(-new.Years) %>% filter(diff != "yr00") %>% # drop new.Years column
  spread(diff, X) %>%  # convert column to rows
  select(-Year.temp) # Drop Year.temp column

temp[is.na(temp)] <- 0 # replace NA with 0
exploration <- temp
write.csv(exploration, file = "exploration_reversed.csv")

temp <- read.csv("exploitation.csv", header = TRUE, fill = FALSE, fileEncoding = "latin1")
exploitation_raw <- temp
temp <- temp %>% 
  gather(new.Years, X, -Year) %>%  # convert rows to one column
  mutate(Year.temp=paste0(rownames(temp), "-", Year)) %>% # concatenate the Year with row number to make them unique
  mutate(new.Years = as.numeric(gsub("X", "", new.Years)), diff = new.Years-Year+1) %>% # calculate the difference to get the yr0 yr1 and so on
  mutate(diff=paste0("yr", stri_sub(paste0("0", (ifelse(diff>0, diff, 0))), -2, -1))) %>% # convert the differences in Yr01 ...
  select(-new.Years) %>% filter(diff != "yr00") %>% # drop new.Years column
  spread(diff, X) %>%  # convert column to rows
  select(-Year.temp) # Drop Year.temp column

temp[is.na(temp)] <- 0 # replace NA with 0
exploitation <- temp
write.csv(exploitation, file = "exploitation_reversed.csv")

# Latent growth curve modelling
# First using the raw data
library(lavaan); library(dplyr)
exploitation_growth <- exploitation_raw
exploitation_growth <- exploitation_growth[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- exploration_raw
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
exploitation_growth <- exploitation
exploitation_growth <- exploitation_growth[,2:21]
# exploitation_growth <- scale(exploitation_growth)  
colnames(exploitation_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

exploration_growth <- exploration
exploration_growth <- exploration_growth[,2:21]
# exploration_growth <- scale(exploration_growth)
colnames(exploration_growth) <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")

a.vector <- rep("Exploration", nrow(exploration_growth))
exploration_growth$type <- a.vector

b.vector <- rep("Exploitation", nrow(exploitation_growth))
exploitation_growth$type <- b.vector

growth_data <- rbind(exploration_growth, exploitation_growth)

# Multi-group Growth model
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 '
fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("lv.variances"))
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_constrained)
summary(fit_UNconstrained)

# Exploitation Model
model2 <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 '
fit_exploitation <- growth(model2, data=exploitation_growth)
summary(fit_exploitation)

# Exploration Model
model_exploration <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 '
fit_exploration <- growth(model_exploration, data=exploration_growth)
summary(fit_exploration)

# Now convert to cumulative sums for exploration
cumsum_exploration <- matrix(nrow = nrow(exploration), ncol = ncol(exploration)-1)
for (i in 1:nrow(exploration)){
  cumsum_exploration[i,] <- cumsum(as.numeric(exploration[i,2:ncol(exploration)]))
}
cumsum_exploration_df <- as.data.frame(cbind(exploration[,1], cumsum_exploration))
colnames(cumsum_exploration_df) <- colnames(exploration)

# Now convert to cumulative sums for exploitation
cumsum_exploitation <- matrix(nrow = nrow(exploitation), ncol = ncol(exploitation)-1)
for (i in 1:nrow(exploitation)){
  cumsum_exploitation[i,] <- cumsum(as.numeric(exploitation[i,2:ncol(exploitation)]))
}
cumsum_exploitation_df <- as.data.frame(cbind(exploitation[,1], cumsum_exploitation))
colnames(cumsum_exploitation_df) <- colnames(exploitation)

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
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 + 20*t21  
t1 ~~ 0.01*t1
t17 ~~ 0.01*t17
'
fit_constrained <- growth(model, data=growth_data, group = "type", group.equal = c("means"))
fit_UNconstrained <- growth(model, data=growth_data, group = "type")
summary(fit_constrained)
summary(fit_UNconstrained)

fit_constrained_variances <- growth(model, data=growth_data, group = "type", group.equal = c("residuals", "residual.covariances", "lv.variances", "lv.covariances"))
fit_constrained_variances_and_means <- growth(model, data=growth_data, group = "type", group.equal = c("intercepts", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(fit_constrained_variances)
summary(fit_constrained_variances_and_means)

# Exploitation Model
model2 <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20 + 1*t21
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 + 20*t21  '
fit_exploitation <- growth(model2, data=exploitation_growth)
summary(fit_exploitation)

# Exploration Model
model_exploration <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4 + 1*t5 + 1*t6 + 1*t7 + 1*t8 + 1*t9 + 1*t10 + 1*t11 + 1*t12 + 1*t13+ 1*t14 + 1*t15 + 1*t16 + 1*t17 + 1*t18 + 1*t19 + 1*t20 + 1*t21
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 + 4*t5 + 5*t6 + 6*t7 + 7*t8 + 8*t9 + 9*t10 + 10*t11 + 11*t12 + 12*t13 + 13*t14 + 14*t15 + 15*t16 + 16*t17 + 17*t18 + 18*t19 + 19*t20 + 1*t21 '
fit_exploration <- growth(model_exploration, data=exploration_growth)
summary(fit_exploration)

# BACKUP yearly model
model_exploration <- ' i =~ 1*t1995 + 1*t1996 + 1*t1997 + 1*t1998 + 1*t1999 + 1*t2000 + 1*t2001 + 1*t2002 + 1*t2003 + 1*t2004 + 1*t2005 + 1*t2006 + 1*t2007+ 1*t2008 + 1*t2009 + 1*t2010 + 1*t2011 + 1*t2012 + 1*t2013 + 1*t2014
           s =~ 0*t1995 + 1*t1996 + 2*t1997 + 3*t1998 + 4*t1999 + 5*t2000 + 6*t2001 + 7*t2002 + 8*t2003 + 9*t2004 + 10*t2005 + 11*t2006 + 12*t2007 + 13*t2008 + 14*t2009 + 15*t2010 + 16*t2011 + 17*t2012 + 18*t2013 + 19*t2014 '
fit_exploration <- growth(model_exploration, data=exploration_growth)
summary(fit_exploration)
