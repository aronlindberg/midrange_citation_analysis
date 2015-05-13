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
data$Treatment.of.IT <- revalue(data$Treatment.of.IT, c("Ensemble "="Ensemble", "Tool "="Tool", "Computational "="Computational", "Nominal "="Nominal", "Proxy "="Proxy"))

oneway.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Treatment.of.IT)
kruskal.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$Treatment.of.IT)
TukeyHSD(data$Total.Citations.by..ISI.Web.of.Science...SSCI.,  "Treatment.of.IT", ordered = FALSE, conf.level = 0.95)

data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- mapvalues(data$Total.Citations.by..ISI.Web.of.Science...SSCI., NA, 1)

data$Total.Citations.by..ISI.Web.of.Science...SSCI. <- mapvalues(data$Total.Citations.by..ISI.Web.of.Science...SSCI., 0, 1)

a1 <- aov(log(data$Total.Citations.by..ISI.Web.of.Science...SSCI.) ~ data$Treatment.of.IT)
posthoc <- TukeyHSD(x=a1, 'data$Treatment.of.IT', conf.level=0.95)