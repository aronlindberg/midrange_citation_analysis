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
kruskal.test(data$Total.Citations.by..ISI.Web.of.Science...SSCI. ~ data$X4_Classification..Exploitation.Exploration)

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


# Extracting alphas adjusted for h1_index
exploitation_total_cites_h1_2013_sum_adjusted <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$X4_Classification..Exploitation.Exploration == "Exploitation")/subset(data$h1_2013_sum, data$X4_Classification..Exploitation.Exploration == "Exploitation")

exploration_total_cites_h1_2013_sum_adjusted <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$X4_Classification..Exploitation.Exploration == "Exploration")/subset(data$h1_2013_sum, data$X4_Classification..Exploitation.Exploration == "Exploration")
  
  
exploitation_total_cites_h1_2013_sum_adjusted[exploitation_total_cites_h1_2013_sum_adjusted==0] <- 1
m1 <- conpl$new(exploitation_total_cites_h1_2013_sum_adjusted)
m1$setPars(estimate_pars(m1))

exploration_total_cites_h1_2013_sum_adjusted[exploration_total_cites_h1_2013_sum_adjusted==0] <- 1
m2 <- conpl$new(exploration_total_cites_h1_2013_sum_adjusted)
m2$setPars(estimate_pars(m2))

m1$pars
m2$pars

library(moments)
skewness(exploitation_total_cites_h1_2013_sum_adjusted)
skewness(exploration_total_cites_h1_2013_sum_adjusted)

# Plotting
library(ggvis)
as.data.frame(exploitation_total_cites) %>% ggvis(~exploitation_total_cites) %>% layer_histograms() %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE) %>% scale_numeric("y", domain = c(0, 25), nice = FALSE, clamp = TRUE)
as.data.frame(exploration_total_cites) %>% ggvis(~exploration_total_cites) %>% layer_histograms() %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE) %>% scale_numeric("x", domain = c(0, 800), nice = FALSE, clamp = TRUE)

# Make boxplots
boxplot(exploitation_total_cites, ylim=c(0, 800), axes=FALSE, frame.plot=FALSE)
axis(1,at=0:2) 
axis(2,at=(0:800)*100) 

boxplot(exploration_total_cites, ylim=c(0, 800), axes=FALSE, frame.plot=FALSE)
axis(1,at=0:2) 
axis(2,at=(0:800)*100)