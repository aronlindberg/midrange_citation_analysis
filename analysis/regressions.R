# Compute additional variables
data$h1_initial_2013_average_sum <- (data$h1_2013_sum+data$h1_initial_sum)/2

h1_2013_max_author <- vector(length = nrow(data), mode = "integer")
for (i in 1:length(h1_2013_max_author)){
  row_to_evaluate <- c(data$h1_2013_author_1[i], data$h1_2013_author_2[i], data$h1_2013_author_3[i], data$h1_2013_author_4[i], data$h1_2013_author_5[i])
  h1_2013_max_author[i] <- max(row_to_evaluate, na.rm = TRUE)
}

data$h1_2013_max_author <- h1_2013_max_author

data$outside_IS_citations <- data$Total.Citations.by..ISI.Web.of.Science...SSCI.-data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI.

data$only_exploration_exploitation <- gsub(pattern = "-", data$X4_Classification..Exploitation.Exploration, replacement = NA) 

# Reducing factor levels
# levels(data$X1_Classification..Exploitation.Exploration)[1] <- NA
# levels(data$X2_Classification..Exploitation.Exploration)[1] <- NA
# levels(data$X3_Classification..Exploitation.Exploration)[1] <- NA
# levels(data$X4_Classification..Exploitation.Exploration)[1] <- NA
# levels(data$Classification..instantiation..modifying..or.extending)[1] <- NA
# levels(data$Magnitude..none..small..medium..large)[1:2] <- NA


# Mean differences ANCOVA for average citations and proportion of within-IS citations
wilcox.test(exploitation_total_cites, exploration_total_cites)

predict_total_citations_model <- lm(log(data$Total.Cites...last.10.years+1) ~ data$X4_Classification..Exploitation.Exploration + log(data$h1_initial_2013_average_sum) + data$Age.since.2013)
predict_IS_citations_model <- lm(log(data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI.+1) ~ data$X4_Classification..Exploitation.Exploration + log(data$h1_initial_2013_average_sum) + data$Age.since.2013)
predict_outside_IS_citations <- lm(log(data$outside_IS_citations+1) ~ data$X4_Classification..Exploitation.Exploration + log(data$h1_initial_2013_average_sum) + data$Age.since.2013)

predict_IS_proportion_model <- lm(log(data$X..of.citations.in.IS.domain+1) ~ data$X4_Classification..Exploitation.Exploration + log(data$h1_initial_2013_average_sum) + data$Age.since.2013)
predict_halflife <- lm(log(data$Half.life...duration..compared.to.2013.+2) ~ data$X4_Classification..Exploitation.Exploration + log(data$h1_initial_2013_average_sum) + data$Age.since.2013)

summary(predict_total_citations_model)
summary.lm(predict_IS_citations_model)
summary(predict_outside_IS_citations)
summary.lm(predict_IS_proportion_model)
summary.lm(predict_halflife)

# Output
library(stargazer)
stargazer(predict_total_citations_model, predict_IS_citations_model, predict_outside_IS_citations, predict_IS_proportion_model, predict_halflife, type = "html")

# Correlation matrix
library(ltm)
cor(data.frame(total_citations=data$Total.Citations.by..ISI.Web.of.Science...SSCI., IS_citations=data$Information.Science...Library.Science.Citations..ISI.Web.of.Science...SSCI., outside_IS_citations=data$outside_IS_citations, proportion_IS_citations=as.numeric(data$X..of.citations.in.IS.domain), half_life=as.numeric(data$Half.life...duration..compared.to.2013), age=data$Age.since.2013, h_index=data$h1_initial_2013_average_sum), use = "complete")

biserial.cor(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$X4_Classification..Exploitation.Exploration)