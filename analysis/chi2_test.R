exploitation <- c(18, 14, 60, 38, 26)
exploration <- c(2, 13, 12, 14, 15)
data <- matrix(c(exploitation, exploration), ncol = 2, nrow=5)
data <- matrix(data)
chisq.test(data)

(exploitation/sum(exploitation))*100
(exploration/sum(exploration))*100
