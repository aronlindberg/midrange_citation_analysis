setwd("/Users/aron/git/midrange_citation_analysis/")

# This loads all the packages which are required for all other scripts
required_packages <- c('gdata','poweRlaw','dunn.test', 'plyr', 'devtools', 'dplyr', 'tidyr', 'stringi', 'lavaan')
new_packages <- required_packages[!(required_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only=T)

# Run scripts
script_list <- list("load_data.R", "power_laws.R", "regressions.R", "growth_modeling.R")
for (i in 1:length(script_list)){
  try(source(script_list[[i]]))
}