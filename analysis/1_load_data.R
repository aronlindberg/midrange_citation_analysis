# First we load the data for the mean-comparisons
data <- read.csv("middle_range_coding_v4.csv", header = TRUE, fill = FALSE, fileEncoding = "latin1")

data$Classification..instantiation..modifying..or.extending
data$Total.Citations.by..ISI.Web.of.Science...SSCI. 

str(data$Classification..instantiation..modifying..or.extending)

str(data$Total.Citations.by..ISI.Web.of.Science...SSCI.)

## Crunch data for total cites
extending_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Extending")

modifying_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Modifying")

instantiation_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..instantiation..modifying..or.extending == "Instantiation")

exploitation_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..Exploitation.Exploration == "Exploitation")

exploration_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$Classification..Exploitation.Exploration == "Exploration")


## Take out zeroes
instantiation_total_cites <- subset(instantiation_total_cites, instantiation_total_cites != 0)
modifying_total_cites <- subset(modifying_total_cites, modifying_total_cites != 0)
extending_total_cites <- subset(extending_total_cites, extending_total_cites != 0)

# Load and reverse data for latent growth curve modeling
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