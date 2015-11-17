# First we load the data for the mean-comparisons
data <- read.csv(paste0(getwd(), "/data/data_w_h1_indices.csv"), header = TRUE, fill = FALSE, fileEncoding = "latin1")

## Crunch data for total cites
exploitation_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$X2_Classification..Exploitation.Exploration == "Exploitation")
exploration_total_cites <- subset(data$Total.Citations.by..ISI.Web.of.Science...SSCI., data$X2_Classification..Exploitation.Exploration == "Exploration")

# Load and reverse data for latent growth curve modeling for exploration/exploitation (crude parsing)
temp <- cbind(data$Year[data$X4_Classification..Exploitation.Exploration=="Exploration"], data[data$X4_Classification..Exploitation.Exploration=="Exploration",][,39:58])
colnames(temp)[1] <- "Year"
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

temp <- cbind(data$Year[data$X4_Classification..Exploitation.Exploration=="Exploitation"], data[data$X4_Classification..Exploitation.Exploration=="Exploitation",][,39:58])
colnames(temp)[1] <- "Year"
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