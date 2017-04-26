library(dplyr)
library(scales)

#setwd("..")
setwd("~/documents/inviz/taxorama")

# load data
park_id = "PORE" #Point Reyes is park of interest

# load observations data
d <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv"), header=T, stringsAsFactors=F) %>%
      mutate(species=speciesFixed,
             category=tolower(category),
             root="life",
             user=userNumber)

# compute user statistics and add to table
# each user scored from 1-10 on # obs, spp, and visits
u <- d %>%
      group_by(userNumber) %>%
      summarize(obs = n(),
                spp = length(unique(species)),
                visits = length(unique(paste(year, month)))) %>%
      mutate(obs = ecdf(obs)(obs),
             spp = ecdf(spp)(spp),
             visits = ecdf(visits)(visits)) %>%
      mutate(user_obs = rescale(obs),
             user_spp = rescale(spp),
             user_visits = rescale(visits)) %>%
      select(userNumber, user_obs, user_spp, user_visits)
d <- left_join(d, u)

write.csv(d, "d3_master/PORE_obs_tidy.csv", row.names=F)

