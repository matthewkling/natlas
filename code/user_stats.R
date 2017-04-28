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

# assign obs to geographic bins (to address user cosmopolitanism)
yrange <- range(d$decimalLatitude)
xrange <- range(d$decimalLongitude)
span <- max(diff(xrange), diff(yrange))
resolution <- span / 5
ra <- plyr::round_any
d <- mutate(d,
            xbin = as.integer(factor(ra(decimalLongitude, resolution, floor))),
            ybin = as.integer(factor(ra(decimalLatitude, resolution, floor))),
            geo_bin = paste(xbin, ybin))

# compute user statistics and add to table
# each user scored from 1-10 on # obs, spp, and visits
jitter <- .01 # add a bit of noise to reduce overplotting
u <- d %>%
      group_by(userNumber) %>%
      summarize(obs = n() + runif(1,0,jitter),
                spp = length(unique(species)) + runif(1,0,jitter),
                visits = length(unique(paste(year, month))) + runif(1,0,jitter),
                locations = length(unique(geo_bin)) + runif(1,0,jitter)) %>%
      mutate(obs = ecdf(obs)(obs),
             spp = ecdf(spp)(spp),
             visits = ecdf(visits)(visits),
             locations = ecdf(locations)(locations)) %>%
      mutate(user_obs = rescale(obs),
             user_spp = rescale(spp),
             user_visits = rescale(visits),
             user_locations = rescale(locations),
             user_hits = rescale(user_visits + user_locations)) %>%
      select(userNumber, user_obs:user_locations)
d <- left_join(d, u)

write.csv(d, "d3_master/PORE_obs_tidy.csv", row.names=F)

