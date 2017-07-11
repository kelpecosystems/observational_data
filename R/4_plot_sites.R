library(tidyverse)
library(ggmap)


keen_map <- get_map(c(min(sites$START_LONGITUDE, na.rm=TRUE), 
                      min(sites$START_LATITUDE, na.rm=TRUE)),
                    zoom=7)

sites_summary <- sites %>%
  group_by(PI, SITE) %>%
  summarise(START_LONGITUDE = mean(START_LONGITUDE, na.rm=T),
            START_LATITUDE = mean(START_LATITUDE, na.rm=T),
            n_years = length(unique(YEAR))
  )



keen_map <- get_map(c(min(sites_summary$START_LONGITUDE), 
                      min(sites_summary$START_LATITUDE)),
                    zoom=7)

ggmap(keen_map) +
  geom_point(data=sites_summary, mapping=aes(x=START_LONGITUDE,
                                     y = START_LATITUDE,
                                     group=SITE),
             size=3) +
  xlim(c(-72, -69)) + ylim(c(41,43.9)) +
  theme_nothing() 



##### Spatially grouped
sites_summary_grouped <- sites_summary %>%
  mutate(lat_small = round(START_LATITUDE,1),
         lon_small = round(START_LONGITUDE,1),
  ) %>%
  group_by(lat_small, lon_small) %>%
  summarise(START_LONGITUDE = mean(START_LONGITUDE, na.rm=T),
            START_LATITUDE = mean(START_LATITUDE, na.rm=T),
            n_years = max(n_years),
            n_sites = length(unique(SITE))
  )

ggmap(keen_map) +
  geom_point(data=sites_summary_grouped, mapping=aes(x=START_LONGITUDE,
                                                     y = START_LATITUDE,
                                                     size=factor(n_sites))) +
  xlim(c(-72, -69)) + ylim(c(41,43.9)) +
  xlab("") + ylab("") +
  theme_bw() +
  scale_size_manual(values=c(2,4,8), guide=guide_legend(title="# Sites"))
