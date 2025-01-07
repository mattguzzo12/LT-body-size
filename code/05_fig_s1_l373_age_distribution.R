library(tidyverse)
library(cowplot)

# read in L373 lake trout data 1986-2016
l373 <- read.csv("data/373_LT_DB_2016.csv") %>% 
  mutate(DATE_CATCH = as.POSIXct(DATE_CATCH, format="%m/%d/%Y")) %>%
  mutate(DATE_CATCH = as.Date(DATE_CATCH)) %>%
  mutate(Year = year(DATE_CATCH), Month = month(DATE_CATCH), taxon = as.character(taxon)) %>%
  filter(taxon == "F081") %>% # keep only lake trout
  filter(!Month < 7) %>% # remove spring data
  filter(!Year == 1982) %>% # remove 1982 data - small sample and was exploratory gill netting
  mutate(Ws = 10^(-5.681 + 3.2462*(log10(TOTAL_LENGTH)))) %>% # calculates standard weight for each fish based on its FL
  mutate(rW = WEIGHT/Ws*100) # calculates relative weight for each fish

# plot histogram of ages
l373 %>%
  group_by(Year) %>%
  ggplot(aes(x = AGE, group = Year)) +
  geom_histogram() +
  theme_bw(base_size=18) +
  ylab("Number of fish") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  facet_wrap(~Year, ncol = 4)

# save plot
ggsave("figs/fig_s1_l373_age_dist.tiff", width=12, height=17, units="in", dpi=500)
