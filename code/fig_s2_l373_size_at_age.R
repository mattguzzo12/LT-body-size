# load packages
library(tidyverse)
library(cowplot)

# read in data, format data, filter out spring captures
l373 <- read.csv("data/373_LT_DB_2016.csv") %>% 
  mutate(DATE_CATCH = as.POSIXct(DATE_CATCH, format="%m/%d/%Y")) %>%
  mutate(DATE_CATCH = as.Date(DATE_CATCH)) %>%
  mutate(Year = year(DATE_CATCH), Month = month(DATE_CATCH), taxon = as.character(taxon)) %>%
  filter(taxon == "F081") %>% # keep only lake trout
  filter(!Month < 7) %>% # remove spring data
  filter(!Year == 1982) %>% # remove 1982 data - small sample and was exploratory gill netting
  mutate(Ws = 10^(-5.681 + 3.2462*(log10(TOTAL_LENGTH)))) %>% # calculates standard weight for each fish based on its FL
  mutate(rW = WEIGHT/Ws*100) # calculates relative weight for each fish


# plot fork length at age for ages <=4
a=
  l373 %>%
  filter(AGE <= 4) %>%
  ggplot(aes(x = Year, y = FORK_LENGTH, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Fork length (mm)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 

# plot fork length at age for ages >4 and <11 
b=
  l373 %>%
  filter(AGE > 4 & AGE < 11) %>%
  ggplot(aes(x = Year, y = FORK_LENGTH, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "grey", high = "black")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  #scale_y_continuous(limits = c(230, 450)) +
  ylab("Fork length (mm)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 

# plot mass at age for ages <=4
c=
  l373 %>%
  filter(AGE <= 4) %>%
  ggplot(aes(x = Year, y = WEIGHT, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Mass (g)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 

# plot mass at age for ages >4 and <11 
d=
  l373 %>%
  filter(AGE > 4 & AGE < 11) %>%
  ggplot(aes(x = Year, y = WEIGHT, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "grey", high = "black")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  #scale_y_continuous(limits = c(230, 450)) +
  ylab("Mass (g)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 

# plot condition (relative weight) at age for ages <=4  
e=
  l373 %>%
  filter(AGE <= 4) %>%
  ggplot(aes(x = Year, y = rW, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Condition (%)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 


# plot condition (relative weight) at age for ages >4 and <11   
f=
  l373 %>%
  filter(AGE > 4 & AGE < 11) %>%
  mutate(rW = WEIGHT/Ws*100) %>%
  ggplot(aes(x = Year, y = rW, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "grey", high = "black")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Condition (%)")+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(50, 120)) +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

plot_grid(a, b, c, d, e, f, ncol=2, labels=c("A", "D", "B", "E", "C", "F"), 
          label_size=22)
# save  plot
ggsave("figs/fig_s2_l373_saa.tiff", width=12, height=17, units="in", dpi=500)

