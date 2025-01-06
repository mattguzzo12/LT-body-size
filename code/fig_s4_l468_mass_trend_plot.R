# load packages
library(tidyverse)
library(Kendall)
library(mblm)

# read in data and filter to include post 1980
rod = read.csv("data/468laker.csv") %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    u95 = quantile(WEIGHT, probs = 0.95, na.rm = T)
    ) %>%
  filter(Year>=1980)

# test for changes in long term trends in mean values over time
MannKendall(as.ts(rod$u95))
# siegal repeated median slope
mod = mblm(u95 ~ Year, data = rod)
summary(mod)
# predict for trend line
rod$pred = predict(mod, rod)
  
# plot
ggplot(rod, aes(x = Year, y = u95)) +
  geom_point(size = 4) +
  geom_line(aes(x = Year, y = pred), linewidth = 1.2) +
  theme_bw(base_size=18)+
  ylab("Upper 5th percentile mass (g)")+
  scale_y_continuous(limits = c(800, 1800)) +
  scale_x_continuous(breaks = c(1980, 1984, 1988, 1992, 1996, 2000, 2004)) +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) 

# save figure 
ggsave("figs/fig_s4_l468_max_mass.tiff", width=7, height=6, units="in", dpi=500)
