# load packages
library(tidyverse)
library(cowplot)
library(readxl)

# read in data, filter out spring captures
l224 = read_excel("data/Lake 224 LT to 2016.xlsx") %>%
  filter(SESN == "2F")

# plot fork length at age for ages <=4
a=
  l224 %>%
  filter(AGE <= 4) %>%
  ggplot(aes(x = Year, y = FORK_LENGTH, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Fork length (mm)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# plot fork length at age for ages >4 and <11 
b=
l224 %>%
  filter(AGE > 4 & AGE < 11) %>%
  ggplot(aes(x = Year, y = FORK_LENGTH, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "grey", high = "black")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  scale_y_continuous(limits = c(230, 450)) +
  ylab("Fork length (mm)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# plot mass at age for ages <=4
c=
l224 %>%
  filter(AGE <= 4) %>%
  ggplot(aes(x = Year, y = WEIGHT, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Mass (g)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# plot mass at age for ages >4 and <11 
d=
  l224 %>%
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
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# plot condition (relative weight) at age for ages <=4  
e=
l224 %>%
  filter(AGE <= 4) %>%
  mutate(Ws = 10^(-5.681 + 3.2462*(log10(TOTAL_LENGTH)))) %>% # calculates standard weight for each fish based on its FL
  mutate(rW = WEIGHT/Ws*100) %>% # calculates relative weight for each fish
  ggplot(aes(x = Year, y = rW, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "light blue", high = "blue")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Condition (%)")+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# plot condition (relative weight) at age for ages >4 and <11   
f=
l224 %>%
  filter(AGE > 4 & AGE < 11) %>%
  mutate(Ws = 10^(-5.681 + 3.2462*(log10(TOTAL_LENGTH)))) %>% # calculates standard weight for each fish based on its FL
  mutate(rW = WEIGHT/Ws*100) %>%
  filter(!rW > 500) %>%# calculates relative weight for each fish
  ggplot(aes(x = Year, y = rW, group = AGE, colour = AGE)) +
  scale_colour_continuous(low = "grey", high = "black")+
  geom_point() +
  geom_smooth(span = 1, se = F) +
  theme_bw(base_size=18)+
  ylab("Condition (%)")+
  theme(legend.position="top")+
  scale_y_continuous(limits = c(50, 120)) +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) + 
  geom_vline(xintercept = 2000)

# make panel plot  
plot_grid(a, b, c, d, e, f, ncol=2, labels=c("A", "D", "B", "E", "C", "F"), 
          label_size=22)
# save  plot
ggsave("figs/fig_s3_l224_saa.tiff", width=12, height=17, units="in", dpi=500)

