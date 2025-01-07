source("code/02_fig_2-4_373_lake_trout_trends.R")

prod <- read.csv("data/L373LakeTroutIGRProductionEsts.csv")

abun <- read.csv("data/373_fall_markrecap_rennie.csv")
colnames(abun)[6] <- "Year"

abun <- merge(abun, dat_s, by = "Year")

abun$biomass <- (abun$n * (abun$mass_mean/1000))/27.3
abun$biomass_lcl <- (abun$lcl * (abun$mass_mean/1000))/27.3
abun$biomass_ucl <- (abun$ucl * (abun$mass_mean/1000))/27.3

mk_abun <- MannKendall(as.ts(abun$n)) 
summary(mk_abun)

sens_abun <- mblm(n ~ Year, data = abun) 
summary(sens_abun)

#library(modifiedmk)
#bbsmk(abun$n, ci=0.95, nsim=2000, eta=1, bl.len=NULL)

a =
ggplot(abun, aes(x = Year, y = n)) +
  scale_x_continuous(breaks = c(seq(1986, 2016, 4)))+
  geom_pointrange(aes(ymin = lcl, ymax = ucl), size = 1)+
  #geom_smooth(se = F, method = "lm", colour = "black") +
  theme_bw(base_size = 18)+
  ylab("Abundance")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

mk_biomass <- MannKendall(as.ts(abun$biomass)) 
summary(mk_biomass)

sens_biomass <- mblm(biomass ~ Year, data = abun) 
summary(sens_biomass)

b =
ggplot(abun, aes(x = Year, y = biomass)) +
  scale_x_continuous(breaks = c(seq(1986, 2016, 4)))+
  geom_pointrange(aes(ymin = biomass_lcl, ymax = biomass_ucl), size = 1)+
  theme_bw(base_size = 18)+
  ylab("Biomass (kg ha-1)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

mk_prod <- MannKendall(as.ts(prod$ProdEst)) 
summary(mk_prod)

c =
  ggplot(prod, aes(x = Year, y = ProdEst)) +
  scale_x_continuous(breaks = c(seq(1986, 2016, 4)))+
  geom_pointrange(aes(ymin = ProdEst - SDProd*1.96, ymax = ProdEst + SDProd*1.96), size = 1)+
  theme_bw(base_size = 18)+
  ylab("Productivity (kg ha-1 year-1)")+
  scale_y_continuous(limits = c(-0.9, 1.8)) + 
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

abun <- merge(abun, prod, by = "Year", all.x = T)
abun$pb = abun$ProdEst/abun$biomass

mk_pb <- MannKendall(as.ts(abun$pb)) 
summary(mk_pb)

max(abun$pb, na.rm = T)

d =
  ggplot(abun, aes(x = Year, y = pb)) +
  scale_x_continuous(breaks = c(seq(1986, 2016, 4)))+
  geom_point(size = 4) + 
  #geom_pointrange(aes(ymin = ProdEst - SDProd*1.96, ymax = ProdEst + SDProd*1.96), size = 1)+
  theme_bw(base_size = 18)+
  ylab("P/B")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))
d

library(cowplot)

abun_biomass_plot <- plot_grid(a, b, c, d, ncol = 2, labels = c("A", "B", "C", "D"), align = "vh", label_size = 20)
abun_biomass_plot

save_plot("figs/abun_biomass_prod_plot.pdf", abun_biomass_plot, ncol = 1, base_height = 10.5, base_width = 13, dpi = 500)
