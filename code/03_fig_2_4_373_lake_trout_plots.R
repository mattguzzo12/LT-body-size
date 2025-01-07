# source trend code
source("code/02_fig_2-4_373_lake_trout_trends.R")

# load packages
library(ggridges)
library(cowplot)
library(ggiraphExtra)

###################################
# mean plots
###################################

# fork length slope
mod_mean_fl = dat_trend %>%
  filter(trait == "fl_mean") %>%
  mblm(value ~ Year, data = .)
  
# mean fl plot
fl_mean_plot =
  ggplot(dat_s, aes(x = Year, y = fl_mean))+
  scale_y_continuous(limits = c(275, 550))+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_pointrange(aes(ymin = fl_mean - fl_sd, ymax = fl_mean + fl_sd), size = 1)+
  geom_line(aes(x = Year, y = predict(mod_mean_fl)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  theme_bw(base_size = 18)+
  ylab("Fork length (mm)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# weight slopes
mod_mean_weight = dat_trend %>%
  filter(trait == "mass_mean") %>%
  mblm(value ~ Year, data = .)

# mean weight plot
weight_mean_plot =
  ggplot(dat_s, aes(x = Year, y = mass_mean))+
  scale_y_continuous(limits = c(250, 2000))+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_pointrange(aes(ymin = mass_mean - mass_sd, ymax = mass_mean + mass_sd), size = 1)+
  geom_line(aes(x = Year, y = predict(mod_mean_weight)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  theme_bw(base_size = 18)+
  ylab("Weight (g)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# condition (relative weight) slope
mod_mean_rw = dat_trend %>%
  filter(trait == "rw_mean") %>%
  mblm(value ~ Year, data = .)

# mean condition plot
rw_mean_plot =
  ggplot(dat_s, aes(x = Year, y = rw_mean))+
  scale_y_continuous(limits=c(60, 115))+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_pointrange(aes(ymin = rw_mean - rw_sd, ymax = rw_mean + rw_sd), size = 1)+
  geom_line(aes(x = Year, y = predict(mod_mean_rw)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  theme_bw(base_size = 18)+
  ylab("Relative weight (%)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# age slopes
mod_mean_age = dat_trend %>%
  filter(trait == "age_mean") %>%
  mblm(value ~ Year, data = .)

# mean age plot
age_mean_plot = 
  ggplot(dat_s, aes(x = Year, y = age_mean))+
  scale_y_continuous(limits=c(0, 23))+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_pointrange(aes(ymin = age_mean - age_sd, ymax = age_mean + age_sd), size = 1)+
  geom_line(aes(x = Year, y = predict(mod_mean_age)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  theme_bw(base_size = 18)+
  ylab("Age (years)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# mean panel plot
mean_panel = plot_grid(fl_mean_plot, weight_mean_plot, age_mean_plot, rw_mean_plot, 
                       labels = c("A", "B", "C", "D"), label_size = 20, align = "hv")
mean_panel

save_plot("figs/mean_trend_panel.tiff", mean_panel, ncol = 2, compression = "lzw", base_height = 9.5, base_width = 6, dpi = 500)


##########################################
# quantile plots, aka life history traits
##########################################

# 20th quantile age
mod_age_q20 = dat_trend %>%
  filter(trait == "age_q20") %>%
  mblm(value ~ Year, data = .)

q20_age = 
ggplot(dat_s, aes(x = Year, y = age_q20))+
  scale_y_continuous(limits=c(3, 10.5))+
  geom_line(aes(x = Year, y = predict(mod_age_q20)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size = 4)+
  theme_bw(base_size = 18)+
  ylab("Age-at-maturity (years)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# 95th quantile age
mod_age_q95 = dat_trend %>%
  filter(trait == "age_q95") %>%
  mblm(value ~ Year, data = .)

q95_age = 
ggplot(dat_s, aes(x = Year, y = age_q95))+
  scale_y_continuous(limits=c(10, 29))+
  geom_line(aes(x = Year, y = predict(mod_age_q95)), linewidth = 1) +
  #geom_smooth(se = F, colour = "black", method = "lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size = 4)+
  theme_bw(base_size = 18)+
  ylab("Lifespan (years)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# 20th quantile fork length 
mod_fl_q20 = dat_trend %>%
  filter(trait == "fl_q20") %>%
  mblm(value ~ Year, data = .)

q20_fl = 
ggplot(dat_s, aes(x = Year, y = fl_q20))+
  scale_y_continuous(limits=c(320, 475))+
  geom_line(aes(x = Year, y = predict(mod_fl_q20)), linewidth = 1) +
  #geom_smooth(se=F, colour="black", method="lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size=4)+
  theme_bw(base_size=18)+
  ylab("Fork length-at-maturity (mm)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# 95th quantile fork length
mod_fl_q95 = dat_trend %>%
  filter(trait == "fl_q95") %>%
  mblm(value ~ Year, data = .)

q95_fl =
ggplot(dat_s, aes(x = Year, y = fl_q95))+
  scale_y_continuous(limits=c(400, 540))+
  geom_line(aes(x = Year, y = predict(mod_fl_q95)), linewidth = 1) +
  #geom_smooth(se=F, colour="black", method="lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size=4)+
  theme_bw(base_size=18)+
  ylab("Maximum fork length (mm)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# 20th quantile weight
mod_mass_q20 = dat_trend %>%
  filter(trait == "mass_q20") %>%
  mblm(value ~ Year, data = .)

q20_mass = 
ggplot(dat_s, aes(x = Year, y = mass_q20))+
  scale_y_continuous(limits=c(300, 1300))+
  geom_line(aes(x = Year, y = predict(mod_mass_q20)), linewidth = 1) +
  #geom_smooth(se=F, colour="black", method="lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size=4)+
  theme_bw(base_size=18)+
  ylab("Weight-at-maturity (g)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# 95th quantile weight
mod_mass_q95 = dat_trend %>%
  filter(trait == "mass_q95") %>%
  mblm(value ~ Year, data = .)

q95_mass = 
ggplot(dat_s, aes(x = Year, y = mass_q95))+
  scale_y_continuous(limits=c(600, 2100))+
  geom_line(aes(x = Year, y = predict(mod_mass_q95)), linewidth = 1) +
  #geom_smooth(se=F, colour="black", method="lm")+
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10), se = F)+
  scale_x_continuous(breaks = c(seq(1986, 2016, 5)))+
  geom_point(size=4)+
  theme_bw(base_size=18)+
  ylab("Maximum weight (mm)")+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

quantile_panel = plot_grid(q20_fl, q95_fl, q20_mass, q95_mass, q20_age, q95_age, 
                       labels = c("A", "B", "C", "D", "E", "F"), label_size = 20, ncol = 2, align = "hv")
quantile_panel
save_plot("figs/quantile_trend_panel.tiff", quantile_panel, ncol = 2, compression = "lzw", base_height = 13.5, base_width = 6, dpi = 500)


############################################
# size at age specific effect size plots
############################################

# fork length at age

ef_fl <-
  ggplot(f_res, aes(y = slope, x = age, colour = sig))+
  geom_hline(yintercept=0)+
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd=1.5, colour = "black")+
  geom_point(size = 5, colour = "black")+
  geom_point(size = 3)+
  scale_colour_manual(values = c("white", "black", "grey60"))+
  scale_y_continuous(limits=c(-3, 3))+
  scale_x_continuous(breaks = (2:21), labels = c(2:20, "  21+"))+
  theme_bw(base_size = 16)+
  ylab("Fork length slope (mm year-1)")+
  xlab("Age (years)")+
  guides(colour = F)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# weight at age

ef_w <-
  ggplot(w_res, aes(y = slope, x = age, colour = sig))+
  geom_hline(yintercept=0)+
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd=1, colour = "black")+
  geom_point(size = 5, colour = "black")+
  geom_point(size = 3)+
  scale_colour_manual(values = c("white", "black", "grey60"))+
  scale_y_continuous(limits=c(-30, 30))+
  scale_x_continuous(breaks = (2:21), labels = c(2:20, "  21+"))+
  theme_bw(base_size = 16)+
  ylab("Weight slope (g year-1)")+
  xlab("Age (years)")+
  guides(colour = F)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))

# condition (relative weight) at age

ef_c <-
  ggplot(c_res, aes(y = slope, x = age, colour = sig))+
  geom_hline(yintercept=0)+
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, lwd=1, colour = "black")+
  geom_point(size = 5, colour = "black")+
  geom_point(size = 3)+
  scale_colour_manual(values = c("white", "black", "grey60"))+
  scale_x_continuous(breaks = (2:21), labels = c(2:20, "  21+"))+
  scale_y_continuous(limits=c(-1.3, 1.3))+
  theme_bw(base_size = 16)+
  ylab("Relative weight (% year-1)")+
  xlab("Age (years)")+
  guides(colour = F)+
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))


effect_plot = plot_grid(ef_fl, ef_w, ef_c, ncol = 1, labels=c("A", "B", "C"), label_size=20, align = "hv")
effect_plot

save_plot("figs/size_at_age_effects_panel.tiff", effect_plot, ncol = 1, compression = "lzw", base_height = 13.5, base_width = 6, dpi = 500)


            