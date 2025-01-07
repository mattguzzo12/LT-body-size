# load packages
library(tidyverse)
library(forecast)
library(Hmisc)
library(readxl)
library(Kendall)
library(broom)
library(mblm)
library(bbmle)

###########################################################################
# mean annual air temp
###########################################################################

air_temp = read.csv("data/mean_annual_air_temps.csv")

# filter out 1969 partial year calculate annual means for 1970 on
air_temp1 = air_temp %>% 
  filter(!year == 1969) %>% 
  group_by(year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = T)) 

# calculate means for 1986 ona
air_temp2 = 
  air_temp %>% 
  filter(!year < 1986) %>% 
  group_by(year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = T)) 

# mean air temp trend tests for each time period above
tidy(MannKendall(as.ts(air_temp1$mean_temp)))
tidy(MannKendall(as.ts(air_temp2$mean_temp)))

# obtain siegal slopes 1970 on
mod = mblm(mean_temp ~ year, data = air_temp1)
summary(mod)

# obtain siegal slopes 1986 on 
mod1 = mblm(mean_temp ~ year, data = air_temp2)
summary(mod1)

# obtain siegal confidence intervals for each time period
confint(mblm(mean_temp ~ year, data = air_temp1))
confint(mblm(mean_temp ~ year, data = air_temp2))

# make a new prediction dataframe for each time period
new = data.frame(year = c(1970, 2016))
new1 = data.frame(year = c(1986, 2016))

# predict for each time period
air_temp1$predict = predict(mod)
air_temp2$predict = predict(mod1)

# mean annual temp plot
temp_plot =
air_temp1 %>% filter(!year == 1969) %>% 
  group_by(year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = T)) %>%
  ggplot(aes(x = year, y = mean_temp))+
  geom_point(size = 4)+
  geom_line()+
  geom_line(data = air_temp1, aes(x = year, y = predict), colour = "black") +
  theme_bw(base_size = 16)+
  xlab("Year")+
  ylab("Air temperature (˚C)")+
  scale_x_continuous(breaks = seq(1970, 2016, 4))+
  scale_y_continuous(limits = c(0, 6))+
  guides(fill = "none")+
  annotate("rect", xmin = 1986, xmax = 2016, ymin = 0, ymax = 6,
           alpha = .1,fill = "grey40") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black'))
temp_plot


#################################################################################
#ice on and off data
##################################################################################

# read in ice on and off data
ice <- read.csv("data/ice_off.csv", header = T)

# filter and format
ice1 = ice %>% filter(!Year > 2016) %>%
  dplyr::select(Year, Ice.Off.Date, Ice.On.Date, Number.of.Ice.Free.Days) %>%
  rename(year = Year, ice_off_date = Ice.Off.Date, ice_on_date = Ice.On.Date, ice_free_days = Number.of.Ice.Free.Days) %>% 
  mutate(ice_off_date = as.Date(ice_off_date), ice_on_date = as.Date(ice_on_date)) %>%
  mutate(ice_off = yday(ice_off_date), ice_on = yday(ice_on_date))



##############################################################################
# lake temperature profiles
################################################################################

wt = read.csv("data/water_temp_profiles.csv")

# filter to only include depths <=6m (i.e., littoral/nearshore temp) and format year, date
wt <- wt[!wt$depth > 6,]
wt$year <- year(wt$date)
wt$y_date <- yday(wt$date)

# subset L373 data
temp_373 <- wt[wt$lake == "373",]
# subset L239 data
temp_239 <- wt[wt$lake == "239",]

# merge each lake dataset to ice on/off data
temp_239 <- merge(temp_239, ice1, by = "year")
temp_373 <- merge(temp_373, ice1, by = "year")

# only keep temp data for open-water periods
temp_239 <- temp_239[temp_239$y_date > temp_239$ice_off & temp_239$y_date < temp_239$ice_on,]
temp_373 <- temp_373[temp_373$y_date > temp_373$ice_off & temp_373$y_date < temp_373$ice_on,]

# calculate mean daily littoral zone temps for L373
lit_temp_373 <- aggregate(temp_373$temp, by = list(temp_373$date), FUN = mean)
colnames(lit_temp_373) <- c("date", "lit_temp")
lit_temp_373$date <- as.Date(lit_temp_373$date)
lit_temp_373$year <- year(lit_temp_373$date)
lit_temp_373$lake <- "373"

# calculate mean daily littoral zone temps for L239
lit_temp_239 <- aggregate(temp_239$temp, by = list(temp_239$date), FUN = mean)
colnames(lit_temp_239) <- c("date", "lit_temp")
lit_temp_239$date <- as.Date(lit_temp_239$date)
lit_temp_239$year <- year(lit_temp_239$date)
lit_temp_239$lake <- "239"

# interpolate across days
lit_temps <- rbind(lit_temp_239, lit_temp_373)

ld <- split(lit_temps, paste(lit_temps$year, lit_temps$lake))

res <- list()

for (i in 1:length(ld)) {
  df <- ld[[i]]
  df$date <- as.Date(df$date)
  dates <- data.frame(date=seq(min(df$date), max(df$date), "days"))
  df <- merge(df, dates, by="date", all.y=T)
  df$lit_temp <- na.interp(df$lit_temp)
  df$lit_temp <- as.numeric(round(df$lit_temp, 1))
  df$year <- df$year[1]
  df$lake <- df$lake[1]
  res <- rbind(res, df)
}
  
res <- right_join(res, ice1)
res$y_date <- yday(res$date)


# calculate season lengths, mean and max littoral zone temps in summer
res1 = 
  res %>%
  filter(lit_temp > 15) %>%
  group_by(year, lake) %>%
  mutate(max_date = max(y_date),
         min_date = min(y_date)) %>%
  mutate(year = as.integer(year),
         year_length = seas::year.length(year),
         mean_lit_temp = mean(lit_temp),
         max_lit_temp = max(lit_temp)
  ) %>%
  dplyr::select(year, lake, ice_off, ice_on, max_date, min_date, year_length, max_lit_temp) %>%
  distinct() %>%
  arrange(lake, year) %>%
  ungroup() %>%
  mutate(
    ice_on = case_when(
      lake == 373 ~ ice_on - 4,
      lake == 239 ~ ice_on),
    ice_off = case_when(
      lake == 373 ~ ice_off - 1,
      lake == 239 ~ ice_off)
    ) %>% 
  mutate(spring = min_date - ice_off, 
         summer = (max_date - min_date) + 1,
         fall = ice_on - max_date,
         winter = (year_length - lag(ice_on)) + ice_off
  ) %>%
  filter(!year == 1969)


# trend tests and predictions

# winter length
res1_239 = res1 %>% filter(lake == 239)
tidy(MannKendall(as.ts(res1_239$winter)))
mod = mblm(winter ~ year, data = res1_239)
summary(mod)
confint(mblm(winter ~ year, data = res1_239))
new = data.frame(year = c(1970, 1986, 2016))
new$predict = predict(mod, new)

#plot
winter_p =
ggplot(res1_239, aes(x = year, y = winter)) + 
  geom_point(size = 4) +
  geom_line() +
  geom_line(data = new, aes(x = year, y = predict), colour = "black") +
  theme_bw(base_size = 16) +
  guides(fill = "none")+
  #annotate("rect", xmin = 1986, xmax = 2016, ymin = 120, ymax = 200,
  #         alpha = .1,fill = "grey40") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  xlab("Year")+
  ylab("Winter duration (days)")

# spring
tidy(MannKendall(as.ts(res1_239$spring)))
mod = mblm(spring ~ year, data = res1_239)
summary(mod)
confint(mblm(winter ~ year, data = res1_239))
new = data.frame(year = c(1970, 1986, 2016))
new$predict = predict(mod, new)


# plot
spring_p =
ggplot(res1_239, aes(x = year, y = spring)) + 
  geom_point(size = 4) +
  geom_line() +
  #geom_line(data = new, aes(x = year, y = predict), colour = "black") +
  theme_bw(base_size = 16) +
  guides(fill = "none")+
  #annotate("rect", xmin = 1986, xmax = 2016, ymin = 20, ymax = 85,
  #         alpha = .1,fill = "grey40") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  xlab("Year")+
  ylab("Spring duration (days)")

# summer
tidy(MannKendall(as.ts(res1_239$summer)))
mod = mblm(summer ~ year, data = res1_239)
summary(mod)
confint(mblm(summer ~ year, data = res1_239))
new = data.frame(year = c(1970, 1986, 2016))
new$predict = predict(mod, new)

# plot
summer_p =
ggplot(res1_239, aes(x = year, y = summer)) + 
  geom_point(size = 4) +
  geom_line() +
  geom_line(data = new, aes(x = year, y = predict), colour = "black") +
  theme_bw(base_size = 16) +
  guides(fill = "none")+
  #annotate("rect", xmin = 1986, xmax = 2016, ymin = 50, ymax = 130,
  #         alpha = .1,fill = "grey40") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  xlab("Year")+
  ylab("Summer duration (days)")

# fall
tidy(MannKendall(as.ts(res1_239$fall)))
mod = mblm(fall ~ year, data = res1_239)
summary(mod)
confint(mblm(summer ~ year, data = res1_239))
new = data.frame(year = c(1970, 1986, 2016))
new$predict = predict(mod, new)

# plot
fall_p =
ggplot(res1_239, aes(x = year, y = fall)) + 
  geom_point(size = 4) +
  geom_line() +
  #geom_line(data = new, aes(x = year, y = predict), colour = "black") +
  theme_bw(base_size = 16) +
  guides(fill = "none")+
  #annotate("rect", xmin = 1986, xmax = 2016, ymin = 40, ymax = 85,
  #         alpha = .1,fill = "grey40") +
  theme(axis.text.x = element_text(colour = 'black'),
        axis.text.y = element_text(colour = 'black')) +
  xlab("Year")+
  ylab("Fall duration (days)")

# season length panel
season_plot =cowplot::plot_grid(winter_p, spring_p, summer_p, fall_p)
# air temp and season panel combined
climate_season_plot = cowplot::plot_grid(temp_plot, season_plot, labels = c("A", "B"), label_size=20)
climate_season_plot
# save plot as tiff
cowplot::save_plot("figs/climate_season_panel.tiff", climate_season_plot, compression = "lzw", base_height = 8, base_width = 17, dpi = 500)

#write.csv(res1, "climate_trends.csv", row.names = F)
