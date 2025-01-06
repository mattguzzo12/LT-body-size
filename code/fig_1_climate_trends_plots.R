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

# met station air temp
t1 <- read.csv("data/met_air_temp.csv", header = T)
t2 <- read.csv("data/Ta2014_to_upload_ISODate.csv", header = T)
t3 <- read.csv("data/Ta2015_to_upload_ISODate.csv", header = T)
t4 <- read.csv("data/Ta2016_to_upload_ISODate.csv", header = T)

# combine data
t24 <- rbind(t2, t3, t4)
t24 <- t24[c(5, 7, 8)]
t1 <- t1[c(4:6)]
colnames(t1) <- colnames(t24)
t<-rbind(t1, t24)
rm(t1, t2, t3, t4, t24)
# estimate mean temp
t$mean_temp <- (t$min_temp + t$max_temp)/2
# format dates
t$month<-month(t$mean_date)
t$year<-year(t$mean_date)

t = t %>% select(date = mean_date, year, mean_temp)
write.csv(t, "data_all/mean_annual_air_temps.csv", row.names = F)

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

# up to 2013
temp <- read.csv("data/239_224_373_442_water_temp_profiles.csv", check.names = F)
temp <- temp[c(1, 4, 6, 7)]
colnames(temp) <- c("lake", "date", "depth", "temp")
temp$date <- as.Date(temp$date)
# 2014
t2014 <- read.csv("data/373_239_RBRDepthProfiles2014_corrected.csv")
t2014 <- t2014[c(1, 4, 9, 11)]
colnames(t2014)<-c("lake", "date", "depth", "temp")
t2014$date <- as.character(t2014$date)
t2014$date <- as.POSIXct(t2014$date, format = "%Y-%m-%d")
# 2015
t2015 <- read_excel("data/tempo2 profiles 2015.xlsx")
t2015 <- t2015[c(1, 4, 8, 10)]
colnames(t2015) <- c("lake", "date", "depth", "temp")
#2016
t2016 <- read_excel("data/tempo2 profiles 2016.xlsx")
t2016 <- t2016[c(1, 4, 9, 11)]
colnames(t2016) <- c("lake", "date", "depth", "temp")
# 2017
t2017 <- read_excel("data/tempo2 profiles 2017.xlsx")
t2017 <- t2017[c(1, 4, 8, 10)]
colnames(t2017) <- c("lake", "date", "depth", "temp")

# combine all recent years and format dates
temp1 <- rbind(t2014, t2015, t2016, t2017)
temp1$date <- as.Date(temp1$date)
rm(t2014, t2015, t2016, t2017)

# combine all water temp data round depth to 1 decmimal
wt <- rbind(temp, temp1)
wt$depth <- round(wt$depth, 1)
rm(temp, temp1)

write.csv(wt, "data/water_temp_profiles.csv", row.names = F)

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
# save plot as tiff
cowplot::save_plot("figs/climate_season_panel.tiff", climate_season_plot, compression = "lzw", base_height = 8, base_width = 17, dpi = 500)

#write.csv(res1, "climate_trends.csv", row.names = F)
