#load packages
library(tidyverse)
library(broom)
library(lubridate)
library(Kendall)
library(mblm)
library(bbmle)

# add some functions
# function to calculate lower 20% quantile
q20 = function(x) {quantile(x, probs=c(0.20), na.rm = T)}

# function to calculate upper 5% quantile
q95 = function(x) {quantile(x, probs=c(0.95), na.rm = T)}

# read in L373 lake trout data 1986-2016
dat = read.csv("data/373_LT_DB_2016.csv")

# format data for analysis
dat_f = dat %>% mutate(DATE_CATCH = as.POSIXct(dat$DATE_CATCH, format="%m/%d/%Y")) %>%
  mutate(DATE_CATCH = as.Date(DATE_CATCH)) %>%
  mutate(Year = year(DATE_CATCH), 
         Month = month(DATE_CATCH), 
         taxon = as.character(taxon)
         ) %>%
  filter(taxon == "F081") %>% # keep only lake trout
  filter(!Month < 7) %>% # remove spring data
  filter(!Year == 1982) %>% # remove 1982 data - small sample and was exploratory gill netting
  mutate(Ws = 10^(-5.681 + 3.2462*(log10(TOTAL_LENGTH)))) %>% # calculates standard weight for each fish based on its FL
  mutate(rW = WEIGHT/Ws*100) # calculates relative weight for each fish

# annual sample sizes
n_year = 
  dat_f %>%
  group_by(Year, AGE) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = AGE, values_from = n)

# annual summary data for all growth variables
dat_s = 
  dat_f %>% 
  group_by(Year) %>%
  summarise(sample_size = n(), 
            age_q20 = q20(AGE), 
            age_q95 = q95(AGE), 
            age_mean = mean(AGE, na.rm = T), 
            age_sd = sd(AGE, na.rm = T),
            fl_q20 = q20(FORK_LENGTH), 
            fl_q95 = q95(FORK_LENGTH), 
            fl_mean = mean(FORK_LENGTH, na.rm = T), 
            fl_sd = sd(FORK_LENGTH, na.rm = T),
            mass_q20 = q20(WEIGHT), 
            mass_q95 = q95(WEIGHT), 
            mass_mean = mean(WEIGHT, na.rm = T), 
            mass_sd = sd(WEIGHT, na.rm = T),
            rw_q20 = q20(rW), 
            rw_q95 = q95(rW), 
            rw_mean = mean(rW, na.rm = T), 
            rw_sd = sd(rW, na.rm = T)
            ) 

#write.csv(dat_s, "data/373_lt_summary_data.csv", row.names = F)

# format for trend analysis
dat_trend = 
  dat_s %>%
  select(Year, 
         age_q20, 
         age_q95, 
         age_mean, 
         fl_q20, 
         fl_q95, 
         fl_mean, 
         mass_q20, 
         mass_q95, 
         mass_mean, 
         rw_q20, 
         rw_q95, 
         rw_mean
         ) %>%
  pivot_longer(
    cols = age_q20:rw_mean,
    names_to = "trait",
    values_to = "value"
  )

# test for changes in long term trends in mean values over time
mktest_results =
  dat_trend %>%
  group_by(trait) %>%
  do(tidy(MannKendall(as.ts(.$value)))) 

siegel_results =
  dat_trend %>%
  group_by(trait) %>%
  do(tidy(mblm(value ~ Year, data = .))) %>%
  filter(term == "Year") 

siegel_min_predictions =
dat_trend %>%
  group_by(trait) %>%
  do(tidy(min(predict(mblm(value ~ Year, data = .))))) 

siegel_max_predictions =
dat_trend %>%
  group_by(trait) %>%
  do(tidy(max(predict(mblm(value ~ Year, data = .))))) 

siegel_confint =
  dat_trend %>%
  group_by(trait) %>%
  do(tidy(confint(mblm(value ~ Year, data = .)))) %>%
  filter(x[,1] < 0)

# calculate mean and sd for each trait
summary_data =
  dat_trend %>%
  group_by(trait) %>%
  summarise(mean = mean(value), 
            sd = sd(value)
            ) 

# prep data for size at age effect plots
# fork length
mean_faa = dat_f %>% mutate(AGE1 = replace(AGE, AGE > 20, 21)) %>%
  group_by(YEAR, AGE1) %>%
  summarise(fl = mean(FORK_LENGTH, na.rm = T), fl_sd = sd(FORK_LENGTH)) %>%
  filter(!AGE1 > 25)
colnames(mean_faa) = c("year", "age", "fl", "fl_sd")

f_l = split(mean_faa, mean_faa$age)
f_res = NULL

for (i in 1:length(f_l)) {
  df<-f_l[[i]]
  df<-df[order(df$year),]
  p<-MannKendall(as.ts(df$fl))$sl 
  s<-as.numeric(summary(mblm(fl ~ year, data = df))$coefficients[2])
  lcl<-as.numeric(confint(mblm(fl ~ year, data = df)))[2]
  ucl<-as.numeric(confint(mblm(fl ~ year, data = df)))[4]
  res<-data.frame(age=unique(df$age), slope=s, lcl=lcl, ucl=ucl, pvalue=p)
  f_res<-rbind(f_res, res)
}

f_res$sig<- "n"
f_res[f_res$pvalue < 0.05, names(f_res) == "sig"] <- "y_h"
f_res[f_res$pvalue > 0.05 & f_res$pvalue < 0.1, names(f_res) == "sig"] <- "y_l"
f_res <- f_res[!f_res$age < 2,]


# weight
mean_waa = dat_f %>% mutate(AGE1 = replace(AGE, AGE > 20, 21)) %>% 
  group_by(YEAR, AGE1) %>%
  summarise(weight = mean(WEIGHT, na.rm = T), fl_sd = sd(WEIGHT)) %>%
  filter(!AGE1 > 25)
colnames(mean_waa) = c("year", "age", "weight", "weight_sd")

w_l = split(mean_waa, mean_waa$age)
w_res = NULL

for (i in 1:length(w_l)) {
  df<-w_l[[i]]
  df<-df[order(df$year),]
  p<-MannKendall(as.ts(df$weight))$sl 
  s<-as.numeric(summary(mblm(weight ~ year, data = df))$coefficients[2])
  lcl<-as.numeric(confint(mblm(weight ~ year, data = df)))[2]
  ucl<-as.numeric(confint(mblm(weight ~ year, data = df)))[4]
  res<-data.frame(age=unique(df$age), slope=s, lcl=lcl, ucl=ucl, pvalue=p)
  w_res<-rbind(w_res, res)
}

w_res$sig<- "n"
w_res[w_res$pvalue < 0.05, names(w_res) == "sig"] <- "y_h"
w_res[w_res$pvalue > 0.05 & w_res$pvalue < 0.1, names(w_res) == "sig"] <- "y_l"
w_res <- w_res[!w_res$age < 2,]

# condition (relative weight)
mean_caa <- dat_f %>% mutate(AGE1 = replace(AGE, AGE > 20, 21)) %>% 
  group_by(YEAR, AGE1) %>%
  summarise(rw = mean(rW, na.rm = T), rw_sd = sd(rW)) %>%
  filter(!AGE1 > 25)
colnames(mean_caa)<-c("year", "age", "rw", "rW_sd")

c_l<-split(mean_caa, mean_caa$age)
c_res<-NULL

for (i in 1:length(c_l)) {
  df<-c_l[[i]]
  df<-df[order(df$year),]
  df<-df[!df$rw == "NaN",]
  p<-MannKendall(as.ts(df$rw))$sl 
  s<-as.numeric(summary(mblm(rw ~ year, data = df))$coefficients[2])
  lcl<-as.numeric(confint(mblm(rw ~ year, data = df)))[2]
  ucl<-as.numeric(confint(mblm(rw ~ year, data = df)))[4]
  res<-data.frame(age=unique(df$age), slope=s, lcl=lcl, ucl=ucl, pvalue=p)
  c_res<-rbind(c_res, res)
}

c_res$sig<- "n"
c_res[c_res$pvalue < 0.05, names(c_res) == "sig"] <- "y_h"
c_res[c_res$pvalue > 0.05 & c_res$pvalue < 0.1, names(c_res) == "sig"] <- "y_l"
c_res <- c_res[!c_res$age < 2,]


