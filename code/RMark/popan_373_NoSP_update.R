##################################
# popan_373_NoSP_update.R
#
# uses data to 2016
# original code via Matt Guzzo
# spring catch assigned previous fall
# M. Rennie Dec 2017
#
# note that code requires running on a windows machine (or virtual machine, e.g. Parallels if on a mac) to allow RMark to talk to Program Mark
##################################

rm(list=ls())


require(RMark)
require(plyr)

#create time interval vector
intervals<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

#create vector with sampling occassion names
Dates<-c("1986_2F", "1987_2F", "1988_2F", "1989_2F", "1990_2F", 
         "1991_2F", "1992_2F", "1993_2F", "1994_2F", "1995_2F", 
         "1996_2F", "1997_2F", "1998_2F", "1999_2F", "2000_2F", 
         "2001_2F", "2002_2F", "2003_2F", "2004_2F", "2005_2F", 
         "2006_2F", "2007_2F", "2008_2F", "2009_2F", "2010_2F", 
         "2011_2F", "2012_2F", "2013_2F", "2014_2F", "2015_2F", "2016_2F") 

#bring in encounter history input file
input_file<-"373_LT_2016inclusive_noSP_forR.inp"

#convert input to df
encounter<-convert.inp(input_file, use.comments=T)

#process the encounter data
popan.processed<-process.data(encounter, model="POPAN", time.intervals=intervals)

#test for overdispersion of data
release.gof(popan.processed)

# X2 total /df total = c-hat = 193.2/166 = 1.16 

#make default design data
popan.ddl<-make.design.data(popan.processed)

#create capture probability survival and 
#recruitment variables to be constant or vary over time
Phi.dot<-list(formula=~1)
p.dot<-list(formula=~1)
pent.dot<-list(formula=~1)
Phi.t<-list(formula=~time)
p.t<-list(formula=~time)
pent.t<-list(formula=~time)


#make model combinations (all constant and all time varying models for comparison)
Phi.dot_p.dot_pent.dot<-mark(popan.processed, popan.ddl, time.intervals=intervals,  
                             model.parameters=list(Phi=Phi.dot,p=p.dot,pent=pent.dot))
Phi.t_p.t_pent.t<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                       model.parameters=list(Phi=Phi.t,p=p.t,pent=pent.t))
Phi.t_p.dot_pent.dot<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                       model.parameters=list(Phi=Phi.t,p=p.dot,pent=pent.dot))
Phi.t_p.dot_pent.t<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                           model.parameters=list(Phi=Phi.t,p=p.dot,pent=pent.t))
Phi.dot_p.dot_pent.t<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                           model.parameters=list(Phi=Phi.dot,p=p.dot,pent=pent.t))
Phi.dot_p.t_pent.t<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                           model.parameters=list(Phi=Phi.dot,p=p.t,pent=pent.t))
Phi.dot_p.t_pent.dot<-mark(popan.processed, popan.ddl, time.intervals=intervals,
                         model.parameters=list(Phi=Phi.dot,p=p.t,pent=pent.dot))

#despite notices of models indicating failure to run, inspection of output 
#files demonstrates this is not the case. Possible glitch in Rmark...

#collects and saves results of above models... fails to execute? 
popan.results<-collect.models()

#view results which includes AICc ranking of constant vs time varying model
popan.results

#apply c-hat correction based on c-hat estimate about
popan.results.adj<-adjust.chat(1.16, popan.results) #see release results, 245/208
popan.results.adj


#extract names of model in table
names(popan.results.adj)

#extract output from the adjusted top model
summary(popan.results.adj$Phi.dot_p.t_pent.dot)

#here are the derived estimates
Gross_Births<-ldply(popan.results.adj$Phi.dot_p.t_pent.dot$results$derived[1])
Net_Births<-ldply(popan.results.adj$Phi.dot_p.t_pent.dot$results$derived[2])
N_est<-ldply(popan.results.adj$Phi.dot_p.t_pent.dot$results$derived[3])
Gross_N<-ldply(popan.results.adj$Phi.dot_p.t_pent.dot$results$derived[4])

N_est.t<-ldply(popan.results.adj$Phi.dot_p.t_pent.t$results$derived[3])
#throw the dates on abundance file to make life easier
N_est$Dates<-Dates
N_est #these are the top model results

Net_Births$Intervals<-intervals
Net_Births

N_est.t$Dates<-Dates

N_est.tab<-data.frame(N_est)

write.table(N_est.tab, file="373_LT_NoSp_phi.dot_p.t_pent.dot.csv", sep=",")
            





