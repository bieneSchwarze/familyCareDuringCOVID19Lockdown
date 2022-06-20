###############################################################################################
###############################################################################################
##
## Paper on the impact of informal care during spring 2020 in comparison to 2019 and 2018 on 
## - depression scores
## - general life satisfaction
## 
## Data Preparation & Analysis
## for balanced panel 2018 - 2019
##
##
## SZ, 20.06.2022
##
###############################################################################################
###############################################################################################

rm(list=ls())
library(haven)
library(mice)
library(miceadds)
library(questionr)
library(Hmisc)
library(plyr)
library(survey)
library(reshape2)
library(Hmisc)
library(ggplot2)
library(plyr)
library(plm)
library(reldist)
library(naniar)
library(weights)
library(diagis)

#################################################################
# A. Data preparation 
#################################################################

# MIND:
# (1) Measure on depression avialable for 2020, 2019, and 2016. Take the 2016 measure as proxy for 2018.
# (2) No household income yet available for 2020. Use instead hh income of previous year

# Read data
setwd("C:\\Users\\Freddie\\Documents\\CovS\\informalCare\\DATA")
PL <- read_dta("pl.dta")
COV <- read_dta("soep_cov_20210211_statav13.dta") # derived from plong in SOEP data v36 (simply write plong here, will also work)
COV2016 <- COV[COV$syear == 2016,]
COV2017 <- COV[COV$syear == 2017,]
COV2018 <- COV[COV$syear == 2018,]
COV2019 <- COV[COV$syear == 2019,]

# Care time info 2019
CT <- COV2019[,c("pid", "pli0046", "psample")] 
CT <- CT[!(CT$psample %in% c(17,18,19,22,23)),] # no refuguees and super rich (sample P, not part of 2018 survey) and LQBT (sample Q, not part of 2018 survey) 
colnames(CT)[2] <- "careTime_2019"
table(CT$careTime_2019, exclude=NULL)
CT$careTime_2019[CT$careTime_2019<0] <- NA

# Add care time info of 2018
K1 <- COV2018[,c("pid", "pli0046")] # in 2016: 24% miss auf Caretime -> take 2018 and use depression score of 2016 as proxy for 2018
colnames(K1)[2] <- "careTime_2018"
CT <- merge(CT, K1, by="pid", all.x=TRUE) # base is 2019
table(CT$careTime_2018, exclude=NULL)
CT$careTime_2018[CT$careTime_2018<0] <- NA

# Add gender and sex 
COV201118 <- COV[COV$syear %in% 2011:2018,]
bb <- COV201118[,c("pid", "syear", "gebjahr", "sex")]
bb <- bb[order(bb$pid),]
bb <- bb[setdiff(1:nrow(bb), which(is.na(bb$gebjahr), arr.ind=TRUE)),]
bb <- bb[setdiff(1:nrow(bb), which(bb$gebjahr<0)),]
bb <- bb[order(bb$pid, bb$syear, decreasing = TRUE),]
bb <- bb[!duplicated(bb$pid),]
table(duplicated(bb[,1]))
table(CT$pid %in% bb$pid) 
bb <- bb[bb$pid %in% CT$pid,]
CT <- merge(CT, bb[,-2], by="pid", all.x=TRUE) 

# Add information on depression: 2016
D2016 <- COV2016[,c("pid", "plh0339", "plh0340", "plh0341", "plh0342")]
colnames(D2016) <- c("pid", "depress1_2016", "depress2_2016","depress3_2016", "depress4_2016")
CT <- merge(CT, D2016, by="pid", all.x=TRUE)
table(CT$depress1_2016, exclude=NULL)
table(CT$depress2_2016, exclude=NULL)
table(CT$depress3_2016, exclude=NULL)
table(CT$depress4_2016, exclude=NULL)
CT$depress1_2016[CT$depress1_2016<0] <- NA
CT$depress2_2016[CT$depress2_2016<0] <- NA
CT$depress3_2016[CT$depress3_2016<0] <- NA
CT$depress4_2016[CT$depress4_2016<0] <- NA
CT$depress_2018 <- CT$depress1_2016 + CT$depress2_2016 + CT$depress3_2016 + CT$depress4_2016 # sum score, 2016 values takes as proxies for 2018
table(CT$depress_2018, exclude = NULL)
CT <- CT[,!(colnames(CT) %in% c("depress1_2016", "depress2_2016","depress3_2016", "depress4_2016"))]

# Add information on depression: 2019
D2019 <- COV2019[,c("pid", "plh0339", "plh0340", "plh0341", "plh0342")]
colnames(D2019) <- c("pid", "depress1_2019", "depress2_2019","depress3_2019", "depress4_2019")
CT <- merge(CT, D2019, by="pid", all.x=TRUE)
table(CT$depress1_2019, exclude=NULL)
table(CT$depress2_2019, exclude=NULL)
table(CT$depress3_2019, exclude=NULL)
table(CT$depress4_2019, exclude=NULL)
CT$depress1_2019[CT$depress1_2019<0] <- NA
CT$depress2_2019[CT$depress2_2019<0] <- NA
CT$depress3_2019[CT$depress3_2019<0] <- NA
CT$depress4_2019[CT$depress4_2019<0] <- NA
CT$depress_2019 <- CT$depress1_2019 + CT$depress2_2019 + CT$depress3_2019 + CT$depress4_2019 # sum score 
table(CT$depress_2019, exclude = NULL)
CT <- CT[,!(colnames(CT) %in% c("depress1_2019", "depress2_2019","depress3_2019", "depress4_2019"))]

# Add employment status 2018 
E2018 <- COV2018[,c("pid", "plb0022_h")] # current employment status
colnames(E2018) <- c("pid", "employ_2018")
E2018$employ_2018 <- ifelse(E2018$employ_2018 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2018$employ_2018 %in% c(5,9), "non_unemployed", NA))
table(E2018$employ_2018, exclude=NULL)
E2018 <- E2018[E2018$pid %in% CT$pid,] # N=22809 of N=22998
PL2018 <- PL[PL$syear %in% 2018,]   # person questionnaire in long format, 2018
PL2018_unempl <- PL2018[, c("pid", "plb0021")] # registered unemployed at BA? (code 1 yes, code 2 no)
E2018 <- merge(E2018, PL2018_unempl, by="pid", all.x=TRUE)
E2018$employ_2018[E2018$plb0021 %in% 1] <- "unemployed"
E2018$employ_2018[E2018$employ_2018 %in% "non_unemployed" & !(E2018$plb0021 %in% 1)] <- "nonemployed"
table(E2018$employ_2018, exclude=NULL)
PL2018_self <- PL2018[, c("pid", "plb0568_v1")] # self-employed (value 1)?
E2018 <- merge(E2018, PL2018_self, by="pid", all.x=TRUE)
table(E2018$employ_2018, E2018$plb0568_v1, exclude=NULL) # Few inconsistencies: N=18 self-employed, N=100 worker, N=7 trainees, N=80 employees are registered unemployed (all self-reported)
CT <- merge(CT, E2018[,c("pid", "employ_2018")], by="pid", all.x = TRUE)
table(CT$employ_2018, exclude=NULL)

# Add emplyoment status 2019 
E2019 <- COV2019[,c("pid", "plb0022_h")] # current employment status
colnames(E2019) <- c("pid", "employ_2019")
E2019$employ_2019 <- ifelse(E2019$employ_2019 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2019$employ_2019 %in% c(5,9), "non_unemployed", NA))
table(E2019$employ_2019, exclude=NULL)
E2019 <- E2019[E2019$pid %in% CT$pid,] # N=22998 of N=22998
PL2019 <- PL[PL$syear %in% 2019,]   # person questionnaire in long format, 2019
PL2019_unempl <- PL2019[, c("pid", "plb0021")] # registered unemployed at BA? (code 1 yes, code 2 no)
E2019 <- merge(E2019, PL2019_unempl, by="pid", all.x=TRUE)
E2019$employ_2019[E2019$plb0021 %in% 1] <- "unemployed"
E2019$employ_2019[E2019$employ_2019 %in% "non_unemployed" & !(E2019$plb0021 %in% 1)] <- "nonemployed"
table(E2019$employ_2019, exclude=NULL)
PL2019_self <- PL2019[, c("pid", "plb0568_v1")] # self-employed (value 1)?
E2019 <- merge(E2019, PL2019_self, by="pid", all.x=TRUE)
table(E2019$employ_2019, E2019$plb0568_v1, exclude=NULL) # Few inconsistencies: N=19 self-employed, N=85 worker, N=5 trainees, N=74 employees are registered unemployed (all self-reported)
CT <- merge(CT, E2019[,c("pid", "employ_2019")], by="pid", all.x = TRUE)
table(CT$employ_2019, exclude=NULL)

# Worrying about own economic situation, 2018 
CT <- merge(CT, COV2018[, c("pid", "plh0033")], by="pid", all.x=TRUE)
table(CT$plh0033, exclude=NULL) # 1: grave worries, 2: some worries, 3: no worries
CT$plh0033[CT$plh0033 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0033"] <- "worriesEcon_2018"

# Worrying about own economic situation, 2019 
CT <- merge(CT, COV2019[, c("pid", "plh0033")], by="pid", all.x=TRUE)
table(CT$plh0033, exclude=NULL) # 1: grave worries, 2: some worries, 3: no worries
CT$plh0033[CT$plh0033 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0033"] <- "worriesEcon_2019"

# Life satisfaction, 2018 
CT <- merge(CT, COV2018[, c("pid", "plh0166")], by="pid", all.x=TRUE)
table(CT$plh0166, exclude=NULL) # 0 completely dissatisfied, ..., 10: completely satisfied
CT$plh0166[CT$plh0166 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0166"] <- "lifeSatis_2018"

# Life satisfaction, 2019 
PL2019_lifeSatis <- PL2019[, c("pid", "plh0182")] # general life satisfaction
CT <- merge(CT, PL2019_lifeSatis, by="pid", all.x=TRUE)
table(CT$plh0182, exclude=NULL) # 0 completely dissatisfied, ..., 10: completely satisfied
CT$plh0182[CT$plh0182 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0182"] <- "lifeSatis_2019"

# Add weights 2019
phrf2019 <- read_dta("phrf.dta")
CT <- merge(CT, phrf2019[, c("pid", "bjphrf")], by="pid", all.x=TRUE)
table(CT$bjphrf %in% 0) # N=171 cases with zero weights

#################################################################
# B. Descriptives
#################################################################

table(is.na(CT$lifeSatis_2018)) 
weighted.mean(CT$lifeSatis_2018, weights=CT$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CT$lifeSatis_2018[!is.na(CT$lifeSatis_2018)], CT$bjphrf[!is.na(CT$lifeSatis_2018)]))
v1 <- CT$lifeSatis_2018 + 1
v2 <- CT$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CT$careTime_2018), NA, ifelse(CT$careTime_2018 < 1, 0, ifelse(CT$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <=2, 1, 2)))
CT_c <- CT[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CT_c)
CT_nc <- CT[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2018 + 1
v2 <- CT_c$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$bjphrf, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2018 + 1
v2 <- CT_nc$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$bjphrf, na.rm=TRUE)

CT_r <- CT[CT$careTime_2018 < 1 & !is.na(CT$careTime_2018),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2018)) 
weighted.mean(CT_r$lifeSatis_2018, weights=CT_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CT_r$lifeSatis_2018[!is.na(CT_r$lifeSatis_2018)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2018)]))
v1 <- CT_r$lifeSatis_2018 + 1
v2 <- CT_r$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CT_r$careTime_2018), NA, ifelse(CT_r$careTime_2018 < 1, 0, ifelse(CT_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2018 + 1
v2 <- CT_c$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$bjphrf, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2018 + 1
v2 <- CT_nc$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$bjphrf, na.rm=TRUE)

CT_r <- CT[CT$careTime_2018 >= 1 & CT$careTime_2018 <= 2 & !is.na(CT$careTime_2018),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2018)) 
weighted.mean(CT_r$lifeSatis_2018, weights=CT_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CT_r$lifeSatis_2018[!is.na(CT_r$lifeSatis_2018)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2018)]))
v1 <- CT_r$lifeSatis_2018 + 1
v2 <- CT_r$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CT_r$careTime_2018), NA, ifelse(CT_r$careTime_2018 < 1, 0, ifelse(CT_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2018 + 1
v2 <- CT_c$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$bjphrf, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2018 + 1
v2 <- CT_nc$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$bjphrf, na.rm=TRUE)

CT_r <- CT[CT$careTime_2018 > 2 & !is.na(CT$careTime_2018),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2018)) 
weighted.mean(CT_r$lifeSatis_2018, weights=CT_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CT_r$lifeSatis_2018[!is.na(CT_r$lifeSatis_2018)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2018)]))
v1 <- CT_r$lifeSatis_2018 + 1
v2 <- CT_r$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CT_r$careTime_2018), NA, ifelse(CT_r$careTime_2018 < 1, 0, ifelse(CT_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2018 + 1
v2 <- CT_c$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$bjphrf, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2018 + 1
v2 <- CT_nc$lifeSatis_2019 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$bjphrf, na.rm=TRUE)

table(is.na(CT$lifeSatis_2019))
m_lif2019 <- weighted.mean(CT$lifeSatis_2019, weights=CT$bjphrf, na.rm=TRUE); m_lif2019
sd_lif2019 <- sqrt(wtd.var(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)], CT$bjphrf[!is.na(CT$lifeSatis_2019)])); sd_lif2019
se_lif2019 <- weighted_se(as.numeric(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)]), as.numeric(CT$bjphrf[!is.na(CT$lifeSatis_2019)]), na.rm=TRUE); se_lif2019

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c1 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_lif2019_c1
sd_lif2019_c1 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c1
se_lif2019_c1 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c1

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c2 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_lif2019_c2
sd_lif2019_c2 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c2
se_lif2019_c2 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c2

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c3 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_lif2019_c3
sd_lif2019_c3 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c3 
se_lif2019_c3 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c3

table(is.na(CT$depress_2018)) # 291 miss
CTwo <- CT[!(CT$psample %in% c(20,21)),] # take out samples N and O (no measurement in 2016 of depression score, at this time samples N and O were not yet part of SOEP)
weighted.mean(CTwo$depress_2018, weights=CTwo$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CTwo$depress_2018[!is.na(CTwo$depress_2018)], CTwo$bjphrf[!is.na(CTwo$depress_2018)]))
v1 <- CTwo$depress_2018
v2 <- CTwo$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CTwo$careTime_2018), NA, ifelse(CTwo$careTime_2018 < 1, 0, ifelse(CTwo$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CTwo$careTime_2019), NA, ifelse(CTwo$careTime_2019 < 1, 0, ifelse(CTwo$careTime_2019 <=2, 1, 2)))
CTwo_c <- CTwo[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CTwo_c)
CTwo_nc <- CTwo[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CTwo_nc)
v1 <- CTwo_c$depress_2018
v2 <- CTwo_c$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_c$bjphrf, na.rm=TRUE)
v1 <- CTwo_nc$depress_2018
v2 <- CTwo_nc$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_nc$bjphrf, na.rm=TRUE)

CTwo_r <- CTwo[CTwo$careTime_2018 < 1 & !is.na(CTwo$careTime_2018),]; nrow(CTwo_r)
table(is.na(CTwo_r$depress_2018)) 
weighted.mean(CTwo_r$depress_2018, weights=CTwo_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CTwo_r$depress_2018[!is.na(CTwo_r$depress_2018)], CTwo_r$bjphrf[!is.na(CTwo_r$depress_2018)]))
v1 <- CTwo_r$depress_2018
v2 <- CTwo_r$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CTwo_r$careTime_2018), NA, ifelse(CTwo_r$careTime_2018 < 1, 0, ifelse(CTwo_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CTwo_r$careTime_2019), NA, ifelse(CTwo_r$careTime_2019 < 1, 0, ifelse(CTwo_r$careTime_2019 <=2, 1, 2)))
CTwo_c <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CTwo_c)
CTwo_nc <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CTwo_nc)
v1 <- CTwo_c$depress_2018
v2 <- CTwo_c$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_c$bjphrf, na.rm=TRUE)
v1 <- CTwo_nc$depress_2018
v2 <- CTwo_nc$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_nc$bjphrf, na.rm=TRUE)

CTwo_r <- CTwo[CTwo$careTime_2018 >= 1 & CTwo$careTime_2018 <= 2 & !is.na(CTwo$careTime_2018),]; nrow(CTwo_r)
table(is.na(CTwo_r$depress_2018)) 
weighted.mean(CTwo_r$depress_2018, weights=CTwo_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CTwo_r$depress_2018[!is.na(CTwo_r$depress_2018)], CTwo_r$bjphrf[!is.na(CTwo_r$depress_2018)]))
v1 <- CTwo_r$depress_2018
v2 <- CTwo_r$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CTwo_r$careTime_2018), NA, ifelse(CTwo_r$careTime_2018 < 1, 0, ifelse(CTwo_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CTwo_r$careTime_2019), NA, ifelse(CTwo_r$careTime_2019 < 1, 0, ifelse(CTwo_r$careTime_2019 <=2, 1, 2)))
CTwo_c <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CTwo_c)
CTwo_nc <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CTwo_nc)
v1 <- CTwo_c$depress_2018
v2 <- CTwo_c$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_c$bjphrf, na.rm=TRUE)
v1 <- CTwo_nc$depress_2018
v2 <- CTwo_nc$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_nc$bjphrf, na.rm=TRUE)

CTwo_r <- CTwo[CTwo$careTime_2018 > 2 & !is.na(CTwo$careTime_2018),]; nrow(CTwo_r)
table(is.na(CTwo_r$depress_2018)) 
weighted.mean(CTwo_r$depress_2018, weights=CTwo_r$bjphrf, na.rm=TRUE)
sqrt(wtd.var(CTwo_r$depress_2018[!is.na(CTwo_r$depress_2018)], CTwo_r$bjphrf[!is.na(CTwo_r$depress_2018)]))
v1 <- CTwo_r$depress_2018
v2 <- CTwo_r$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_r$bjphrf, na.rm=TRUE)
cAT_2018 <- ifelse(is.na(CTwo_r$careTime_2018), NA, ifelse(CTwo_r$careTime_2018 < 1, 0, ifelse(CTwo_r$careTime_2018 <=2, 1, 2)))
cAT_2019 <- ifelse(is.na(CTwo_r$careTime_2019), NA, ifelse(CTwo_r$careTime_2019 < 1, 0, ifelse(CTwo_r$careTime_2019 <=2, 1, 2)))
CTwo_c <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 != cAT_2019),]; nrow(CTwo_c)
CTwo_nc <- CTwo_r[!is.na(cAT_2018) & !is.na(cAT_2019) & (cAT_2018 == cAT_2019),]; nrow(CTwo_nc)
v1 <- CTwo_c$depress_2018
v2 <- CTwo_c$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_c$bjphrf, na.rm=TRUE)
v1 <- CTwo_nc$depress_2018
v2 <- CTwo_nc$depress_2019
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CTwo_nc$bjphrf, na.rm=TRUE)

table(is.na(CT$depress_2019)) # 291 miss
m_dep2019 <- weighted.mean(CT$depress_2019, weights=CT$bjphrf, na.rm=TRUE); m_dep2019
sd_dep2019 <- sqrt(wtd.var(CT$depress_2019[!is.na(CT$depress_2019)], CT$bjphrf[!is.na(CT$depress_2019)])); sd_dep2019
se_dep2019 <- weighted_se(as.numeric(CT$depress_2019[!is.na(CT$depress_2019)]), as.numeric(CT$bjphrf[!is.na(CT$depress_2019)]), na.rm=TRUE); se_dep2019

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c1 <- weighted.mean(CT_r$depress_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_dep2019_c1
sd_dep2019_c1 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$bjphrf[!is.na(CT_r$depress_2019)])); sd_dep2019_c1
se_dep2019_c1 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c1

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c2 <- weighted.mean(CT_r$depress_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_dep2019_c2
sd_dep2019_c2 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$bjphrf[!is.na(CT_r$depress_2019)])); sd_dep2019_c2
se_dep2019_c2 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c2

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c3 <- weighted.mean(CT_r$depress_2019, weights=CT_r$bjphrf, na.rm=TRUE); m_dep2019_c3
sd_dep2019_c3 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$bjphrf[!is.na(CT_r$depress_2019)])); sd_dep2019_c3
se_dep2019_c3 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$bjphrf[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c3

# Test Means 2019 - 2020
CT_t <- CT[!is.na(CT$lifeSatis_2018) & !is.na(CT$lifeSatis_2019),]
t_lif2018_19 <- wtd.t.test(x=CT_t$lifeSatis_2018-CT_t$lifeSatis_2019, weight=CT_t$bjphrf, bootse=TRUE);t_lif2018_19
CT_t <- CT[!is.na(CT$lifeSatis_2018) & !is.na(CT$lifeSatis_2019) & CT$careTime_2018<1,]
t_lif2018_19_c1 <- wtd.t.test(x=CT_t$lifeSatis_2018-CT_t$lifeSatis_2019, weight=CT_t$bjphrf, bootse=FALSE);t_lif2018_19_c1
CT_t <- CT[!is.na(CT$lifeSatis_2018) & !is.na(CT$lifeSatis_2019) & CT$careTime_2018>=1 & CT$careTime_2018<=2,]
t_lif2018_19_c2 <- wtd.t.test(x=CT_t$lifeSatis_2018-CT_t$lifeSatis_2019, weight=CT_t$bjphrf, bootse=FALSE);t_lif2018_19_c2
CT_t <- CT[!is.na(CT$lifeSatis_2018) & !is.na(CT$lifeSatis_2019) & CT$careTime_2018>2,]
t_lif2018_19_c3 <- wtd.t.test(x=CT_t$lifeSatis_2018-CT_t$lifeSatis_2019, weight=CT_t$bjphrf, bootse=FALSE);t_lif2018_19_c3
CTwo_t <- CTwo[!is.na(CTwo$depress_2018) & !is.na(CTwo$depress_2019),]
t_dep2018_19 <- wtd.t.test(x=CTwo_t$depress_2018-CTwo_t$depress_2019, weight=CTwo_t$bjphrf, bootse=TRUE);t_dep2018_19
CTwo_t <- CTwo[!is.na(CTwo$depress_2018) & !is.na(CTwo$depress_2019) & CTwo$careTime_2018<1,]
t_dep2018_19_c1 <- wtd.t.test(x=CTwo_t$depress_2018-CTwo_t$depress_2019, weight=CTwo_t$bjphrf, bootse=FALSE);t_dep2018_19_c1
CTwo_t <- CTwo[!is.na(CTwo$depress_2018) & !is.na(CTwo$depress_2019) & CTwo$careTime_2018>=1 & CTwo$careTime_2018<=2,]
t_dep2018_19_c2 <- wtd.t.test(x=CTwo_t$depress_2018-CTwo_t$depress_2019, weight=CTwo_t$bjphrf, bootse=FALSE);t_dep2018_19_c2
CTwo_t <- CTwo[!is.na(CTwo$depress_2018) & !is.na(CTwo$depress_2019) & CTwo$careTime_2018>2,]
t_dep2018_19_c3 <- wtd.t.test(x=CTwo_t$depress_2018-CTwo_t$depress_2019, weight=CTwo_t$bjphrf, bootse=FALSE);t_dep2018_19_c3

w <- wtd.table(CT$sex, weights=CT$bjphrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

age <- 2019 - CT$gebjahr
weighted.mean(age, CT$bjphrf, na.rm=TRUE)

table(is.na(age))
ageKat <- ifelse(is.na(age), NA, ifelse(age<=40, 1, ifelse(age<60,2,3)))
w <- wtd.table(ageKat, weights=CT$bjphrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$employ_2018)) 
w <- wtd.table(CT$employ_2018, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$employ_2019)) 
w <- wtd.table(CT$employ_2019, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$worriesEcon_2018)) 
w <- wtd.table(CT$worriesEcon_2018, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$worriesEcon_2019)) 
w <- wtd.table(CT$worriesEcon_2019, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2018))
careKat_2018 <- ifelse(is.na(CT$careTime_2018), NA, ifelse(CT$careTime_2018 < 1, 0, ifelse(CT$careTime_2018 <= 2, 1, 3))) 
table(is.na(careKat_2018)) 
w <- wtd.table(careKat_2018, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2019))
careKat_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <= 2, 1, 3))) 
table(is.na(careKat_2019)) 
w <- wtd.table(careKat_2019, weights=CT$bjphrf, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

weighted.mean(CT$careTime_2018, CT$bjphrf, na.rm=TRUE)
weighted.mean(CT$careTime_2019, CT$bjphrf, na.rm=TRUE)

#################################################################
# C. Impute Missings
#################################################################
MD <- md.pattern(CT, plot=FALSE)
round(MD[nrow(MD),]/nrow(CT),2) # max. 27% missing values in one of the variables: in "depress_2018" (Beware: no depression score values for 2016 (proxy for 2018) for sample N (psample: 20))
table(complete.cases(CT))/nrow(CT) # ~66% complete cases

CTI <- CT
fact <- c("sex", "employ_2018", "employ_2019")
num <- c("careTime_2018", "careTime_2019", "careTime_2020", "gebjahr", 
         "depress_2018", "depress_2019", 
         "lifeSatis_2018", "lifeSatis_2019", 
         "worriesEcon_2018", "worriesEcon_2019", 
         "bjphrf")
for(f in fact){
  CTI[,colnames(CTI) %in% f] <- as.factor(CTI[,colnames(CTI) %in% f])
}
for(n in num){
  CTI[,colnames(CTI) %in% n] <- as.numeric(CTI[,colnames(CTI) %in% n])
}
options(warn=0)
predM <- mice::make.predictorMatrix(data=CTI)
predM[,1] <- 0
predM[1,] <- 0
impM <- mice::make.method(data=CTI)
impM[!(impM =="")] <- "cart"
imp <- mice::mice(CTI, m=20, predictorMatrix=predM, method=impM, maxit=20,seed=28614)
imp$loggedEvents # ok

#################################################################
# D. Fixed Effects Regression
# Separate Models for continuous carers, new carers, noncarers
#################################################################

# --------------------------------------------------------------
# D.1 DV Depression Score for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList_nonCarers <- vector(length=imp$m, mode="list") 
modList_contCarers <- vector(length=imp$m, mode="list") 
modList_newCarers <- vector(length=imp$m, mode="list") 
rsq_noCarers <- rep(NA,imp$m)
rsq_contCarers <- rep(NA,imp$m)
rsq_newCarers <- rep(NA,imp$m)

for(i in 1:imp$m){

  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
  D_i <- D_i[!(D_i$psample %in% c(20,21)),] # take out samples N and O (no measurement in 2016 of depression score, at this time samples N and O were not yet part of SOEP)
  DAT_fe_i <- D_i[,c("pid", "sex", "gebjahr", "bjphrf", 
                     "careTime_2018", "careTime_2019", 
                     "employ_2018", "employ_2019", 
                     "worriesEcon_2018", "worriesEcon_2019", 
                     "depress_2018", "depress_2019",
                     "lifeSatis_2018",  "lifeSatis_2019")] 
  colnames(DAT_fe_i)[5:6] <- c("careTime.2018", "careTime.2019")
  colnames(DAT_fe_i)[7:8] <- c("employ.2018", "employ.2019")
  colnames(DAT_fe_i)[9:10] <- c("worriesEcon.2018", "worriesEcon.2019") 
  colnames(DAT_fe_i)[11:12] <- c("depress.2018", "depress.2019")
  colnames(DAT_fe_i)[13:14] <- c("lifeSatis.2018", "lifeSatis.2019")  
  
  DAT_fe_i$group <- ifelse(DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% 0, 1, 
                           ifelse(DAT_fe_i$careTime.2018 > 0 & DAT_fe_i$careTime.2019 > 0, 2,
                                  ifelse(DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
  table(DAT_fe_i$group)
  
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis")) 
  
  DAT_long_i$noCare_dum <- ifelse(DAT_long_i$careTime <1, 1, 0)  
  DAT_long_i$lowCare_dum <- ifelse(DAT_long_i$careTime >=1 & DAT_long_i$careTime <=2, 1, 0)    
  DAT_long_i$highCare_dum <- ifelse(DAT_long_i$careTime >2, 1, 0)  
  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)    
  DAT_long_i$age2019 <- 2019 - DAT_long_i$gebjahr
  DAT_long_i$birthyearCat <- ifelse(DAT_long_i$age2019<41,0,ifelse(DAT_long_i$age2019<61,1,2)) 
  
  DAT_long_i_noCarer <- DAT_long_i[DAT_long_i$group %in% 1,]
  mod_noCar <- plm(depress  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                     + unEmpl_dum + noEmpl_dum + 
                     + lowWorr_dum + highWorr_dum, 
                   data=DAT_long_i_noCarer, model="within", index=c("pid","time"))
  modList_nonCarers[[i]] <- mod_noCar
  rsq_noCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  
  DAT_long_i_contCarer <- DAT_long_i[DAT_long_i$group %in% 2,]
  mod_contCar <- plm(depress  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                       + highCare_dum +
                       + unEmpl_dum + noEmpl_dum + 
                       + lowWorr_dum + highWorr_dum, 
                     data=DAT_long_i_contCarer, model="within", index=c("pid","time"))
  modList_contCarers[[i]] <- mod_contCar
  rsq_contCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  
  DAT_long_i_newCarer <- DAT_long_i[DAT_long_i$group %in% 3,]
  mod_newCar <- plm(depress  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                      + highCare_dum + 
                      + unEmpl_dum + noEmpl_dum + 
                      + lowWorr_dum + highWorr_dum, 
                    data=DAT_long_i_newCarer, model="within", index=c("pid","time"))
  modList_newCarers[[i]] <- mod_newCar
  rsq_newCarers[i] <- summary(mod_newCar)$r.squared["rsq"]
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
# noncaregivers
res_nonCar_Depr <- summary(pool(as.mira(modList_nonCarers)), conf.int = TRUE)
res_nonCar <- cbind.data.frame(res_nonCar_Depr[,1], round(res_nonCar_Depr[,"estimate"],2), 
                               round(res_nonCar_Depr[,"2.5 %"],2), round(res_nonCar_Depr[,"97.5 %"],2))
colnames(res_nonCar) <- c("Var", "Est", "CIlow", "CIup")
res_nonCar
mean(rsq_noCarers)
# continuing caregivers
res_contCar_Depr <- summary(pool(as.mira(modList_contCarers)), conf.int = TRUE)
res_contCar <- cbind.data.frame(res_contCar_Depr[,1], round(res_contCar_Depr[,"estimate"],2), 
                                round(res_contCar_Depr[,"2.5 %"],2), round(res_contCar_Depr[,"97.5 %"],2))
colnames(res_contCar) <- c("Var", "Est", "CIlow", "CIup")
res_contCar
mean(rsq_contCarers)
# new caregivers
res_newCar_Depr <- summary(pool(as.mira(modList_newCarers)), conf.int = TRUE)
res_newCar <- cbind.data.frame(res_newCar_Depr[,1], round(res_newCar_Depr[,"estimate"],2), 
                               round(res_newCar_Depr[,"2.5 %"],2), round(res_newCar_Depr[,"97.5 %"],2))
colnames(res_newCar) <- c("Var", "Est", "CIlow", "CIup")
res_newCar
mean(rsq_newCarers)

# --------------------------------------------------------------
# D.2 DV Life Satisfaction for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList_nonCarers <- vector(length=imp$m, mode="list") 
modList_contCarers <- vector(length=imp$m, mode="list") 
modList_newCarers <- vector(length=imp$m, mode="list") 
rsq_noCarers <- rep(NA,imp$m)
rsq_contCarers <- rep(NA,imp$m)
rsq_newCarers <- rep(NA,imp$m)

for(i in 1:imp$m){
  
  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
  D_i <- D_i[!(D_i$psample %in% c(20,21)),] # take out samples N and O (no measurement in 2016 of depression score, at this time samples N and O were not yet part of SOEP)
  DAT_fe_i <- D_i[,c("pid", "sex", "gebjahr", "bjphrf", 
                     "careTime_2018", "careTime_2019", 
                     "employ_2018", "employ_2019", 
                     "worriesEcon_2018", "worriesEcon_2019", 
                     "depress_2018", "depress_2019",
                     "lifeSatis_2018",  "lifeSatis_2019")] 
  colnames(DAT_fe_i)[5:6] <- c("careTime.2018", "careTime.2019")
  colnames(DAT_fe_i)[7:8] <- c("employ.2018", "employ.2019")
  colnames(DAT_fe_i)[9:10] <- c("worriesEcon.2018", "worriesEcon.2019") 
  colnames(DAT_fe_i)[11:12] <- c("depress.2018", "depress.2019")
  colnames(DAT_fe_i)[13:14] <- c("lifeSatis.2018", "lifeSatis.2019")  
  
  DAT_fe_i$group <- ifelse(DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% 0, 1, 
                           ifelse(DAT_fe_i$careTime.2018 > 0 & DAT_fe_i$careTime.2019 > 0, 2,
                                  ifelse(DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 

  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis")) 
  
  DAT_long_i$noCare_dum <- ifelse(DAT_long_i$careTime <1, 1, 0)  
  DAT_long_i$lowCare_dum <- ifelse(DAT_long_i$careTime >=1 & DAT_long_i$careTime <=2, 1, 0)    
  DAT_long_i$highCare_dum <- ifelse(DAT_long_i$careTime >2, 1, 0)  
  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)    
  DAT_long_i$age2019 <- 2019 - DAT_long_i$gebjahr
  DAT_long_i$birthyearCat <- ifelse(DAT_long_i$age2019<41,0,ifelse(DAT_long_i$age2019<61,1,2)) 
  
  DAT_long_i_noCarer <- DAT_long_i[DAT_long_i$group %in% 1,]
  mod_noCar <- plm(lifeSatis  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                     + unEmpl_dum + noEmpl_dum + 
                     + lowWorr_dum + highWorr_dum, 
                   data=DAT_long_i_noCarer, model="within", index=c("pid","time"))
  modList_nonCarers[[i]] <- mod_noCar
  rsq_noCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  
  DAT_long_i_contCarer <- DAT_long_i[DAT_long_i$group %in% 2,]
  mod_contCar <- plm(lifeSatis  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                       + highCare_dum +
                       + unEmpl_dum + noEmpl_dum + 
                       + lowWorr_dum + highWorr_dum, 
                     data=DAT_long_i_contCarer, model="within", index=c("pid","time"))
  modList_contCarers[[i]] <- mod_contCar
  rsq_contCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  
  DAT_long_i_newCarer <- DAT_long_i[DAT_long_i$group %in% 3,]
  mod_newCar <- plm(lifeSatis  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                      + highCare_dum + 
                      + unEmpl_dum + noEmpl_dum + 
                      + lowWorr_dum + highWorr_dum, 
                    data=DAT_long_i_newCarer, model="within", index=c("pid","time"))
  modList_newCarers[[i]] <- mod_newCar
  rsq_newCarers[i] <- summary(mod_newCar)$r.squared["rsq"]
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
# noncaregivers
res_nonCar_LifeSatis <- summary(pool(as.mira(modList_nonCarers)), conf.int = TRUE)
res_nonCar <- cbind.data.frame(res_nonCar_LifeSatis[,1], round(res_nonCar_LifeSatis[,"estimate"],2), 
                               round(res_nonCar_LifeSatis[,"2.5 %"],2), round(res_nonCar_LifeSatis[,"97.5 %"],2))
colnames(res_nonCar) <- c("Var", "Est", "CIlow", "CIup")
res_nonCar
mean(rsq_noCarers)
# continuing caregivers
res_contCar_LifeSatis <- summary(pool(as.mira(modList_contCarers)), conf.int = TRUE)
res_contCar <- cbind.data.frame(res_contCar_LifeSatis[,1], round(res_contCar_LifeSatis[,"estimate"],2), 
                                round(res_contCar_LifeSatis[,"2.5 %"],2), round(res_contCar_LifeSatis[,"97.5 %"],2))
colnames(res_contCar) <- c("Var", "Est", "CIlow", "CIup")
res_contCar
mean(rsq_contCarers)
# new caregivers
res_newCar_LifeSatis <- summary(pool(as.mira(modList_newCarers)), conf.int = TRUE)
res_newCar <- cbind.data.frame(res_newCar_LifeSatis[,1], round(res_newCar_LifeSatis[,"estimate"],2), 
                               round(res_newCar_LifeSatis[,"2.5 %"],2), round(res_newCar_LifeSatis[,"97.5 %"],2))
colnames(res_newCar) <- c("Var", "Est", "CIlow", "CIup")
res_newCar
mean(rsq_newCarers)
