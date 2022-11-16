###############################################################################################
###############################################################################################
##
## Paper on the impact of informal care during spring 2020 in comparison to 2019 and 2018 on 
## - depression scores
## - general life satisfaction
## 
## Data Preparation & Descriptive Analysis
## for balanced panel 2019 - 2020
##
##
## SZ, 05.10.2022
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
library(lme4)
library(lmerTest)
library(parameters)
library(broom)
library(broom.mixed)
library(broomExtra)

#################################################################
# A. Data preparation 
#################################################################

# Read data
setwd("C:\\Users\\Freddie\\Documents\\CovS\\informalCare\\DATA")
PL <- read_dta("pl.dta")
COV <- read_dta("soep_cov_20210211_statav13.dta") # derived from plong in SOEP data v36 (simply write plong here, will also work)
COV2016 <- COV[COV$syear == 2016,]
COV2017 <- COV[COV$syear == 2017,]
COV2018 <- COV[COV$syear == 2018,]
COV2019 <- COV[COV$syear == 2019,]
COV2020 <- COV[COV$syear == 2020,]

# Care time info 2020
CT <- COV2020[,c("pid", "pli0046", "tranche")] # N=6694
colnames(CT)[2] <- "careTime_2020"
table(CT$careTime_2020, exclude=NULL)
CT$careTime_2020[CT$careTime_2020<0] <- NA

# Add care time info of 2018, 2019
K1 <- COV2019[,c("pid", "pli0046")]
colnames(K1)[2] <- "careTime_2019"
CT <- merge(CT, K1, by="pid", all.x=TRUE)
table(CT$careTime_2019, exclude=NULL)
CT$careTime_2019[CT$careTime_2019<0] <- NA

K2 <- COV2018[,c("pid", "pli0046")] # in 2016: 24% miss auf Caretime -> take 2018 and use depression score of 2016 as proxy for 2018
colnames(K2)[2] <- "careTime_2018"
CT <- merge(CT, K2, by="pid", all.x=TRUE) # base all participants in CoV Study
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

# Add information on depression: 2020
D2020 <- COV2020[,c("pid", "plh0339", "plh0340", "plh0341", "plh0342")]
colnames(D2020) <- c("pid", "depress1_2020", "depress2_2020","depress3_2020", "depress4_2020")
CT <- merge(CT, D2020, by="pid", all.x=TRUE)
table(CT$depress1_2020, exclude=NULL)
table(CT$depress2_2020, exclude=NULL)
table(CT$depress3_2020, exclude=NULL)
table(CT$depress4_2020, exclude=NULL)
CT$depress1_2020[CT$depress1_2020<0] <- NA
CT$depress2_2020[CT$depress2_2020<0] <- NA
CT$depress3_2020[CT$depress3_2020<0] <- NA
CT$depress4_2020[CT$depress4_2020<0] <- NA
CT$depress_2020 <- CT$depress1_2020 + CT$depress2_2020 + CT$depress3_2020 + CT$depress4_2020 # sum score 
table(CT$depress_2020, exclude = NULL)
CT <- CT[,!(colnames(CT) %in% c("depress1_2020", "depress2_2020","depress3_2020", "depress4_2020"))]

# Add employment status 2018 
E2018 <- COV2018[,c("pid", "plb0022_h")] # current employment status
colnames(E2018) <- c("pid", "employ_2018")
E2018$employ_2018 <- ifelse(E2018$employ_2018 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2018$employ_2018 %in% c(5,9), "non_unemployed", NA))
table(E2018$employ_2018, exclude=NULL)
E2018 <- E2018[E2018$pid %in% CT$pid,] # N=6683 of N=6694
PL2018 <- PL[PL$syear %in% 2018,]   # person questionnaire in long format, 2018
PL2018_unempl <- PL2018[, c("pid", "plb0021")] # registered unemployed at BA? (code 1 yes, code 2 no)
E2018 <- merge(E2018, PL2018_unempl, by="pid", all.x=TRUE)
#table(E2018$employ_2018, E2018$plb0021, exclude=NULL) # N=63 employed are registered unemployed, how this is possible
#pidStrange <- E2018[E2018$employ_2018 %in% "employed" & E2018$plb0021 %in% 1, "pid"]
#table(COV2018[COV2018$pid %in% pidStrange,"plb0022_h"]) # 1: fulltime, 2: parttime, 4: marginally employed, 7: FSJ or similar -> assign all to unemployed
E2018$employ_2018[E2018$plb0021 %in% 1] <- "unemployed"
E2018$employ_2018[E2018$employ_2018 %in% "non_unemployed" & !(E2018$plb0021 %in% 1)] <- "nonemployed"
table(E2018$employ_2018, exclude=NULL)
PL2018_self <- PL2018[, c("pid", "plb0568_v1")] # self-employed (value 1)?
E2018 <- merge(E2018, PL2018_self, by="pid", all.x=TRUE)
table(E2018$employ_2018, E2018$plb0568_v1, exclude=NULL) # Few inconsistencies: N=6 self-employed, N=28 worker, N=1 trainees, N=24 employees are registered unemployed (all self-reported)
CT <- merge(CT, E2018[,c("pid", "employ_2018")], by="pid", all.x = TRUE)
table(CT$employ_2018, exclude=NULL)

# Add emplyoment status 2019 
E2019 <- COV2019[,c("pid", "plb0022_h")] # current employment status
colnames(E2019) <- c("pid", "employ_2019")
E2019$employ_2019 <- ifelse(E2019$employ_2019 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2019$employ_2019 %in% c(5,9), "non_unemployed", NA))
table(E2019$employ_2019, exclude=NULL)
E2019 <- E2019[E2019$pid %in% CT$pid,] # N=6442 of N=6694
PL2019 <- PL[PL$syear %in% 2019,]   # person questionnaire in long format, 2019
PL2019_unempl <- PL2019[, c("pid", "plb0021")] # registered unemployed at BA? (code 1 yes, code 2 no)
E2019 <- merge(E2019, PL2019_unempl, by="pid", all.x=TRUE)
#table(E2019$employ_2019, E2019$plb0021, exclude=NULL) # N=58 employed are registered unemployed, how this is possible
#pidStrange <- E2019[E2019$employ_2019 %in% "employed" & E2019$plb0021 %in% 1, "pid"]
#table(COV2019[COV2019$pid %in% pidStrange,"plb0022_h"]) # 1: fulltime, 2: parttime, 3: vocational training, 4: marginally employed, 7: FSJ or similar -> assign all to unemployed
E2019$employ_2019[E2019$plb0021 %in% 1] <- "unemployed"
E2019$employ_2019[E2019$employ_2019 %in% "non_unemployed" & !(E2019$plb0021 %in% 1)] <- "nonemployed"
table(E2019$employ_2019, exclude=NULL)
PL2019_self <- PL2019[, c("pid", "plb0568_v1")] # self-employed (value 1)?
E2019 <- merge(E2019, PL2019_self, by="pid", all.x=TRUE)
table(E2019$employ_2019, E2019$plb0568_v1, exclude=NULL) # Few inconsistencies: N=10 self-employed, N=22 worker, N=1 trainees, N=22 employees are registered unemployed (all self-reported)
CT <- merge(CT, E2019[,c("pid", "employ_2019")], by="pid", all.x = TRUE)
table(CT$employ_2019, exclude=NULL)

# Add emplyoment status 2020 
E2020 <- COV2020[,c("pid", "plb0022_h")] # current employment status
colnames(E2020) <- c("pid", "employ_2020")
E2020$employ_2020 <- ifelse(E2020$employ_2020 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2020$employ_2020 %in% c(5,9), "non_unemployed", NA))
table(E2020$employ_2020, exclude=NULL)
E2020 <- E2020[E2020$pid %in% CT$pid,] 
PL2020_1 <- read_dta("P20_A-Q.dta")
PL2020_2 <- read_dta("P20_L2-3.dta")
PL2020_3 <- read_dta("P20_M1-2.dta")
PL2020_1 <- PL2020_1[, c("pnrfest", "palo")]
PL2020_2 <- PL2020_2[, c("pnrfest", "palo")]
PL2020_3 <- PL2020_3[, c("pnrfest", "palo")]
PL2020_unempl <- rbind.data.frame(PL2020_1,PL2020_2,PL2020_3)
colnames(PL2020_unempl)[1] <- "pid"
E2020 <- merge(E2020, PL2020_unempl, by="pid", all.x=TRUE)
#table(E2020$employ_2020, E2020$palo, exclude=NULL) # N=96 employed are registered unemployed, how this is possible
#pidStrange <- E2020[E2020$employ_2020 %in% "employed" & E2020$palo %in% 1, "pid"]
#table(COV2020[COV2020$pid %in% pidStrange,"plb0022_h"]) # 1: fulltime, 2: parttime, 3: vocational training, 4: marginally employed, 7: FSJ or similar, 12: short work -> assign all to unemployed
E2020$employ_2020[E2020$palo %in% 1] <- "unemployed"
E2020$employ_2020[E2020$employ_2020 %in% "non_unemployed" & !(E2020$palo %in% 1)] <- "nonemployed"
table(E2020$employ_2020, exclude=NULL)
COV2020_self <- COV2020[, c("pid", "pcovseink")] # self-employed (value 1)?
E2020 <- merge(E2020, COV2020_self, by="pid", all.x=TRUE)
E2020$employ_self <- ifelse(E2020$pcovseink %in% -2, 0,1)
table(E2020$employ_2020, E2020$employ_self, exclude=NULL) # Few inconsistencies (N=9): N=1 self-employed and nonemployed, N=8 self-employed and unemployed 
CT <- merge(CT, E2020[,c("pid", "employ_2020")], by="pid", all.x = TRUE)
table(CT$employ_2020, exclude=NULL)

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

# Worrying about own economic situation, 2020 
CT <- merge(CT, COV2020[, c("pid", "plh0033")], by="pid", all.x=TRUE)
table(CT$plh0033, exclude=NULL) # 1: grave worries, 2: some worries, 3: no worries
CT$plh0033[CT$plh0033 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0033"] <- "worriesEcon_2020"

# Life satisfaction, 2018 
CT <- merge(CT, COV2018[, c("pid", "plh0166")], by="pid", all.x=TRUE)
table(CT$plh0166, exclude=NULL) # 0 completely dissatisfied, ..., 10: completely satisfied
CT$plh0166[CT$plh0166 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0166"] <- "lifeSatis_2018"

# Life satisfaction, 2020 
CT <- merge(CT, COV2020[, c("pid", "plh0166")], by="pid", all.x=TRUE)
table(CT$plh0166, exclude=NULL) # 0 completely dissatisfied, ..., 10: completely satisfied
CT$plh0166[CT$plh0166 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0166"] <- "lifeSatis_2020"

# Life satisfaction, 2019 
PL2019_lifeSatis <- PL2019[, c("pid", "plh0182")] # general life satisfaction
CT <- merge(CT, PL2019_lifeSatis, by="pid", all.x=TRUE)
table(CT$plh0182, exclude=NULL) # 0 completely dissatisfied, ..., 10: completely satisfied
CT$plh0182[CT$plh0182 < 0] <- NA
colnames(CT)[colnames(CT) %in% "plh0182"] <- "lifeSatis_2019"

# Add weights (SOEP-CoV, CATI)
W <- COV2020[, c("pid", "phrf_cati_SOEPCoV")]
CT <- merge(CT, W, by="pid", all.x=TRUE)
table(CT$phrf_cati_SOEPCoV %in% 0) # N=27 cases with zero weights

# Clean data set
notIn <- c("tranche")
CT <- CT[,!(colnames(CT) %in% notIn)]

#################################################################
# B. Descriptives
#################################################################

# ---------------------------------------------------------------
# B1. Descriptives of balanced panel 
# ---------------------------------------------------------------

table(is.na(CT$lifeSatis_2019)) # 258 miss
m_lif2019 <- weighted.mean(CT$lifeSatis_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019
sd_lif2019 <- sqrt(wtd.var(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)], CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2019)])); sd_lif2019
se_lif2019 <- weighted_se(as.numeric(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2019)]), na.rm=TRUE); se_lif2019

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c1 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c1
sd_lif2019_c1 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c1
se_lif2019_c1 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c1

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c2 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c2
sd_lif2019_c2 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c2
se_lif2019_c2 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c2

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c3 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c3
sd_lif2019_c3 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c3 
se_lif2019_c3 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c3

CT_r <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >= 1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_lif2019_cont <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_cont 
sd_lif2019_cont <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_cont
se_lif2019_cont <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_cont 

CT_r <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_lif2019_new <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_new 
sd_lif2019_new <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_new
se_lif2019_new <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_new 

table(is.na(CT$lifeSatis_2020)) # 161 miss
m_lif2020 <- weighted.mean(CT$lifeSatis_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020
sd_lif2020 <- sqrt(wtd.var(CT$lifeSatis_2020[!is.na(CT$lifeSatis_2020)], CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2020)])); sd_lif2020
se_lif2020 <- weighted_se(as.numeric(CT$lifeSatis_2020[!is.na(CT$lifeSatis_2020)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2020)]), na.rm=TRUE); se_lif2020

CT_r <- CT[CT$careTime_2020 < 1 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2020)) 
m_lif2020_c1 <- weighted.mean(CT_r$lifeSatis_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020_c1
sd_lif2020_c1 <- sqrt(wtd.var(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)])); sd_lif2020_c1 
se_lif2020_c1 <- weighted_se(as.numeric(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)]), na.rm=TRUE); se_lif2020_c1

CT_r <- CT[CT$careTime_2020 >= 1 & CT$careTime_2020 <= 2 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2020)) 
m_lif2020_c2 <- weighted.mean(CT_r$lifeSatis_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020_c2 
sd_lif2020_c2 <- sqrt(wtd.var(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)])); sd_lif2020_c2 
se_lif2020_c2 <- weighted_se(as.numeric(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)]), na.rm=TRUE); se_lif2020_c2

CT_r <- CT[CT$careTime_2020 > 2 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2020)) 
m_lif2020_c3 <- weighted.mean(CT_r$lifeSatis_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020_c3
sd_lif2020_c3 <- sqrt(wtd.var(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)])); sd_lif2020_c3 
se_lif2020_c3 <- weighted_se(as.numeric(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)]), na.rm=TRUE); se_lif2020_c3

CT_r <- CT[!is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >= 1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_lif2020_cont <- weighted.mean(CT_r$lifeSatis_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020_cont 
sd_lif2020_cont <- sqrt(wtd.var(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)])); sd_lif2020_cont
se_lif2020_cont <- weighted_se(as.numeric(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)]), na.rm=TRUE); se_lif2020_cont 

CT_r <- CT[!is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_lif2020_new <- weighted.mean(CT_r$lifeSatis_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2020_new 
sd_lif2020_new <- sqrt(wtd.var(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)])); sd_lif2020_new
se_lif2020_new <- weighted_se(as.numeric(CT_r$lifeSatis_2020[!is.na(CT_r$lifeSatis_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2020)]), na.rm=TRUE); se_lif2020_new 

table(is.na(CT$depress_2019)) # 291 miss
m_dep2019 <- weighted.mean(CT$depress_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019
sd_dep2019 <- sqrt(wtd.var(CT$depress_2019[!is.na(CT$depress_2019)], CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2019)])); sd_dep2019
se_dep2019 <- weighted_se(as.numeric(CT$depress_2019[!is.na(CT$depress_2019)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2019)]), na.rm=TRUE); se_dep2019

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c1 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c1
sd_dep2019_c1 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c1
se_dep2019_c1 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c1

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c2 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c2
sd_dep2019_c2 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c2
se_dep2019_c2 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c2

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c3 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c3
sd_dep2019_c3 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c3
se_dep2019_c3 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c3

CT_r <- CT[!is.na(CT$depress_2019) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >= 1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_dep2019_cont <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_cont 
sd_dep2019_cont <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_cont
se_dep2019_cont <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_cont 

CT_r <- CT[!is.na(CT$depress_2019) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_dep2019_new <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_new 
sd_dep2019_new <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_new
se_dep2019_new <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_new 

table(is.na(CT$depress_2020)) # 291 miss
m_dep2020 <- weighted.mean(CT$depress_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2020
sd_dep2020 <- sqrt(wtd.var(CT$depress_2020[!is.na(CT$depress_2020)], CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2020)])); sd_dep2020
se_dep2020 <- weighted_se(as.numeric(CT$depress_2020[!is.na(CT$depress_2020)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2020)]), na.rm=TRUE); se_dep2020

CT_r <- CT[CT$careTime_2020 < 1 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$depress_2020)) 
m_dep2020_c1 <- weighted.mean(CT_r$depress_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2020_c1
sd_dep2020_c1 <- sqrt(wtd.var(CT_r$depress_2020[!is.na(CT_r$depress_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)])); sd_dep2020_c1 
se_dep2020_c1 <- weighted_se(as.numeric(CT_r$depress_2020[!is.na(CT_r$depress_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)]), na.rm=TRUE); se_dep2020_c1

CT_r <- CT[CT$careTime_2020 >= 1 & CT$careTime_2020 <= 2 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$depress_2020)) 
m_dep2020_c2 <- weighted.mean(CT_r$depress_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2020_c2
sd_dep2020_c2 <- sqrt(wtd.var(CT_r$depress_2020[!is.na(CT_r$depress_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)])); sd_dep2020_c2
se_dep2020_c2 <- weighted_se(as.numeric(CT_r$depress_2020[!is.na(CT_r$depress_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)]), na.rm=TRUE); se_dep2020_c2

CT_r <- CT[CT$careTime_2020 > 2 & !is.na(CT$careTime_2020),]; nrow(CT_r)
table(is.na(CT_r$depress_2020)) 
m_dep2020_c3 <- weighted.mean(CT_r$depress_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2020_c3
sd_dep2020_c3 <- sqrt(wtd.var(CT_r$depress_2020[!is.na(CT_r$depress_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)])); sd_dep2020_c3
se_dep2020_c3 <- weighted_se(as.numeric(CT_r$depress_2020[!is.na(CT_r$depress_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)]), na.rm=TRUE); se_dep2020_c3

CT_r <- CT[!is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >= 1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_dep2020_cont <- weighted.mean(CT_r$depress_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_cont 
sd_dep2020_cont <- sqrt(wtd.var(CT_r$depress_2020[!is.na(CT_r$depress_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)])); sd_dep2020_cont
se_dep2020_cont <- weighted_se(as.numeric(CT_r$depress_2020[!is.na(CT_r$depress_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)]), na.rm=TRUE); se_dep2020_cont 

CT_r <- CT[!is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
m_dep2020_new <- weighted.mean(CT_r$depress_2020, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2020_new 
sd_dep2020_new <- sqrt(wtd.var(CT_r$depress_2020[!is.na(CT_r$depress_2020)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)])); sd_dep2020_new
se_dep2020_new <- weighted_se(as.numeric(CT_r$depress_2020[!is.na(CT_r$depress_2020)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2020)]), na.rm=TRUE); se_dep2020_new 

# Test Means 2019 - 2020
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020),]
t_lif2019_20 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_lif2019_20
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2020<1 & CT$careTime_2019<1,]
t_lif2019_20_c1 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_lif2019_20_c1
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & (CT$careTime_2019>=1 & CT$careTime_2019<=2)  & (CT$careTime_2020>=1 & CT$careTime_2020<=2),]
t_lif2019_20_c2 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_lif2019_20_c2
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019>2 & CT$careTime_2020>2,]
t_lif2019_20_c3 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_lif2019_20_c3
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020),]
t_dep2019_20 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_dep2019_20
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2020<1 & CT$careTime_2019<1,]
t_dep2019_20_c1 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c1
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & (CT$careTime_2019>=1 & CT$careTime_2019<=2)  & (CT$careTime_2020>=1 & CT$careTime_2020<=2),]
t_dep2019_20_c2 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c2
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019>2 & CT$careTime_2020>2,]
t_dep2019_20_c3 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c3
CT_t <- CT[!is.na(CT$depress_2019)& !is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
t_dep2019_20_new <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_dep2019_20_new
CT_t <- CT[!is.na(CT$depress_2019)& !is.na(CT$depress_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >=1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
t_dep2019_20_cont <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_dep2019_20_cont
CT_t <- CT[!is.na(CT$lifeSatis_2019)& !is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 <1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
t_lif2019_20_new <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_lif2019_20_new
CT_t <- CT[!is.na(CT$lifeSatis_2019)& !is.na(CT$lifeSatis_2020) & !is.na(CT$careTime_2019) & !is.na(CT$careTime_2020) & CT$careTime_2019 >=1 & CT$careTime_2020 >= 1,]; nrow(CT_r)
t_lif2019_20_cont <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_lif2019_20_cont

w <- wtd.table(CT$sex, weights=CT$phrf_cati_SOEPCoV)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

age <- 2019 - CT$gebjahr
weighted.mean(age, CT$phrf_cati_SOEPCoV, na.rm=TRUE)

table(is.na(age)) # 11 miss, ok
ageKat <- ifelse(is.na(age), NA, ifelse(age<=40, 1, ifelse(age<60,2,3)))
w <- wtd.table(ageKat, weights=CT$phrf_cati_SOEPCoV)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$employ_2019)) # 252 miss -> quite some, make own category
#CT$employ_2019[is.na(CT$employ_2019)] <- -1
w <- wtd.table(CT$employ_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
#CT$employ_2019[CT$employ_2019 %in% -1] <- NA

table(is.na(CT$employ_2020)) # no miss
w <- wtd.table(CT$employ_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$worriesEcon_2019)) # 267 miss -> quite some, make own category
#CT$worriesEcon_2019[is.na(CT$worriesEcon_2019)] <- -1
w <- wtd.table(CT$worriesEcon_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
#CT$worriesEcon_2019[CT$worriesEcon_2019 %in% -1] <- NA

table(is.na(CT$worriesEcon_2020)) # 46 miss -> quite some, make own category
#CT$worriesEcon_2020[is.na(CT$worriesEcon_2020)] <- -1
w <- wtd.table(CT$worriesEcon_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2019))
careKat_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <= 2, 1, 3))) 
table(is.na(careKat_2019)) # 320 miss -> quite some, make own category
#careKat_2019[is.na(careKat_2019)] <- -1
w <- wtd.table(careKat_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2020))
careKat_2020 <- ifelse(is.na(CT$careTime_2020), NA, ifelse(CT$careTime_2020 < 1, 0, ifelse(CT$careTime_2020 <= 2, 1, 3))) 
table(is.na(careKat_2020)) # 2 miss -> quite some, make own category
#careKat_2020[is.na(careKat_2020)] <- -1
w <- wtd.table(careKat_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

weighted.mean(CT$careTime_2019, CT$phrf_cati_SOEPCoV, na.rm=TRUE)
weighted.mean(CT$careTime_2020, CT$phrf_cati_SOEPCoV, na.rm=TRUE)

# ---------------------------------------------------------------
# B2. Plot it
# ---------------------------------------------------------------
cats <- c("overall", "non-caregivers", "low-intensity", "high-intensity", "continuing caregiver", "new caregiver")
lifAll_2019 <- cbind(c(m_lif2019, m_lif2019_c1, m_lif2019_c2, m_lif2019_c3, m_lif2019_cont, m_lif2019_new), 
                     c(se_lif2019, se_lif2019_c1, se_lif2019_c2, se_lif2019_c3, se_lif2019_cont, se_lif2019_new), cats, 2019, "general life satisfaction")
lifAll_2020 <- cbind(c(m_lif2020, m_lif2020_c1, m_lif2020_c2, m_lif2020_c3, m_lif2020_cont, m_lif2020_new), 
                     c(se_lif2020, se_lif2020_c1, se_lif2020_c2, se_lif2020_c3, se_lif2020_cont, se_lif2020_new), cats, 2020, "general life satisfaction")
lif <- rbind(lifAll_2019, lifAll_2020)

depAll_2019 <- cbind(c(m_dep2019, m_dep2019_c1, m_dep2019_c2, m_dep2019_c3, m_dep2019_cont, m_dep2019_new), 
                     c(se_dep2019, se_dep2019_c1, se_dep2019_c2, se_dep2019_c3, se_dep2019_cont, se_dep2019_new), cats, 2019, "depression score")
depAll_2020 <- cbind(c(m_dep2020, m_dep2020_c1, m_dep2020_c2, m_dep2020_c3, m_dep2020_cont, m_dep2020_new), 
                     c(se_dep2020, se_dep2020_c1, se_dep2020_c2, se_dep2020_c3, se_dep2020_cont, se_dep2020_new), cats, 2020, "depression score")
dep <- rbind(depAll_2019, depAll_2020)
res <- as.data.frame(rbind(lif,dep))
colnames(res) <- c("M", "SE", "Categories", "Year", "Outcome")
res$M <- as.numeric(res$M)
res$SE <- as.numeric(res$SE)
res$Categories <- as.factor(res$Categories)
res$Year <- as.factor(res$Year)
res$Outcome <- as.factor(res$Outcome)
res$bord.low <- res$M - 1.96*res$SE
res$bord.up <- res$M + 1.96*res$SE
res$Categories <- factor(res$Categories, levels=rev(cats))

zp <- ggplot(res,aes(colour=Year)) 
zp <- zp + geom_pointrange(aes(x = Categories, y = M, ymin = bord.low, ymax = bord.up),
                           lwd = 0.7, position = position_dodge(width = 1/2)) 
zp <- zp + geom_vline(xintercept=2.5, color = "grey10")
zp <- zp + geom_vline(xintercept=5.5, color = "grey10")
zp <- zp + facet_grid(cols=vars(Outcome)) + scale_color_manual(values=c("darkred", "darkblue"))
zp <- zp + xlab("") + ylab("") 
zp <- zp + theme(axis.text.x=element_text(size=10)) 
zp <- zp + theme(strip.text.x = element_text(size = 12))
zp <- zp + theme(axis.text.y = element_text(size = 12))
zp <- zp + coord_flip()
zp

#################################################################
# C. Deeper Look at Care Intensities
#################################################################

tab <- table(CT[,"careTime_2019"], CT[,"careTime_2020"])
mat <- matrix(0, nrow=25, ncol=25)
for(i in 1:dim(tab)[1]){
  for(j in 1:dim(tab)[2]){
      mat[as.numeric(rownames(tab)[i])+1, as.numeric(colnames(tab)[j])+1] <- tab[i,j]
  }
}
mtab <- cbind.data.frame(rep(0:24, each=25), rep(0:24, 25), as.vector(t(mat)))
colnames(mtab) <- c("careInt2019", "careInt2020", "count") 
mtab$count[mtab$count %in% 0] <- NA
mtab %>% 
  ggplot() + 
  aes(y = careInt2019, x = careInt2020, size = count) +
  geom_point() 

# non caregivers
noncarers <- CT$careTime_2019 %in% 0 & CT$careTime_2020 %in% 0
tab_nonCarers <- table(CT[noncarers,"careTime_2019"], CT[noncarers,"careTime_2020"])
mat_non <- matrix(0, nrow=25, ncol=25)
for(i in 1:dim(tab_nonCarers)[1]){
  for(j in 1:dim(tab_nonCarers)[2]){
    mat_non[as.numeric(rownames(tab_nonCarers)[i])+1, as.numeric(colnames(tab_nonCarers)[j])+1] <- tab_nonCarers[i,j]
  }
}
mtab_non <- cbind.data.frame(rep(0:24, each=25), rep(0:24, 25), as.vector(t(mat_non)))
colnames(mtab_non) <- c("careInt2019", "careInt2020", "Frequency") 
mtab_non <- cbind.data.frame(mtab_non, "none")
colnames(mtab_non)[4] <- "group" 

# new care givers
newcarers <- CT$careTime_2019 %in% 0 & CT$careTime_2020 > 0
tab_newCarers <- table(CT[newcarers,"careTime_2019"], CT[newcarers,"careTime_2020"])
mat_new <- matrix(0, nrow=25, ncol=25)
for(i in 1:dim(tab_newCarers)[1]){
  for(j in 1:dim(tab_newCarers)[2]){
    mat_new[as.numeric(rownames(tab_newCarers)[i])+1, as.numeric(colnames(tab_newCarers)[j])+1] <- tab_newCarers[i,j]
  }
}
mtab_new <- cbind.data.frame(rep(0:24, each=25), rep(0:24, 25), as.vector(t(mat_new)))
colnames(mtab_new) <- c("careInt2019", "careInt2020", "Frequency") 
mtab_new <- cbind.data.frame(mtab_new, "new")
colnames(mtab_new)[4] <- "group" 

# cont. care givers
contcarers <- CT$careTime_2019 > 0 & CT$careTime_2020 > 0
tab_contCarers <- table(CT[contcarers,"careTime_2019"], CT[contcarers,"careTime_2020"])
mat_cont <- matrix(0, nrow=25, ncol=25)
for(i in 1:dim(tab_contCarers)[1]){
  for(j in 1:dim(tab_contCarers)[2]){
    mat_cont[as.numeric(rownames(tab_contCarers)[i])+1, as.numeric(colnames(tab_contCarers)[j])+1] <- tab_contCarers[i,j]
  }
}
mtab_cont <- cbind.data.frame(rep(0:24, each=25), rep(0:24, 25), as.vector(t(mat_cont)))
colnames(mtab_cont) <- c("careInt2019", "careInt2020", "Frequency") 
mtab_cont <- cbind.data.frame(mtab_cont, "continuing")
colnames(mtab_cont)[4] <- "group" 

# giving up care givers
giveupcarers <- CT$careTime_2019 > 0 & CT$careTime_2020 %in% 0
tab_upCarers <- table(CT[giveupcarers,"careTime_2019"], CT[giveupcarers,"careTime_2020"])
mat_up <- matrix(0, nrow=25, ncol=25)
for(i in 1:dim(tab_upCarers)[1]){
  for(j in 1:dim(tab_upCarers)[2]){
    mat_up[as.numeric(rownames(tab_upCarers)[i])+1, as.numeric(colnames(tab_upCarers)[j])+1] <- tab_upCarers[i,j]
  }
}
mtab_up <- cbind.data.frame(rep(0:24, each=25), rep(0:24, 25), as.vector(t(mat_up)))
colnames(mtab_up) <- c("careInt2019", "careInt2020", "Frequency") 
mtab_up <- cbind.data.frame(mtab_up, "giving up")
colnames(mtab_up)[4] <- "group" 

#mtab <- rbind.data.frame(mtab_non,mtab_new,mtab_cont,mtab_up)
mtab <- rbind.data.frame(mtab_new,mtab_cont,mtab_up)

mtab$Frequency[mtab$Frequency %in% 0] <- NA
mtab %>% 
  ggplot() + 
  aes(y = careInt2019, x = careInt2020, size = Frequency, colour = factor(group)) +  
  geom_point() +
  geom_hline(yintercept=2, color = "grey40") +
  geom_vline(xintercept=2, color = "grey40") + 
  ylab("care intensity 2019") + xlab("care intensity 2020") +
  scale_colour_discrete("Caregivers")

#################################################################
# D. Impute Missings
#################################################################

CC <- CT[, c("careTime_2019", "careTime_2020", 
             "depress_2019", "depress_2020", 
             "employ_2019", "employ_2020", 
             "worriesEcon_2019", "worriesEcon_2020",
             "lifeSatis_2019", "lifeSatis_2020")]
table(complete.cases(CC))

MD <- md.pattern(CT, plot=FALSE)
round(MD[nrow(MD),]/nrow(CT),2) # max. 9% missing values in one of the variables
table(complete.cases(CC))/nrow(CC) # ~91% complete cases

# Plot missingness pattern
gg_miss_upset(CC, nsets=10)

# Little's MCAR test 
mcar_test(CT) # H_0 reject -> no MCAR

# add aux variables
CTI <- as.data.frame(CT)
# educational attainment (last observation carried forward)
casM <- as.data.frame(COV[COV$syear %in% 2011:2019, c("pid", "syear", "pgcasmin")])
casM <- casM[order(casM$pid),]
casM <- casM[setdiff(1:nrow(casM), which(is.na(casM$pgcasmin), arr.ind=TRUE)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pgcasmin<0)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pid<0)),]
casM <- casM[order(casM$pid, casM$syear, decreasing = TRUE),]
casM <- casM[!duplicated(casM$pid),]
table(CTI$pid %in% casM$pid) # N=109 miss
CTI <- merge(CTI, casM[, c("pid", "pgcasmin")], by="pid", all.x=TRUE)
table(CTI$pgcasmin, exclude=NULL)
CTI$edu <- ifelse(CTI$pgcasmin %in% c(0, 1, 2, 4), 0, ifelse(CTI$pgcasmin %in% c(3,5,6,7), 1, ifelse(CTI$pgcasmin %in% c(8,9), 2, NA))) 
table(CTI$edu, exclude=NULL)
CTI <- CTI[,-which(colnames(CTI) %in% "pgcasmin")]

# household type (last observation carried forward)
hidHT <- as.data.frame(COV[COV$syear %in% 2011:2019, c("pid", "syear", "hgtyp1hh")]) 
hidHT <- hidHT[order(hidHT$pid),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(is.na(hidHT$hgtyp1hh), arr.ind=TRUE)),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(hidHT$hgtyp1hh<0)),]
hidHT <- hidHT[order(hidHT$pid, hidHT$syear, decreasing = TRUE),]
hidHT <- hidHT[!duplicated(hidHT$pid),]
table(CTI$pid %in% hidHT$pid) # N=6 miss
CTI <- merge(CTI, hidHT[, c("pid", "hgtyp1hh")], by="pid", all.x=TRUE)
table(CTI$hgtyp1hh, exclude=NULL) # no miss

# migration background (last observation carried forward)
mig <- as.data.frame(COV[COV$syear %in% 2011:2019, c("pid", "syear", "migback")])
mig <- mig[order(mig$pid),]
mig <- mig[setdiff(1:nrow(mig), which(is.na(mig$migback), arr.ind=TRUE)),]
mig <- mig[setdiff(1:nrow(mig), which(mig$migback<0)),]
mig <- mig[order(mig$pid, mig$syear, decreasing = TRUE),]
mig <- mig[!duplicated(mig$pid),]
table(CTI$pid %in% mig$pid) # N=10 miss
CTI <- merge(CTI, mig[, c("pid", "migback")], by="pid", all.x=TRUE) # only for surveyed person migback
table(CTI$migback, exclude=NULL) 

# federal state
bl <- as.data.frame(COV[COV$syear %in% 2011:2019, c("pid", "syear", "bula")])
bl <- bl[order(bl$pid),]
bl <- bl[setdiff(1:nrow(bl), which(is.na(bl$bula), arr.ind=TRUE)),]
bl <- bl[setdiff(1:nrow(bl), which(bl$bula<0)),]
bl <- bl[order(bl$pid, bl$syear, decreasing = TRUE),]
bl <- bl[!duplicated(bl$pid),]
table(CTI$pid %in% bl$pid) # N=6 miss
CTI <- merge(CTI, bl[,c("pid", "bula")], by="pid", all.x=TRUE) # only for surveyed person migback
table(CTI$bula, exclude=NULL) 

# conduct imputation
fact <- c("sex", "employ_2018", "employ_2019", "employ_2020",
          "edu", "migback", "bula", "hgtyp1hh")
num <- c("careTime_2018", "careTime_2019", "careTime_2020", "gebjahr", 
         "depress_2018", "depress_2019", "depress_2020", 
         "lifeSatis_2018", "lifeSatis_2019", "lifeSatis_2020", 
         "worriesEcon_2018", "worriesEcon_2019", "worriesEcon_2020",
         "phrf_cati_SOEPCoV")
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

