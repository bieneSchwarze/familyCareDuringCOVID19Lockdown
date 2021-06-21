###############################################################################################
###############################################################################################
##
## Paper on the impact of informal care during spring 2020 in comparison to 2019 and 2018 on 
## - depression scores
## - general life satisfaction
## 
## Data Preparation & Analysis
## for balanced panel 2019 - 2020
##
##
## SZ, 21.06.2021
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
# No household income yet available for 2020. Use instead hh income of previous year

# Read data
setwd("C:\\Users\\Freddie\\Documents\\CovS\\informalCare\\DATA")
PL <- read_dta("pl.dta")
COV <- read_dta("soep_cov_20210211_statav13.dta") # derived from plong in SOEP data v36 (simply write plong here, will also work)
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

# Add gender and sex (last observation carried forward)
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

# Add emplyoment status 2019 
E2019 <- COV2019[,c("pid", "plb0022_h")] # current employment status
colnames(E2019) <- c("pid", "employ_2019")
E2019$employ_2019 <- ifelse(E2019$employ_2019 %in% c(1,2,3,4,6,7,8,10,11,12), "employed", ifelse(E2019$employ_2019 %in% c(5,9), "non_unemployed", NA))
table(E2019$employ_2019, exclude=NULL)
E2019 <- E2019[E2019$pid %in% CT$pid,] # N=6442 of N=6694
PL2019 <- PL[PL$syear %in% 2019,]   # person questionnaire in long format, 2019
PL2019_unempl <- PL2019[, c("pid", "plb0021")] # registered unemployed at BA? (code 1 yes, code 2 no)
E2019 <- merge(E2019, PL2019_unempl, by="pid", all.x=TRUE)
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
PL2020_1 <- read_dta("P20_A-Q.dta")  # this is raw data from v37 (currently in preparation)
PL2020_2 <- read_dta("P20_L2-3.dta") # this is raw data from v37 (currently in preparation)
PL2020_3 <- read_dta("P20_M1-2.dta") # this is raw data from v37 (currently in preparation)
PL2020_1 <- PL2020_1[, c("pnrfest", "palo")]
PL2020_2 <- PL2020_2[, c("pnrfest", "palo")]
PL2020_3 <- PL2020_3[, c("pnrfest", "palo")]
PL2020_unempl <- rbind.data.frame(PL2020_1,PL2020_2,PL2020_3)
colnames(PL2020_unempl)[1] <- "pid"
E2020 <- merge(E2020, PL2020_unempl, by="pid", all.x=TRUE)
E2020$employ_2020[E2020$palo %in% 1] <- "unemployed"
E2020$employ_2020[E2020$employ_2020 %in% "non_unemployed" & !(E2020$palo %in% 1)] <- "nonemployed"
table(E2020$employ_2020, exclude=NULL)
COV2020_self <- COV2020[, c("pid", "pcovseink")] # self-employed (value 1)?
E2020 <- merge(E2020, COV2020_self, by="pid", all.x=TRUE)
E2020$employ_self <- ifelse(E2020$pcovseink %in% -2, 0,1)
table(E2020$employ_2020, E2020$employ_self, exclude=NULL) # Few inconsistencies (N=9): N=1 self-employed and nonemployed, N=8 self-employed and unemployed 
CT <- merge(CT, E2020[,c("pid", "employ_2020")], by="pid", all.x = TRUE)
table(CT$employ_2020, exclude=NULL)

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

table(is.na(CT$lifeSatis_2019)) 
m_lif2019 <- weighted.mean(CT$lifeSatis_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019
sd_lif2019 <- sqrt(wtd.var(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)], CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2019)])); sd_lif2019
se_lif2019 <- weighted_se(as.numeric(CT$lifeSatis_2019[!is.na(CT$lifeSatis_2019)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$lifeSatis_2019)]), na.rm=TRUE); se_lif2019
v1 <- CT$lifeSatis_2019 + 1
v2 <- CT$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT$careTime_2020), NA, ifelse(CT$careTime_2020 < 1, 0, ifelse(CT$careTime_2020 <=2, 1, 2)))
CT_c <- CT[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2019 + 1
v2 <- CT_c$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2019 + 1
v2 <- CT_nc$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c1 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c1
sd_lif2019_c1 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c1
se_lif2019_c1 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c1
v1 <- CT_r$lifeSatis_2019 + 1
v2 <- CT_r$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2019 + 1
v2 <- CT_c$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2019 + 1
v2 <- CT_nc$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c2 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c2
sd_lif2019_c2 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c2
se_lif2019_c2 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c2
v1 <- CT_r$lifeSatis_2019 + 1
v2 <- CT_r$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2019 + 1
v2 <- CT_c$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2019 + 1
v2 <- CT_nc$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$lifeSatis_2019)) 
m_lif2019_c3 <- weighted.mean(CT_r$lifeSatis_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_lif2019_c3
sd_lif2019_c3 <- sqrt(wtd.var(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)])); sd_lif2019_c3 
se_lif2019_c3 <- weighted_se(as.numeric(CT_r$lifeSatis_2019[!is.na(CT_r$lifeSatis_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$lifeSatis_2019)]), na.rm=TRUE); se_lif2019_c3
v1 <- CT_r$lifeSatis_2019 + 1
v2 <- CT_r$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$lifeSatis_2019 + 1
v2 <- CT_c$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$lifeSatis_2019 + 1
v2 <- CT_nc$lifeSatis_2020 + 1
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)

table(is.na(CT$lifeSatis_2020)) 
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

table(is.na(CT$depress_2019)) 
m_dep2019 <- weighted.mean(CT$depress_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019
sd_dep2019 <- sqrt(wtd.var(CT$depress_2019[!is.na(CT$depress_2019)], CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2019)])); sd_dep2019
se_dep2019 <- weighted_se(as.numeric(CT$depress_2019[!is.na(CT$depress_2019)]), as.numeric(CT$phrf_cati_SOEPCoV[!is.na(CT$depress_2019)]), na.rm=TRUE); se_dep2019
v1 <- CT$depress_2019 
v2 <- CT$depress_2020
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT$careTime_2020), NA, ifelse(CT$careTime_2020 < 1, 0, ifelse(CT$careTime_2020 <=2, 1, 2)))
CT_c <- CT[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$depress_2019 
v2 <- CT_c$depress_2020 
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$depress_2019 
v2 <- CT_nc$depress_2020 
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)

CT_r <- CT[CT$careTime_2019 < 1 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c1 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c1
sd_dep2019_c1 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c1
se_dep2019_c1 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c1
v1 <- CT_r$depress_2019 
v2 <- CT_r$depress_2020
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$depress_2019 
v2 <- CT_c$depress_2020
ch_c <- (v2-v1) / v1
weighted.mean(ch_c, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$depress_2019 
v2 <- CT_nc$depress_2020
ch_nc <- (v2-v1) / v1
weighted.mean(ch_nc, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)
wtd.t.test(x=ch_c, y=ch_nc, weight=CT_c$phrf_cati_SOEPCoV, weighty=CT_nc$phrf_cati_SOEPCoV, samedata=FALSE, bootse=TRUE)

CT_r <- CT[CT$careTime_2019 >= 1 & CT$careTime_2019 <= 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c2 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c2
sd_dep2019_c2 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c2
se_dep2019_c2 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c2
v1 <- CT_r$depress_2019 
v2 <- CT_r$depress_2020
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$depress_2019 
v2 <- CT_c$depress_2020
ch_c <- (v2-v1) / v1
weighted.mean(ch_c, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$depress_2019 
v2 <- CT_nc$depress_2020
ch_nc <- (v2-v1) / v1
weighted.mean(ch_nc, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)
wtd.t.test(x=ch_c, y=ch_nc, weight=CT_c$phrf_cati_SOEPCoV, weighty=CT_nc$phrf_cati_SOEPCoV, samedata=FALSE, bootse=TRUE)

CT_r <- CT[CT$careTime_2019 > 2 & !is.na(CT$careTime_2019),]; nrow(CT_r)
table(is.na(CT_r$depress_2019)) 
m_dep2019_c3 <- weighted.mean(CT_r$depress_2019, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE); m_dep2019_c3
sd_dep2019_c3 <- sqrt(wtd.var(CT_r$depress_2019[!is.na(CT_r$depress_2019)], CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)])); sd_dep2019_c3
se_dep2019_c3 <- weighted_se(as.numeric(CT_r$depress_2019[!is.na(CT_r$depress_2019)]), as.numeric(CT_r$phrf_cati_SOEPCoV[!is.na(CT_r$depress_2019)]), na.rm=TRUE); se_dep2019_c3
v1 <- CT_r$depress_2019 
v2 <- CT_r$depress_2020
ch <- (v2-v1) / v1
weighted.mean(ch, weights=CT_r$phrf_cati_SOEPCoV, na.rm=TRUE)
cAT_2019 <- ifelse(is.na(CT_r$careTime_2019), NA, ifelse(CT_r$careTime_2019 < 1, 0, ifelse(CT_r$careTime_2019 <=2, 1, 2)))
cAT_2020 <- ifelse(is.na(CT_r$careTime_2020), NA, ifelse(CT_r$careTime_2020 < 1, 0, ifelse(CT_r$careTime_2020 <=2, 1, 2)))
CT_c <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 != cAT_2020),]; nrow(CT_c)
CT_nc <- CT_r[!is.na(cAT_2019) & !is.na(cAT_2020) & (cAT_2019 == cAT_2020),]; nrow(CT_nc)
v1 <- CT_c$depress_2019 
v2 <- CT_c$depress_2020
ch_c <- (v2-v1) / v1
weighted.mean(ch_c, weights=CT_c$phrf_cati_SOEPCoV, na.rm=TRUE)
v1 <- CT_nc$depress_2019 
v2 <- CT_nc$depress_2020
ch_nc <- (v2-v1) / v1
weighted.mean(ch_nc, weights=CT_nc$phrf_cati_SOEPCoV, na.rm=TRUE)
wtd.t.test(x=ch_c[!is.na(ch_c)], y=ch_nc[!is.na(ch_nc)], 
           weight=CT_c$phrf_cati_SOEPCoV[!is.na(ch_c)], weighty=CT_nc$phrf_cati_SOEPCoV[!is.na(ch_nc)], samedata=FALSE, bootse=TRUE)

table(is.na(CT$depress_2020)) 
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

# Test Means 2019 - 2020
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020),]
t_lif2019_20 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_lif2019_20
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & CT$careTime_2019<1,]
t_lif2019_20_c1 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_lif2019_20_c1
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & CT$careTime_2019>=1 & CT$careTime_2019<=2,]
t_lif2019_20_c2 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_lif2019_20_c2
CT_t <- CT[!is.na(CT$lifeSatis_2019) & !is.na(CT$lifeSatis_2020) & CT$careTime_2019>2,]
t_lif2019_20_c3 <- wtd.t.test(x=CT_t$lifeSatis_2019-CT_t$lifeSatis_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_lif2019_20_c3
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020),]
t_dep2019_20 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=TRUE);t_dep2019_20
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & CT$careTime_2019<1,]
t_dep2019_20_c1 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c1
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & CT$careTime_2019>=1 & CT$careTime_2019<=2,]
t_dep2019_20_c2 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c2
CT_t <- CT[!is.na(CT$depress_2019) & !is.na(CT$depress_2020) & CT$careTime_2019>2,]
t_dep2019_20_c3 <- wtd.t.test(x=CT_t$depress_2019-CT_t$depress_2020, weight=CT_t$phrf_cati_SOEPCoV, bootse=FALSE);t_dep2019_20_c3

w <- wtd.table(CT$sex, weights=CT$phrf_cati_SOEPCoV)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

age <- 2020 - CT$gebjahr
weighted.mean(age, CT$phrf_cati_SOEPCoV, na.rm=TRUE)

table(is.na(age)) # 11 miss, ok
ageKat <- ifelse(is.na(age), NA, ifelse(age<=40, 1, ifelse(age<60,2,3)))
w <- wtd.table(ageKat, weights=CT$phrf_cati_SOEPCoV)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$employ_2019)) 
w <- wtd.table(CT$employ_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$employ_2020))
w <- wtd.table(CT$employ_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$worriesEcon_2019)) 
w <- wtd.table(CT$worriesEcon_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$worriesEcon_2020)) 
w <- wtd.table(CT$worriesEcon_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2019))
careKat_2019 <- ifelse(is.na(CT$careTime_2019), NA, ifelse(CT$careTime_2019 < 1, 0, ifelse(CT$careTime_2019 <= 2, 1, 3))) 
table(is.na(careKat_2019)) 
w <- wtd.table(careKat_2019, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(is.na(CT$careTime_2020))
careKat_2020 <- ifelse(is.na(CT$careTime_2020), NA, ifelse(CT$careTime_2020 < 1, 0, ifelse(CT$careTime_2020 <= 2, 1, 3))) 
table(is.na(careKat_2020)) 
w <- wtd.table(careKat_2020, weights=CT$phrf_cati_SOEPCoV, na.rm=TRUE)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

weighted.mean(CT$careTime_2019, CT$phrf_cati_SOEPCoV, na.rm=TRUE)
weighted.mean(CT$careTime_2020, CT$phrf_cati_SOEPCoV, na.rm=TRUE)

# ---------------------------------------------------------------
# B2. Plot it
# ---------------------------------------------------------------
cats <- c("all", "<1 hour/day", "1-2 hours/day", ">2 hours/day")
lifAll_2019 <- cbind(c(m_lif2019, m_lif2019_c1, m_lif2019_c2, m_lif2019_c3), 
                     c(se_lif2019, se_lif2019_c1, se_lif2019_c2, se_lif2019_c3), cats, 2019, "general life satisfaction")
lifAll_2020 <- cbind(c(m_lif2020, m_lif2020_c1, m_lif2020_c2, m_lif2020_c3), 
                     c(se_lif2020, se_lif2020_c1, se_lif2020_c2, se_lif2020_c3), cats, 2020, "general life satisfaction")
lif <- rbind(lifAll_2019, lifAll_2020)

depAll_2019 <- cbind(c(m_dep2019, m_dep2019_c1, m_dep2019_c2, m_dep2019_c3), 
                     c(se_dep2019, se_dep2019_c1, se_dep2019_c2, se_dep2019_c3), cats, 2019, "depression score")
depAll_2020 <- cbind(c(m_dep2020, m_dep2020_c1, m_dep2020_c2, m_dep2020_c3), 
                     c(se_dep2020, se_dep2020_c1, se_dep2020_c2, se_dep2020_c3), cats, 2020, "depression score")
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
res$Categories <- factor(res$Categories, levels=cats)

zp <- ggplot(res,aes(colour=Year)) 
zp <- zp + geom_pointrange(aes(x = Categories, y = M, ymin = bord.low, ymax = bord.up),
                           lwd = 0.7, position = position_dodge(width = 1/2)) 
zp <- zp + facet_grid(rows=vars(Outcome)) + scale_color_manual(values=c("darkred", "darkblue"))
zp <- zp + xlab("") + ylab("") 
zp <- zp + theme(axis.text.x=element_text(size=12)) 
zp <- zp + theme(strip.text.y = element_text(size = 11))
zp

#################################################################
# C. Impute Missings
#################################################################
CC <- CT[, c("careTime_2019", "careTime_2020", 
             "depress_2019", "depress_2020", 
             "employ_2019", "employ_2020", 
             "worriesEcon_2019", "worriesEcon_2020",
             "lifeSatis_2019", "lifeSatis_2020")]
table(complete.cases(CC))

MD <- md.pattern(CT, plot=FALSE)
round(MD[nrow(MD),]/nrow(CT),2) # max. 23% missing values in one of the variables: in "depress_2018"
table(complete.cases(CT))/nrow(CT) # ~68% complete cases

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
fact <- c("sex", "employ_2019", "employ_2020",
          "edu", "migback", "bula", "hgtyp1hh")
num <- c("careTime_2019", "careTime_2020", "gebjahr", 
         "depress_2019", "depress_2020", 
         "lifeSatis_2019", "lifeSatis_2020", 
         "worriesEcon_2019", "worriesEcon_2020",
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

#################################################################
# D. Fixed Effects
#################################################################

# Function to pool results
poolIt <- function(modList, n){
  #n <- nrow(DAT_long_i)
  #modList <- modList
  m <- length(modList)
  betas <- NULL
  vars <- NULL
  for(i in 1:m){
    #i <- 1
    mi <- summary(modList[[i]])
    betas <- cbind(betas, as.numeric(coef(mi)[,1]))
    #vars_within <- diag(vcovHC(modList[[i]], method="white1"))
    vars <- cbind(vars, as.numeric(coef(mi)[,2]^2)) 
  }
  rownames(betas) <- names(coef(mi)[,1])
  rownames(vars) <- names(coef(mi)[,1])  
  betasPool <- apply(betas, 1, mean)
  varWithin <- apply(vars, 1, mean)
  varBetween <- apply((betas - betasPool)^2, 1, sum)/(n-1)
  varTotal <- varWithin + varBetween + varBetween/m
  seTotal <- sqrt(varTotal)
  lambda <- (varBetween + (varBetween / m))/(varTotal)
  dfold <- (m-1)/lambda^2
  k <- length(betasPool)
  dfobserved <- ((n-k)+1)/((n-k)+3) * (n-k)*(1-lambda) 
  dfadjusted <- (dfold*dfobserved)/(dfold+dfobserved)
  alpha <- 0.1
  td <- qt(1-alpha/2, dfadjusted)
  ci_low <- betasPool - td*seTotal
  ci_up <- betasPool + td*seTotal
  res <- round(cbind( betasPool, ci_low, ci_up),3)
  rownames(res) <- rownames(betas)
  return(res)
}

# --------------------------------------------------------------
# D.1 DV Depression Score for 2019, 2020 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
modList_Pred <- vector(length=imp$m, mode="list") 
for(i in 1:imp$m){
  
  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
  DAT_fe_i <- D_i[,c("pid", "sex", "gebjahr", "phrf_cati_SOEPCoV", 
                     "careTime_2019", "careTime_2020",
                     "employ_2019", "employ_2020",
                     "worriesEcon_2019", "worriesEcon_2020",
                     "depress_2019", "depress_2020",
                     "lifeSatis_2019",  "lifeSatis_2020")] 
  colnames(DAT_fe_i)[5:6] <- c("careTime.2019", "careTime.2020")
  colnames(DAT_fe_i)[7:8] <- c("employ.2019", "employ.2020")
  colnames(DAT_fe_i)[9:10] <- c("worriesEcon.2019", "worriesEcon.2020") 
  colnames(DAT_fe_i)[11:12] <- c("depress.2019", "depress.2020")
  colnames(DAT_fe_i)[13:14] <- c("lifeSatis.2019", "lifeSatis.2020")
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  DAT_long_i$careCat <- ifelse(is.na(DAT_long_i$careTime), NA, ifelse(DAT_long_i$careTime <1, 0, ifelse(DAT_long_i$careTime <=2, 1, 2)))
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- as.factor(DAT_long_i$time)
  DAT_long_i$time <- relevel(DAT_long_i$time, ref="1")
  DAT_long_i$employ <- as.factor(DAT_long_i$employ)
  DAT_long_i$employ <- relevel(DAT_long_i$employ, ref="employed")
  DAT_long_i$worriesEcon <- as.factor(DAT_long_i$worriesEcon)
  DAT_long_i$worriesEcon <- relevel(DAT_long_i$worriesEcon, ref="3")
  DAT_long_i$timeCare <- ifelse(is.na(DAT_long_i$careCat), NA, 
                                  ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 0, 0,                      
                                     ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 1, 0, 
                                        ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 2, 0,   
                                          ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 0, 0,                        
                                            ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 1, 1, 
                                               ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 2, 2, NA)))))))                             
  MODEL_i <- plm(depress  ~  time + employ + worriesEcon + as.factor(careCat) + as.factor(timeCare), 
                 model = "within", index=c("pid","time"),
                 data = DAT_long_i)
  #summary(MODEL_i, vcov=vcovHC(MODEL_i, method="white1"))   
  modList[[i]] <- MODEL_i 
  cat("\n ------------- \n")
}
n <- nrow(CT)
res <- poolIt(modList, n)
round(res,2)
rs <- rep(NA, imp$m); for(i in 1:imp$m) {rs[i] <- summary(modList[[i]])$r.squared[1]}; mean(rs)

# --------------------------------------------------------------
# D.2 DV Life satisfaction for 2019, 2020 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
modList_Pred <- vector(length=imp$m, mode="list") 
for(i in 1:imp$m){

  #i <- 1  
  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
  DAT_fe_i <- D_i[,c("pid", "sex", "gebjahr", "phrf_cati_SOEPCoV", 
                     "careTime_2019", "careTime_2020",
                     "employ_2019", "employ_2020",
                     "worriesEcon_2019", "worriesEcon_2020",
                     "depress_2019", "depress_2020",
                     "lifeSatis_2019",  "lifeSatis_2020")] 
  colnames(DAT_fe_i)[5:6] <- c("careTime.2019", "careTime.2020")
  colnames(DAT_fe_i)[7:8] <- c("employ.2019", "employ.2020")
  colnames(DAT_fe_i)[9:10] <- c("worriesEcon.2019", "worriesEcon.2020") 
  colnames(DAT_fe_i)[11:12] <- c("depress.2019", "depress.2020")
  colnames(DAT_fe_i)[13:14] <- c("lifeSatis.2019", "lifeSatis.2020")
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  DAT_long_i$careCat <- ifelse(is.na(DAT_long_i$careTime), NA, ifelse(DAT_long_i$careTime <1, 0, ifelse(DAT_long_i$careTime <=2, 1, 2)))
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- as.factor(DAT_long_i$time)
  DAT_long_i$time <- relevel(DAT_long_i$time, ref="1")
  DAT_long_i$employ <- as.factor(DAT_long_i$employ)
  DAT_long_i$employ <- relevel(DAT_long_i$employ, ref="employed")
  DAT_long_i$worriesEcon <- as.factor(DAT_long_i$worriesEcon)
  DAT_long_i$worriesEcon <- relevel(DAT_long_i$worriesEcon, ref="3")
  DAT_long_i$timeCare <- ifelse(is.na(DAT_long_i$careCat), NA, 
                                ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 0, 0,                      
                                       ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 1, 0, 
                                              ifelse(DAT_long_i$time %in% 1 & DAT_long_i$careCat %in% 2, 0,   
                                                     ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 0, 0,                        
                                                            ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 1, 1, 
                                                                   ifelse(DAT_long_i$time %in% 2 & DAT_long_i$careCat %in% 2, 2, NA)))))))                             
  MODEL_i <- plm(lifeSatis  ~  time + employ + worriesEcon + as.factor(careCat) + as.factor(timeCare), 
                 model = "within", index=c("pid","time"),
                 data = DAT_long_i)
  #summary(MODEL_i, vcov=vcovHC(MODEL_i, method="white1"))   
  modList[[i]] <- MODEL_i 
  cat("\n ------------- \n")  
}

n <- nrow(CT)
res <- poolIt(modList, n)
round(res,2)
rs <- rep(NA, imp$m); for(i in 1:imp$m) {rs[i] <- summary(modList[[i]])$r.squared[1]}; mean(rs)
