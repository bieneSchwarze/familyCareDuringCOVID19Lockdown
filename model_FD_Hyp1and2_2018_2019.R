###############################################################################################
###############################################################################################
##
## Paper on the impact of informal care during spring 2020 in comparison to 2019 and 2018 on 
## - depression scores
## - general life satisfaction
## 
## Hypothesis 1 
## (a) "Those providing family care will experience a larger decline in well-being (more depressive symptoms, less life satisfaction) 
##  during the early phase of the COVID-19 pandemic compared to 2019 than those not providing family care"
## (b) "Those providing family care will experience no change, or an increase in well-being (depressive symptoms, life satisfaction) 
##  during the early phase of the COVID-19 pandemic compared to 2019"
##
## Hypothesis 2 
## (a) "the relationship between family care provision and well-being is moderated by the family care 
## pattern: the decline in well-being (i.e., more depressive symptoms, less life satisfaction) will be 
## more severe for those newly taking up family care responsibilities"
## (b) "while continuing family caregivers will experience no change or even an increase in well-being"
##
## Model: OLS Fixed Effects Regression
##
## Data : balanced panel 2018 - 2019
##
## SZ, 14.11.2022
##
###############################################################################################
###############################################################################################

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

# load: _dataPreparedImputed_2018_2019_30032021

DAT <- CTI[,c("pid", "sex", "gebjahr", "bjphrf", 
                   "careTime_2018", "careTime_2019",
                   "employ_2018", "employ_2019",
                   "worriesEcon_2018", "worriesEcon_2019",
                   "depress_2018", "depress_2019",
                   "lifeSatis_2018",  "lifeSatis_2019")] 
colnames(DAT)[5:6] <- c("careTime.2018", "careTime.2019")
colnames(DAT)[7:8] <- c("employ.2018", "employ.2019")
colnames(DAT)[9:10] <- c("worriesEcon.2018", "worriesEcon.2019") 
colnames(DAT)[11:12] <- c("depress.2018", "depress.2019")
colnames(DAT)[13:14] <- c("lifeSatis.2018", "lifeSatis.2019")

DAT$group <- ifelse(DAT$careTime.2018 %in% 0 & DAT$careTime.2019 %in% 0, 1, 
                         ifelse(DAT$careTime.2018 > 0 & DAT$careTime.2019 > 0, 2,
                                ifelse(DAT$careTime.2018 %in% 0 & DAT$careTime.2019 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
table(DAT$group, exclude=NULL)
DAT$careTimeDum.2018 <- ifelse(is.na(DAT$careTime.2018), NA, ifelse(DAT$careTime.2018 %in% 0,0,1))
DAT$careTimeDum.2019 <- ifelse(is.na(DAT$careTime.2019), NA, ifelse(DAT$careTime.2019 %in% 0,0,1))
table(DAT$careTimeDum.2018, DAT$careTimeDum.2019, exclude=NULL)

DAT_newcare <- DAT[(!is.na(DAT$careTimeDum.2018) & DAT$careTimeDum.2018 %in% 0 & !is.na(DAT$careTimeDum.2019) & DAT$careTimeDum.2019 >0),]
DAT_cont <-DAT[(!is.na(DAT$careTimeDum.2018) & DAT$careTimeDum.2018>0 & !is.na(DAT$careTimeDum.2019) & DAT$careTimeDum.2019 >0),]
DAT_cont$lowCare_2018_dum <- ifelse(DAT_cont$careTime.2018 >=1 & DAT_cont$careTime.2018 <=2, 1, 0)    
DAT_cont$highCare_2018_dum <- ifelse(DAT_cont$careTime.2018 >2, 1, 0)   
DAT_cont$lowCare_2019_dum <- ifelse(DAT_cont$careTime.2019 >=1 & DAT_cont$careTime.2019 <=2, 1, 0)    
DAT_cont$highCare_2019_dum <- ifelse(DAT_cont$careTime.2019 >2, 1, 0)   

DAT_cont_low2018 <- DAT_cont[DAT_cont$lowCare_2018_dum %in% 1,] 
table(DAT_cont_low2018$highCare_2019_dum, exclude=NULL)

DAT_cont_high2018 <- DAT_cont[DAT_cont$highCare_2018_dum %in% 1,] 
table(DAT_cont_high2018$highCare_2019_dum, exclude=NULL)

#################################################################
# D. Fixed Effects Regression
# Separate Models for continuous carers, new carers, noncarers
#################################################################
# --------------------------------------------------------------
# DV Depression Score for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList_nonCarers <- vector(length=imp$m, mode="list") 
modList_contCarers <- vector(length=imp$m, mode="list") 
modList_newCarers <- vector(length=imp$m, mode="list") 
rsq_noCarers <- rep(NA,imp$m)
rsq_contCarers <- rep(NA,imp$m)
rsq_newCarers <- rep(NA,imp$m)
n_noCarers <- rep(NA,imp$m)
n_contCarers <- rep(NA,imp$m)
n_newCarers <- rep(NA,imp$m)

for(i in 1:imp$m){
  
  #i <- 1
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
  #summary(mod_noCar)
  modList_nonCarers[[i]] <- mod_noCar
  rsq_noCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  n_noCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 1,])
  
  DAT_long_i_contCarer <- DAT_long_i[DAT_long_i$group %in% 2,]
  mod_contCar <- plm(depress  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                       + unEmpl_dum + noEmpl_dum + 
                       + lowWorr_dum + highWorr_dum, 
                     data=DAT_long_i_contCarer, model="within", index=c("pid","time"))
  #summary(mod_contCar)
  modList_contCarers[[i]] <- mod_contCar
  rsq_contCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  n_contCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 2,])
  
  DAT_long_i_newCarer <- DAT_long_i[DAT_long_i$group %in% 3,]
  mod_newCar <- plm(depress  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                      + unEmpl_dum + noEmpl_dum + 
                      + lowWorr_dum + highWorr_dum, 
                    data=DAT_long_i_newCarer, model="within", index=c("pid","time"))
  #summary(mod_newCar)
  modList_newCarers[[i]] <- mod_newCar
  rsq_newCarers[i] <- summary(mod_newCar)$r.squared["rsq"]
  n_newCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 3,])
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
# noncarers
res_nonCar_Depr <- summary(pool(as.mira(modList_nonCarers)), conf.int = TRUE)
res_nonCar <- cbind.data.frame(res_nonCar_Depr[,1], round(res_nonCar_Depr[,"estimate"],2), 
                               round(res_nonCar_Depr[,"2.5 %"],2), round(res_nonCar_Depr[,"97.5 %"],2))
colnames(res_nonCar) <- c("Var", "Est", "CIlow", "CIup")
res_nonCar
mean(rsq_noCarers)
mean(n_noCarers)
# continuous carers
res_contCar_Depr <- summary(pool(as.mira(modList_contCarers)), conf.int = TRUE)
res_contCar <- cbind.data.frame(res_contCar_Depr[,1], round(res_contCar_Depr[,"estimate"],2), 
                                round(res_contCar_Depr[,"2.5 %"],2), round(res_contCar_Depr[,"97.5 %"],2))
colnames(res_contCar) <- c("Var", "Est", "CIlow", "CIup")
res_contCar
mean(rsq_contCarers)
mean(n_contCarers)
# new carers
res_newCar_Depr <- summary(pool(as.mira(modList_newCarers)), conf.int = TRUE)
res_newCar <- cbind.data.frame(res_newCar_Depr[,1], round(res_newCar_Depr[,"estimate"],2), 
                               round(res_newCar_Depr[,"2.5 %"],2), round(res_newCar_Depr[,"97.5 %"],2))
colnames(res_newCar) <- c("Var", "Est", "CIlow", "CIup")
res_newCar
mean(rsq_newCarers)
mean(n_newCarers)

#DAT$group <- ifelse(is.na(DAT$careTime.2018) | is.na(DAT$careTime.2019), NA, 
#                    ifelse(DAT$careTime.2018 %in% 0 & DAT$careTime.2019 %in% 0, 1, 
#                           ifelse(DAT$careTime.2018 > 0 & DAT$careTime.2019 > 0, 2,
#                                  ifelse(DAT$careTime.2018 %in% 0 & DAT$careTime.2019 > 0,3,4)))) # 1: noncarers, 2: cont. carers, 3: new carers 
#DATs <- merge(DAT, D_i[, c("pid", "psample")], by="pid", all.x=TRUE)
#DATs <- DATs[!(DATs$psample %in% c(20,21)),]
#table(DATs$group, exclude=NULL)

# --------------------------------------------------------------
# DV Life Satisfaction for 2018, 2019 (using imputed data)
# --------------------------------------------------------------
# Do analysis separately for each imputed data set
modList_nonCarers <- vector(length=imp$m, mode="list") 
modList_contCarers <- vector(length=imp$m, mode="list") 
modList_newCarers <- vector(length=imp$m, mode="list") 
rsq_noCarers <- rep(NA,imp$m)
rsq_contCarers <- rep(NA,imp$m)
rsq_newCarers <- rep(NA,imp$m)
n_noCarers <- rep(NA,imp$m)
n_contCarers <- rep(NA,imp$m)
n_newCarers <- rep(NA,imp$m)

for(i in 1:imp$m){
  
  #i <- 1
  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
  #D_i <- D_i[!(D_i$psample %in% c(20,21)),] # take out samples N and O (no measurement in 2016 of depression score, at this time samples N and O were not yet part of SOEP)
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
  #table(DAT_fe_i$group)
  
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
  #summary(mod_noCar)
  modList_nonCarers[[i]] <- mod_noCar
  rsq_noCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  n_noCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 1,])
  
  DAT_long_i_contCarer <- DAT_long_i[DAT_long_i$group %in% 2,]
  mod_contCar <- plm(lifeSatis  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                       + unEmpl_dum + noEmpl_dum + 
                       + lowWorr_dum + highWorr_dum, 
                     data=DAT_long_i_contCarer, model="within", index=c("pid","time"))
  #summary(mod_contCar)
  modList_contCarers[[i]] <- mod_contCar
  rsq_contCarers[i] <- summary(mod_noCar)$r.squared["rsq"]
  n_contCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 2,])
  
  DAT_long_i_newCarer <- DAT_long_i[DAT_long_i$group %in% 3,]
  mod_newCar <- plm(lifeSatis  ~  time + as.factor(birthyearCat) + as.factor(sex) + # 1: male, 2: female, 
                      + unEmpl_dum + noEmpl_dum + 
                      + lowWorr_dum + highWorr_dum, 
                    data=DAT_long_i_newCarer, model="within", index=c("pid","time"))
  #summary(mod_newCar)
  modList_newCarers[[i]] <- mod_newCar
  rsq_newCarers[i] <- summary(mod_newCar)$r.squared["rsq"]
  n_newCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 3,])
  
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
mean(n_noCarers)
# continuing caregivers
res_contCar_LifeSatis <- summary(pool(as.mira(modList_contCarers)), conf.int = TRUE)
res_contCar <- cbind.data.frame(res_contCar_LifeSatis[,1], round(res_contCar_LifeSatis[,"estimate"],2), 
                                round(res_contCar_LifeSatis[,"2.5 %"],2), round(res_contCar_LifeSatis[,"97.5 %"],2))
colnames(res_contCar) <- c("Var", "Est", "CIlow", "CIup")
res_contCar
mean(rsq_contCarers)
mean(n_contCarers)
# new caregivers
res_newCar_LifeSatis <- summary(pool(as.mira(modList_newCarers)), conf.int = TRUE)
res_newCar <- cbind.data.frame(res_newCar_LifeSatis[,1], round(res_newCar_LifeSatis[,"estimate"],2), 
                               round(res_newCar_LifeSatis[,"2.5 %"],2), round(res_newCar_LifeSatis[,"97.5 %"],2))
colnames(res_newCar) <- c("Var", "Est", "CIlow", "CIup")
res_newCar
mean(rsq_newCarers)
mean(n_newCarers)

#################################################################
# D. Fixed Effects Regression
# Fully Interacted Models for continuous carers, new carers, noncarers
#################################################################
# --------------------------------------------------------------
# DV Depression Score for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)
n_imp <- rep(NA,imp$m)
n_noCarers <- rep(NA,imp$m)
n_contCarers <- rep(NA,imp$m)
n_newCarers <- rep(NA,imp$m)


for(i in 1:imp$m){
  
  #i <- 1
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
  
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside
  n_imp[i] <- nrow(DAT_fe_i)
  n_noCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 1,])
  n_contCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 2,])
  n_newCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 3,])
  
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  
  DAT_long_i$contCarer_dum <- ifelse(DAT_long_i$group %in% 2,1,0)
  DAT_long_i$newCarer_dum <- ifelse(DAT_long_i$group %in% 3,1,0)  
  DAT_long_i$noCarer_dum <- ifelse(DAT_long_i$group %in% 1,1,0)
  
  DAT_long_i$noCare_dum <- ifelse(DAT_long_i$careTime <1, 1, 0)  
  DAT_long_i$Care_dum <- ifelse(DAT_long_i$careTime >=1, 1, 0)  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)       

   mod <- plm(depress  ~  time*contCarer_dum + time*newCarer_dum +
                      + unEmpl_dum*newCarer_dum + noEmpl_dum*newCarer_dum +
                      + unEmpl_dum*contCarer_dum + noEmpl_dum*contCarer_dum +
                      + lowWorr_dum*newCarer_dum + highWorr_dum*newCarer_dum +
                      + lowWorr_dum*contCarer_dum + highWorr_dum*contCarer_dum,
                    data=DAT_long_i, model="within", index=c("pid","time"))
   
   # mod <- plm(depress  ~  time*noCarer_dum + time*newCarer_dum +
   #              + unEmpl_dum*noCarer_dum + noEmpl_dum*noCarer_dum +
   #              + unEmpl_dum*newCarer_dum + noEmpl_dum*newCarer_dum +
   #              + lowWorr_dum*noCarer_dum + highWorr_dum*noCarer_dum +
   #              + lowWorr_dum*newCarer_dum + highWorr_dum*newCarer_dum,
   #            data=DAT_long_i, model="within", index=c("pid","time"))
   
  summary(mod)
  modList[[i]] <- mod
  rsq[i] <- summary(mod)$r.squared["rsq"]
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
res_Depr_2019 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_Depr_2019 <- cbind.data.frame(res_Depr_2019[,1], round(res_Depr_2019[,"estimate"],2), 
                               round(res_Depr_2019[,"5 %"],2), round(res_Depr_2019[,"95 %"],2))
colnames(res_Depr_2019) <- c("Var", "Est", "CIlow", "CIup")
res_Depr_2019
mean(rsq)
mean(n_imp)
mean(n_noCarers)
mean(n_contCarers)
mean(n_newCarers)

# --------------------------------------------------------------
# DV Life Satisfaction for 2018, 2019 (using imputed data)
# --------------------------------------------------------------
# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)
n_imp <- rep(NA,imp$m)
n_noCarers <- rep(NA,imp$m)
n_contCarers <- rep(NA,imp$m)
n_newCarers <- rep(NA,imp$m)

for(i in 1:imp$m){
  
  #i <- 1
  cat("Imp.It: ",i,"\n")
  D_i <- complete(imp, action=i)
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
  #table(DAT_fe_i$group)
  
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside
  n_imp[i] <- nrow(DAT_fe_i)
  n_noCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 1,])
  n_contCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 2,])
  n_newCarers[i] <- nrow(DAT_fe_i[DAT_fe_i$group %in% 3,])
   
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  
  DAT_long_i$contCarer_dum <- ifelse(DAT_long_i$group %in% 2,1,0)
  DAT_long_i$newCarer_dum <- ifelse(DAT_long_i$group %in% 3,1,0)  
  DAT_long_i$noCarer_dum <- ifelse(DAT_long_i$group %in% 1,1,0)
  
  DAT_long_i$noCare_dum <- ifelse(DAT_long_i$careTime <1, 1, 0)  
  DAT_long_i$Care_dum <- ifelse(DAT_long_i$careTime >=1, 1, 0)  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)       
  
  mod <- plm(lifeSatis  ~  time*contCarer_dum + time*newCarer_dum +
               + unEmpl_dum*newCarer_dum + noEmpl_dum*newCarer_dum +
               + unEmpl_dum*contCarer_dum + noEmpl_dum*contCarer_dum +
               + lowWorr_dum*newCarer_dum + highWorr_dum*newCarer_dum +
               + lowWorr_dum*contCarer_dum + highWorr_dum*contCarer_dum,
             data=DAT_long_i, model="within", index=c("pid","time"))
  
  # mod <- plm(lifeSatis  ~  time*noCarer_dum + time*newCarer_dum + 
  #              + unEmpl_dum*noCarer_dum + noEmpl_dum*noCarer_dum + 
  #              + unEmpl_dum*newCarer_dum + noEmpl_dum*newCarer_dum + 
  #              + lowWorr_dum*noCarer_dum + highWorr_dum*noCarer_dum +
  #              + lowWorr_dum*newCarer_dum + highWorr_dum*newCarer_dum, 
  #            data=DAT_long_i, model="within", index=c("pid","time"))
  summary(mod)
  modList[[i]] <- mod
  rsq[i] <- summary(mod)$r.squared["rsq"]
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
res_LS_2019 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_LS_2019 <- cbind.data.frame(res_LS_2019[,1], round(res_LS_2019[,"estimate"],2), 
                                  round(res_LS_2019[,"5 %"],2), round(res_LS_2019[,"95 %"],2))
colnames(res_LS_2019) <- c("Var", "Est", "CIlow", "CIup")
res_LS_2019
mean(rsq)
mean(n_imp)
mean(n_noCarers)
mean(n_contCarers)
mean(n_newCarers)

