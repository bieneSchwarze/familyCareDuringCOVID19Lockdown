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
## Data : balanced panel 2019 - 2020
##
## SZ, 05.10.2022
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

# load: _dataPreparedImputed_2019_2020_05102022.RData

DAT <- CTI[,c("pid", "sex", "gebjahr", "phrf_cati_SOEPCoV", 
                   "careTime_2019", "careTime_2020",
                   "employ_2019", "employ_2020",
                   "worriesEcon_2019", "worriesEcon_2020",
                   "depress_2019", "depress_2020",
                   "lifeSatis_2019",  "lifeSatis_2020")] 
colnames(DAT)[5:6] <- c("careTime.2019", "careTime.2020")
colnames(DAT)[7:8] <- c("employ.2019", "employ.2020")
colnames(DAT)[9:10] <- c("worriesEcon.2019", "worriesEcon.2020") 
colnames(DAT)[11:12] <- c("depress.2019", "depress.2020")
colnames(DAT)[13:14] <- c("lifeSatis.2019", "lifeSatis.2020")

DAT$group <- ifelse(DAT$careTime.2019 %in% 0 & DAT$careTime.2020 %in% 0, 1, 
                         ifelse(DAT$careTime.2019 > 0 & DAT$careTime.2020 > 0, 2,
                                ifelse(DAT$careTime.2019 %in% 0 & DAT$careTime.2020 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
table(DAT$group, exclude=NULL)
DAT$careTimeDum.2019 <- ifelse(is.na(DAT$careTime.2019), NA, ifelse(DAT$careTime.2019 %in% 0,0,1))
DAT$careTimeDum.2020 <- ifelse(is.na(DAT$careTime.2020), NA, ifelse(DAT$careTime.2020 %in% 0,0,1))
table(DAT$careTimeDum.2019, DAT$careTimeDum.2020, exclude=NULL)

DAT_newcare <- DAT[(!is.na(DAT$careTimeDum.2019) & DAT$careTimeDum.2019 %in% 0 & !is.na(DAT$careTimeDum.2020) & DAT$careTimeDum.2020 >0),]
DAT_cont <-DAT[(!is.na(DAT$careTimeDum.2019) & DAT$careTimeDum.2019>0 & !is.na(DAT$careTimeDum.2020) & DAT$careTimeDum.2020 >0),]
DAT_cont$lowCare_2019_dum <- ifelse(DAT_cont$careTime.2019 >=1 & DAT_cont$careTime.2019 <=2, 1, 0)    
DAT_cont$highCare_2019_dum <- ifelse(DAT_cont$careTime.2019 >2, 1, 0)   
DAT_cont$lowCare_2020_dum <- ifelse(DAT_cont$careTime.2020 >=1 & DAT_cont$careTime.2020 <=2, 1, 0)    
DAT_cont$highCare_2020_dum <- ifelse(DAT_cont$careTime.2020 >2, 1, 0)   

DAT_cont_low2019 <- DAT_cont[DAT_cont$lowCare_2019_dum %in% 1,] 
table(DAT_cont_low2019$highCare_2020_dum, exclude=NULL)

DAT_cont_high2019 <- DAT_cont[DAT_cont$highCare_2019_dum %in% 1,] 
table(DAT_cont_high2019$highCare_2020_dum, exclude=NULL)

DAT$group <- ifelse(DAT$careTime.2019 %in% 0 & DAT$careTime.2020 %in% 0, 1, 
                    ifelse(DAT$careTime.2019 > 0 & DAT$careTime.2020 > 0, 2,
                           ifelse(DAT$careTime.2019 %in% 0 & DAT$careTime.2020 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
table(DAT$group)

# --------------------------------------------------------------
# DV Depression Score for 2019, 2020 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)

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
  
  DAT_fe_i$group <- ifelse(DAT_fe_i$careTime.2019 %in% 0 & DAT_fe_i$careTime.2020 %in% 0, 1, 
                           ifelse(DAT_fe_i$careTime.2019 > 0 & DAT_fe_i$careTime.2020 > 0, 2,
                                  ifelse(DAT_fe_i$careTime.2019 %in% 0 & DAT_fe_i$careTime.2020 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
  table(DAT_fe_i$group)
  
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside
  
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
res_Depr_2020 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_Depr_2020 <- cbind.data.frame(res_Depr_2020[,1], round(res_Depr_2020[,"estimate"],2), 
                               round(res_Depr_2020[,"5 %"],2), round(res_Depr_2020[,"95 %"],2))
colnames(res_Depr_2020) <- c("Var", "Est", "CIlow", "CIup")
res_Depr_2020
mean(rsq)

# --------------------------------------------------------------
# D.2 DV Life Satisfaction for 2019, 2020 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)

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
  
  DAT_fe_i$group <- ifelse(DAT_fe_i$careTime.2019 %in% 0 & DAT_fe_i$careTime.2020 %in% 0, 1, 
                           ifelse(DAT_fe_i$careTime.2019 > 0 & DAT_fe_i$careTime.2020 > 0, 2,
                                  ifelse(DAT_fe_i$careTime.2019 %in% 0 & DAT_fe_i$careTime.2020 > 0,3,4))) # 1: noncarers, 2: cont. carers, 3: new carers 
  #table(DAT_fe_i$group)
  
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside
  
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
  
  #mod <- plm(lifeSatis  ~  time*noCarer_dum + time*newCarer_dum + 
  #             + unEmpl_dum*noCarer_dum + noEmpl_dum*noCarer_dum + 
  #             + unEmpl_dum*newCarer_dum + noEmpl_dum*newCarer_dum + 
  #             + lowWorr_dum*noCarer_dum + highWorr_dum*noCarer_dum +
  #             + lowWorr_dum*newCarer_dum + highWorr_dum*newCarer_dum, 
  #           data=DAT_long_i, model="within", index=c("pid","time"))
  summary(mod)
  modList[[i]] <- mod
  rsq[i] <- summary(mod)$r.squared["rsq"]
  
  cat("\n ------------- \n")
}
n <- nrow(CT)
res_LS_2020 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_LS_2020 <- cbind.data.frame(res_LS_2020[,1], round(res_LS_2020[,"estimate"],2), 
                                  round(res_LS_2020[,"5 %"],2), round(res_LS_2020[,"95 %"],2))
colnames(res_LS_2020) <- c("Var", "Est", "CIlow", "CIup")
res_LS_2020
mean(rsq)


