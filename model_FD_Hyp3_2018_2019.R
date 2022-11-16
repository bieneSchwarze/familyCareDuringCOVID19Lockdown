###############################################################################################
###############################################################################################
##
## Paper on the impact of informal care during spring 2019 in comparison to 2018 and 2018 on 
## - depression scores
## - general life satisfaction
## 
## Fixed Effects Regression: Hypothesis 3 
## The relationship between informal care provision and well-being will be moderated by care intensity 
## with the decline in well-being being more severe for those caregivers who increased their care hours 
##
## for balanced panel 2018 - 2019
## sample: all respondent, explan. category: noncaregiver vs. caregiver
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
library(fastDummies)
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

# load: _dataPreparedImputed_2018_2019_05102022.RData

# --------------------------------------------------------------
# DV Depression Score for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)
predList <- vector(length=imp$m, mode="list") 
ns <- rep(NA, imp$m)

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
  
  DAT_fe_i$groupInt <- NA
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% 0] <- 0 # non-caregivers
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 1 # continuing & low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 >2] <- 2 # continuing & high-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 3 # new caregivers & low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 >2] <- 4 # new caregivers & high-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 5 # continuing & switching from high to low-int  
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 >2] <- 6 # continuing & switching from low to high-int  
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 %in% 0] <- 7 # stopping to care from low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 %in% 0] <- 8 # stopping to care from high-int  
  table(DAT_fe_i$groupInt, exclude=NULL)
  DAT_fe_i <- dummy_cols(DAT_fe_i, select_columns = "groupInt")
  
  #DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$groupInt %in% 0:6,] # let caregivers giving up (cat. 4) aside
  ns[i] <- nrow(DAT_fe_i)
  
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)       

  mod <- plm(depress  ~  time*groupInt_1 + time*groupInt_2 + 
                          time*groupInt_3 + time*groupInt_4 +
                          time*groupInt_5 + time*groupInt_6 + 
                          unEmpl_dum*groupInt_1 + unEmpl_dum*groupInt_2 +
                          unEmpl_dum*groupInt_3 + unEmpl_dum*groupInt_4 +                
                          unEmpl_dum*groupInt_5 + unEmpl_dum*groupInt_6 +
                          unEmpl_dum*groupInt_7 + unEmpl_dum*groupInt_8 +                
                          noEmpl_dum*groupInt_1 + noEmpl_dum*groupInt_2 +
                          noEmpl_dum*groupInt_3 + noEmpl_dum*groupInt_4 +                
                          noEmpl_dum*groupInt_5 + noEmpl_dum*groupInt_6 +
                          lowWorr_dum*groupInt_1 + lowWorr_dum*groupInt_2 +
                          lowWorr_dum*groupInt_3 + lowWorr_dum*groupInt_4 +                
                          lowWorr_dum*groupInt_5 + lowWorr_dum*groupInt_6 +
                          lowWorr_dum*groupInt_7 + lowWorr_dum*groupInt_8 +                  
                          highWorr_dum*groupInt_1 + highWorr_dum*groupInt_2 +
                          highWorr_dum*groupInt_3 + highWorr_dum*groupInt_4 +                
                          highWorr_dum*groupInt_5 + highWorr_dum*groupInt_6,
              data=DAT_long_i, model="within", index=c("pid","time"))
   
  summary(mod)
  modList[[i]] <- mod
  rsq[i] <- summary(mod)$r.squared["rsq"]
}
n <- nrow(CT)
res_Depr_2019 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_Depr_2019 <- cbind.data.frame(res_Depr_2019[,1], round(res_Depr_2019[,"estimate"],2), 
                               round(res_Depr_2019[,"5 %"],2), round(res_Depr_2019[,"95 %"],2))
colnames(res_Depr_2019) <- c("Var", "Est", "CIlow", "CIup")
res_Depr_2019
mean(rsq)
mean(ns)

# --------------------------------------------------------------
# DV Life Satisfaction for 2018, 2019 (using imputed data)
# --------------------------------------------------------------

# Do analysis separately for each imputed data set
modList <- vector(length=imp$m, mode="list") 
rsq <- rep(NA,imp$m)
predList <- vector(length=imp$m, mode="list") 
ns <- rep(NA, imp$m)

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
  
  DAT_fe_i$groupInt <- NA
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% 0] <- 0 # non-caregivers
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 1 # continuing & low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 >2] <- 2 # continuing & high-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 3 # new caregivers & low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% 0 & DAT_fe_i$careTime.2019 >2] <- 4 # new caregivers & high-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 %in% c(1,2)] <- 5 # continuing & switching from high to low-int  
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 >2] <- 6 # continuing & switching from low to high-int  
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 %in% c(1,2) & DAT_fe_i$careTime.2019 %in% 0] <- 7 # stopping to care from low-int
  DAT_fe_i$groupInt[DAT_fe_i$careTime.2018 >2 & DAT_fe_i$careTime.2019 %in% 0] <- 8 # stopping to care from high-int  
  table(DAT_fe_i$groupInt, exclude=NULL)
  DAT_fe_i <- dummy_cols(DAT_fe_i, select_columns = "groupInt")

  #DAT_fe_i <-  DAT_fe_i[DAT_fe_i$group %in% c(1,2,3),] # let caregivers giving up (cat. 4) aside  
  DAT_fe_i <-  DAT_fe_i[DAT_fe_i$groupInt %in% 0:6,] # let caregivers giving up (cat. 4) aside
  ns[i] <- nrow(DAT_fe_i)
  
  DAT_long_i <- reshape(DAT_fe_i, idvar="pid", direction ="long", varying=list(c(5,6),c(7,8),c(9,10), c(11,12), c(13,14)),
                        v.names=c("careTime", "employ", "worriesEcon", "depress", "lifeSatis"))  
  
  DAT_long_i <- DAT_long_i[order(DAT_long_i$pid),]
  DAT_long_i$time <- ifelse(DAT_long_i$time %in% 1,0,1)
  DAT_long_i$empl_dum <- ifelse(DAT_long_i$employ %in% "employed", 1, 0)    
  DAT_long_i$noEmpl_dum <- ifelse(DAT_long_i$employ %in% "nonemployed", 1, 0)      
  DAT_long_i$unEmpl_dum <- ifelse(DAT_long_i$employ %in% "unemployed", 1, 0)      
  DAT_long_i$noWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 3, 1, 0)    # 1: grave worries, 2: some worries, 3: no worries
  DAT_long_i$lowWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 2, 1, 0)      
  DAT_long_i$highWorr_dum <- ifelse(DAT_long_i$worriesEcon %in% 1, 1, 0)       

  mod <- plm(lifeSatis  ~  time*groupInt_1 + time*groupInt_2 + 
               time*groupInt_3 + time*groupInt_4 +
               time*groupInt_5 + time*groupInt_6 + 
               time*groupInt_7 + time*groupInt_8 +
               unEmpl_dum*groupInt_1 + unEmpl_dum*groupInt_2 +
               unEmpl_dum*groupInt_3 + unEmpl_dum*groupInt_4 +                
               unEmpl_dum*groupInt_5 + unEmpl_dum*groupInt_6 +
               unEmpl_dum*groupInt_7 + unEmpl_dum*groupInt_8 +                
               noEmpl_dum*groupInt_1 + noEmpl_dum*groupInt_2 +
               noEmpl_dum*groupInt_3 + noEmpl_dum*groupInt_4 +                
               noEmpl_dum*groupInt_5 + noEmpl_dum*groupInt_6 +
               lowWorr_dum*groupInt_1 + lowWorr_dum*groupInt_2 +
               lowWorr_dum*groupInt_3 + lowWorr_dum*groupInt_4 +                
               lowWorr_dum*groupInt_5 + lowWorr_dum*groupInt_6 +
               #lowWorr_dum*groupInt_7 + lowWorr_dum*groupInt_8 +                  
               highWorr_dum*groupInt_1 + highWorr_dum*groupInt_2 +
               highWorr_dum*groupInt_3 + highWorr_dum*groupInt_4 +                
               highWorr_dum*groupInt_5 + highWorr_dum*groupInt_6,
             data=DAT_long_i, model="within", index=c("pid","time"))
  
  #summary(mod)
  modList[[i]] <- mod
  rsq[i] <- summary(mod)$r.squared["rsq"]
}
n <- nrow(CT)
res_LS_2019 <- summary(pool(as.mira(modList)), conf.int = TRUE, conf.level=0.9)
res_LS_2019 <- cbind.data.frame(res_LS_2019[,1], round(res_LS_2019[,"estimate"],2), 
                                  round(res_LS_2019[,"5 %"],2), round(res_LS_2019[,"95 %"],2))
colnames(res_LS_2019) <- c("Var", "Est", "CIlow", "CIup")
res_LS_2019
mean(rsq)
mean(ns)

# ------------
# Plot it
# ------------
# Beta Coefficients
beta_depress <- cbind(res_Depr_2019[6:11,], "Depression")
colnames(beta_depress) <- c("Group", "Estimate", "CI_low", "CI_up", "Measure")
beta_lifeSatis <- cbind(res_LS_2019[6:11,], "Life Satisfaction")
colnames(beta_lifeSatis) <- c("Group", "Estimate", "CI_low", "CI_up", "Measure")

betas <- rbind.data.frame(beta_depress,beta_lifeSatis)
nam <- c("Cont. low intensity", "Cont. high intensity", "New low intensity", 
         "New high intensity", "Cont. high to low intensity", "Cont. low to high intensity")
betas$IntGroup <- as.factor(rep(nam,2))
betas$Estimate <- as.numeric(betas$Estimate)
betas$CI_low <- as.numeric(betas$CI_low)
betas$CI_up <- as.numeric(betas$CI_up)
betas$IntGroup <- factor(betas$IntGroup, levels=rev(nam))

zp <- ggplot(betas) 
zp <- zp + geom_pointrange(aes(x = IntGroup, y = Estimate, ymin = CI_low, ymax = CI_up),
                           lwd = 0.7, position = position_dodge(width = 1/2)) 
zp <- zp + geom_hline(yintercept=0, color = "grey10")
zp <- zp + xlab("") + ylab("beta coefficients (year=2019 * care group)")
zp <- zp + theme(axis.text.y=element_text(size=14))
zp <- zp + facet_grid(cols=vars(Measure)) + scale_color_manual(values=c("darkred", "darkblue"))
zp <- zp + theme(strip.text.x = element_text(size = 12))
zp <- zp + coord_flip()
zp






