# Data sets ####
library(plyr)
library(dplyr)

# Test-takers data set
registered <- readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PDT_2021_PUB_MRUN.csv")
registered <- select(registered,
       MRUN,COD_SEXO,RBD,CODIGO_ENS,RAMA_EDUCACIONAL,DEPENDENCIA,
       PTJE_RANKING,CLEC_ACTUAL,MATE_ACTUAL,PUNTAJES_PROCESO)
registered <- data.frame(filter(registered, PUNTAJES_PROCESO == 1 & CODIGO_ENS %in% c(310,410,510,610,710,810,910)))
registered$PUNTAJES_PROCESO <- NULL
registered$CODIGO_ENS <- NULL
registered <- registered %>% filter(RBD != 99999)

# the sample are students who participated for the first time in the 2020 admission process
# (to enter higher education in 2021) and attended schools with regular curricula (general curriculum
# for secondary school students).

# SES (complementary information) data set
SES <-readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/B_Socioeconomico/B_SOCIOECONOMICO_DOMICILIO_PDT_2021_PUB_MRUN.csv")
SES <- select(SES,
       MRUN,INGRESO_PERCAPITA_GRUPO_FA,EDUCACION_MADRE, EDUCACION_PADRE)

# Joining the data
data <- left_join(registered, SES, by = 'MRUN') #all the rows in the left table will be preserved
rm(registered, SES)

# Recoding data to create the model predictors
data_recode <- data %>% mutate(
  SexFem = COD_SEXO - 1,
  TypeHumanist = recode(RAMA_EDUCACIONAL,
             'H1' = 1,
             'T1' = 0,
             'T2' = 0,
             'T3' = 0,
             'T4' = 0,
             'T5' = 0),
  SectorPriv = recode(DEPENDENCIA,
                  '1' = 0,
                  '2' = 0,
                  '3' = 1,
                  '4' = 1,
                  '5' = 1,
                  '6' = 0),
  EdMomBas = ifelse(EDUCACION_MADRE >= 1 & EDUCACION_MADRE <= 6, 1,
                    ifelse(is.na(EDUCACION_MADRE) == T | EDUCACION_MADRE == 13, NA,0)),
  EdMomHE = ifelse(EDUCACION_MADRE >= 10 & EDUCACION_MADRE <= 12, 1,
                    ifelse(is.na(EDUCACION_MADRE) == T | EDUCACION_MADRE == 13, NA,0)),
  EdDadBas = ifelse(EDUCACION_PADRE >= 1 & EDUCACION_PADRE <= 6, 1,
                    ifelse(is.na(EDUCACION_PADRE) == T | EDUCACION_PADRE == 13, NA,0)),
  EdDadHE = ifelse(EDUCACION_PADRE >= 10 & EDUCACION_PADRE <= 12, 1,
                   ifelse(is.na(EDUCACION_PADRE) == T | EDUCACION_PADRE == 13, NA,0)),
  #category 13 means IDK
  FamIncome_Q2 = ifelse(INGRESO_PERCAPITA_GRUPO_FA > 2, 1, 0),
  FamIncome_Q3 = ifelse(INGRESO_PERCAPITA_GRUPO_FA > 4, 1, 0),
  FamIncome_Q4 = ifelse(INGRESO_PERCAPITA_GRUPO_FA > 6, 1, 0),
  FamIncome_Q5 = ifelse(INGRESO_PERCAPITA_GRUPO_FA > 8, 1, 0),
  RKG_score_550 = PTJE_RANKING - 550,
  RKG_score = PTJE_RANKING,
  MAT_score = MATE_ACTUAL,
  LANG_score = CLEC_ACTUAL)

data_mod <- select(data_recode, RBD, SexFem:LANG_score) %>% arrange(RBD)
rm(data, data_recode)

# Data Transformation #####

# How many students do we need for each school? 135 schools have 1 student    
#data_mod %>% group_by(RBD) %>% summarise(n = n()) %>% arrange(n) #%>% View()

# Level-2, Group Mean (GM) predictors
rbd_means <- data_mod %>% #school means
  group_by(RBD) %>%
  summarise(
  SexFem_RBD = mean(SexFem, na.rm = T),
  EdMomBas_RBD = mean(EdMomBas, na.rm = T),
  EdMomHE_RBD = mean(EdMomHE, na.rm = T),
  EdDadBas_RBD = mean(EdDadBas, na.rm = T),
  EdDadHE_RBD = mean(EdDadHE, na.rm = T),
  FamIncome_Q2_RBD = mean(FamIncome_Q2, na.rm = T),
  FamIncome_Q3_RBD = mean(FamIncome_Q3, na.rm = T),
  FamIncome_Q4_RBD = mean(FamIncome_Q4, na.rm = T),
  FamIncome_Q5_RBD = mean(FamIncome_Q5, na.rm = T),
  RKG_score_RBD = mean(RKG_score, na.rm = T))

rbd_means <- rbd_means %>% # Difference between group mean and constant values
  mutate(
    SexFem_BG_50 = SexFem_RBD - .50,
    EdMomBas_BG_40 = EdMomBas_RBD - .40,
    EdMomHE_BG_40 = EdMomHE_RBD - .40,
    EdDadBas_BG_40 = EdDadBas_RBD - .40,
    EdDadHE_BG_40 = EdDadHE_RBD - .40,
    FamIncome_Q2_BG_30 = FamIncome_Q2_RBD - .30,
    FamIncome_Q3_BG_30 = FamIncome_Q3_RBD - .30,
    FamIncome_Q4_BG_30 = FamIncome_Q4_RBD - .30,
    FamIncome_Q5_BG_30 = FamIncome_Q5_RBD - .30,
    RKG_score_BG_mean = RKG_score_RBD - mean(RKG_score_RBD, na.rm = T))

#rbd_means <- rbd_means %>% # Difference between group mean and grand mean
  mutate(
    SexFem_BG = SexFem_RBD - mean(SexFem_RBD, na.rm = T),
    EdMomBas_BG = EdMomBas_RBD - mean(EdMomBas_RBD, na.rm = T),
    EdMomHE_BG = EdMomHE_RBD - mean(EdMomHE_RBD, na.rm = T),
    EdDadBas_BG = EdDadBas_RBD - mean(EdDadBas_RBD, na.rm = T),
    EdDadHE_BG = EdDadHE_RBD - mean(EdDadHE_RBD, na.rm = T),
    FamIncome_Q2_BG = FamIncome_Q2_RBD - mean(FamIncome_Q2_RBD, na.rm = T),
    FamIncome_Q3_BG = FamIncome_Q3_RBD - mean(FamIncome_Q3_RBD, na.rm = T),
    FamIncome_Q4_BG = FamIncome_Q4_RBD - mean(FamIncome_Q4_RBD, na.rm = T),
    FamIncome_Q5_BG = FamIncome_Q5_RBD - mean(FamIncome_Q5_RBD, na.rm = T),
    RKG_score_BG = RKG_score_RBD - mean(RKG_score_RBD, na.rm = T))

data <- left_join(data_mod, rbd_means, by = "RBD")
rm(data_mod, rbd_means)

#Level-1, Within Group (WG) predictors = deviation from group mean of students' math score
# Grand mean centering for level 1 variables
#data <- data %>%
  mutate(
  SexFem_WG_Grand = SexFem_BG - .5,
  EdMomBas_WG_Grand = EdMomBas_BG - .5,
  EdMomHE_WG_Grand = EdMomHE_BG - .5,
  EdDadBas_WG_Grand = EdDadBas_BG - .5,
  EdDadHE_WG_Grand = EdDadHE_BG - .5,
  FamIncome_Q2_WG_Grand = FamIncome_Q2_BG - .5,
  FamIncome_Q3_WG_Grand = FamIncome_Q3_BG - .5,
  FamIncome_Q4_WG_Grand = FamIncome_Q4_BG - .5,
  FamIncome_Q5_WG_Grand = FamIncome_Q5_BG - .5,
  RKG_score_WG_Grand = RKG_score_BG - .5)

## Group mean centering for level 1 variables
#data <- data %>%
  mutate(
  SexFem_WG_Group = SexFem_BG - SexFem_RBD,
  EdMomBas_WG_Group = EdMomBas_BG - EdMomBas_RBD,
  EdMomHE_WG_Group = EdMomHE_BG - EdMomHE_RBD,
  EdDadBas_WG_Group = EdDadBas_BG - EdDadBas_RBD,
  EdDadHE_WG_Group = EdDadHE_BG - EdDadHE_RBD,
  FamIncome_Q2_WG_Group = FamIncome_Q2_BG - FamIncome_Q2_RBD,
  FamIncome_Q3_WG_Group = FamIncome_Q3_BG - FamIncome_Q3_RBD,
  FamIncome_Q4_WG_Group = FamIncome_Q4_BG - FamIncome_Q4_RBD,
  FamIncome_Q5_WG_Group = FamIncome_Q5_BG - FamIncome_Q5_RBD,
  RKG_score_WG_Group = RKG_score_BG - RKG_score_RBD)

#data <- data %>%
  select(RBD, MAT_score,
TypeHumanist, SectorPriv,
SexFem_BG, EdMomBas_BG, EdMomHE_BG, EdDadBas_BG, EdDadHE_BG,
FamIncome_Q2_BG, FamIncome_Q3_BG, FamIncome_Q4_BG, FamIncome_Q5_BG, RKG_score_BG,
SexFem_WG_Grand, EdMomBas_WG_Grand, EdMomHE_WG_Grand, EdDadBas_WG_Grand, EdDadHE_WG_Grand,
FamIncome_Q2_WG_Grand, FamIncome_Q3_WG_Grand, FamIncome_Q4_WG_Grand, FamIncome_Q5_WG_Grand, RKG_score_WG_Grand,
SexFem_WG_Group, EdMomBas_WG_Group, EdMomHE_WG_Group, EdDadBas_WG_Group, EdDadHE_WG_Group,
FamIncome_Q2_WG_Group, FamIncome_Q3_WG_Group, FamIncome_Q4_WG_Group, FamIncome_Q5_WG_Group, RKG_score_WG_Group)

data <- data %>%
  select(RBD, MAT_score,
         TypeHumanist, SectorPriv,
         EdMomBas, EdMomHE, EdDadBas, EdDadHE,
         FamIncome_Q2, FamIncome_Q3, FamIncome_Q4, FamIncome_Q5,
         EdMomBas_RBD, EdMomHE_RBD, EdDadBas_RBD, EdDadHE_RBD,
         FamIncome_Q2_RBD, FamIncome_Q3_RBD, FamIncome_Q4_RBD, FamIncome_Q5_RBD,
         EdMomBas_BG_40, EdMomHE_BG_40, EdDadBas_BG_40, EdDadHE_BG_40,
         FamIncome_Q2_BG_30, FamIncome_Q3_BG_30, FamIncome_Q4_BG_30, FamIncome_Q5_BG_30)
  

# Variables ####
names(data)

#RBD
#TypeTech
#SectorPriv
#SexFem_BG
#EdMomBas_BG
#EdMomHE_BG
#EdDadBas_BG
#EdDadHE_BG
#FamIncome_Q2_BG
#FamIncome_Q3_BG
#FamIncome_Q4_BG
#FamIncome_Q5_BG
#RKG_score_BG
#SexFem_WG_Grand
#EdMomBas_WG_Grand
#EdMomHE_WG_Grand
#EdDadBas_WG_Grand
#EdDadHE_WG_Grand
#FamIncome_Q2_WG_Grand
#FamIncome_Q3_WG_Grand
#FamIncome_Q4_WG_Grand
#FamIncome_Q5_WG_Grand
#RKG_score_WG_Grand
#SexFem_WG_Group
#EdMomBas_WG_Group
#EdMomHE_WG_Group
#EdDadBas_WG_Group
#EdDadHE_WG_Group
#FamIncome_Q2_WG_Group
#FamIncome_Q3_WG_Group
#FamIncome_Q4_WG_Group
#FamIncome_Q5_WG_Group
#RKG_score_WG_Group  

# Data analysis ####
library(nlme) # To estimate MLMs with R structures
library(emmeans) # To get model-implied means
library(lmerTest) # To get Satterthwaite DDF in lmer
library(performance) # To get ICC in lmer
library(margins) # To get predicted values like Stata does

# Group and Grand mean centering for level 1 categorical predictors ####
## Single-Level Empty Means, Random Intercept
Empty_RI <- lmer(
  formula = MAT_score ~ 1 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Empty_RI, ddf="Satterthwaite"); llikAIC(Empty_RI, chkREML=FALSE)

ranova(Empty_RI, reduce.term=TRUE)

icc_unc <- data.frame(VarCorr(Empty_RI),comp=c("Variance"))
round(icc_unc[1,4]/(sum(icc_unc[,4])),3)

## School predictors
School_RI <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RI, ddf="Satterthwaite"); llikAIC(School_RI, chkREML=FALSE)

contestMD(School_RI, ddf="Satterthwaite", 
          L=rbind(c(0,1,0),c(0,0,1)))

School_RIb <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + TypeHumanist:SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RIb, ddf="Satterthwaite"); llikAIC(School_RIb, chkREML=FALSE)

icc_School_RIb <- data.frame(VarCorr(School_RIb),comp=c("Variance"))
round(icc_School_RIb[1,4]/(sum(icc_School_RIb[,4])),3) #Intercept variance leftover after controlling for school type and sector

((icc_unc[1,4] - icc_School_RIb[1,4]) / icc_unc[1,4]) * 100 #Pseudo R2 level 2
#The three level-2 fixed effects of type and sector explained 35.36% of the school-level intercept variance in math scores

data$PredSchool_RIb <- predict(School_RIb, re.form=NA, na.action = na.exclude)
rPredSchool_RIb <- cor.test(data$PredSchool_RIb, data$MAT_score, method="pearson")
(rPredSchool_RIb$estimate^2)*100 #Total R2
#The three fixed effects of type and sector explained 10.3% of the total variance in math scores

## Empty Means, Random Intercept models for Student-Level predictors
Sex_RI <- lmer(
  formula = MAT_score ~ 1 + SexFem_WG_Grand + (1|RBD),                          # Sex
  REML = TRUE,
  data = data)
summary(Sex_RI, ddf="Satterthwaite"); llikAIC(Sex_RI, chkREML=FALSE)
icc_Sex_RI <- data.frame(VarCorr(Sex_RI),comp=c("Variance"))
round(icc_Sex_RI[1,4]/(sum(icc_Sex_RI[,4])),3)
ranova(Sex_RI, reduce.term=TRUE)

EdMom_RI <- lmer(
  formula = MAT_score ~ 1 + EdMomBas_WG_Grand + EdMomHE_WG_Grand + (1|RBD),     # Mother's educational level
  REML = TRUE,
  data = data)
summary(EdMom_RI, ddf="Satterthwaite"); llikAIC(EdMom_RI, chkREML=FALSE)
icc_EdMom_RI <- data.frame(VarCorr(EdMom_RI),comp=c("Variance"))
round(icc_EdMom_RI[1,4]/(sum(icc_EdMom_RI[,4])),3)
ranova(EdMom_RI, reduce.term=TRUE)

EdDad_RI <- lmer(
  formula = MAT_score ~ 1 + EdDadBas_WG_Grand + EdDadHE_WG_Grand + (1|RBD),     # Father's educational level
  REML = TRUE,
  data = data)
summary(EdDad_RI, ddf="Satterthwaite"); llikAIC(EdDad_RI, chkREML=FALSE)
icc_EdDad_RI <- data.frame(VarCorr(EdDad_RI),comp=c("Variance"))
round(icc_EdDad_RI[1,4]/(sum(icc_EdDad_RI[,4])),3)
ranova(EdDad_RI, reduce.term=TRUE)

FamIncome_RI <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2_WG_Grand + FamIncome_Q3_WG_Grand + FamIncome_Q4_WG_Grand + FamIncome_Q5_WG_Grand + (1|RBD),
  REML = TRUE,
  data = data)
summary(FamIncome_RI, ddf="Satterthwaite"); llikAIC(FamIncome_RI, chkREML=FALSE) # Family income
icc_FamIncome_RI <- data.frame(VarCorr(FamIncome_RI),comp=c("Variance"))
round(icc_FamIncome_RI[1,4]/(sum(icc_FamIncome_RI[,4])),3)
ranova(FamIncome_RI, reduce.term=TRUE)

RKG_RI <- lmer(
  formula = MAT_score ~ 1 + RKG_score_WG_Grand + (1|RBD),                       # Ranking score
  REML = TRUE,
  data = data)
summary(RKG_RI, ddf="Satterthwaite"); llikAIC(RKG_RI, chkREML=FALSE)
icc_RKG_RI <- data.frame(VarCorr(RKG_RI),comp=c("Variance"))
round(icc_RKG_RI[1,4]/(sum(icc_RKG_RI[,4])),3)
ranova(RKG_RI, reduce.term=TRUE)
# All level 1 predictors (Grand-mean-centered) had significant level 2 residual variance (via significant LRT for
# conditional random intercepts)

## Multivel modeling

Mod_a <- lmer(
  formula = MAT_score ~ 1 +
    FamIncome_Q2_WG_Grand + FamIncome_Q3_WG_Grand + FamIncome_Q4_WG_Grand + FamIncome_Q5_WG_Grand +  #level 1
    FamIncome_Q2_BG + FamIncome_Q3_BG + FamIncome_Q4_BG + FamIncome_Q5_BG +                          #level 2 Gran-MC
    TypeHumanist + SectorPriv + TypeHumanist:SectorPriv +                                            #level 2 predictors
    (1|RBD),                                                                                         #variance components
  REML = TRUE,
  data = data)
summary(Mod_a, ddf="Satterthwaite"); llikAIC(Mod_a, chkREML=FALSE)
icc_RKG_RI <- data.frame(VarCorr(RKG_RI),comp=c("Variance"))
round(icc_RKG_RI[1,4]/(sum(icc_RKG_RI[,4])),3)

Mod_b <- lmer(
  formula = MAT_score ~ 1 +
    FamIncome_Q2_WG_Grand + FamIncome_Q3_WG_Grand + FamIncome_Q4_WG_Grand + FamIncome_Q5_WG_Grand +  #level 1
    EdMomBas_WG_Grand + EdMomHE_WG_Grand +
    FamIncome_Q2_BG + FamIncome_Q3_BG + FamIncome_Q4_BG + FamIncome_Q5_BG +                          #level 2 Gran-MC
    EdMomBas_BG + EdMomHE_BG +
    TypeHumanist + SectorPriv + TypeHumanist:SectorPriv +                                            #level 2 predictors
    (1|RBD),                                                                                         #variance components
  REML = TRUE,
  data = data)
summary(Mod_b, ddf="Satterthwaite"); llikAIC(Mod_b, chkREML=FALSE)
icc_RKG_RI <- data.frame(VarCorr(RKG_RI),comp=c("Variance"))
round(icc_RKG_RI[1,4]/(sum(icc_RKG_RI[,4])),3)

Mod_c <- lmer(
  formula = MAT_score ~ 1 +
    FamIncome_Q2_WG_Grand + FamIncome_Q3_WG_Grand + FamIncome_Q4_WG_Grand + FamIncome_Q5_WG_Grand +  #level 1
    EdMomBas_WG_Grand + EdMomHE_WG_Grand +
    EdDadBas_WG_Grand + EdDadHE_WG_Grand +
    FamIncome_Q2_BG + FamIncome_Q3_BG + FamIncome_Q4_BG + FamIncome_Q5_BG +                          #level 2 Gran-MC
    EdMomBas_BG + EdMomHE_BG +
    EdDadBas_BG + EdDadHE_BG +
    TypeHumanist + SectorPriv + TypeHumanist:SectorPriv +                                            #level 2 predictors
    (1|RBD),                                                                                         #variance components
  REML = TRUE,
  data = data)
summary(Mod_c, ddf="Satterthwaite"); llikAIC(Mod_c, chkREML=FALSE)
icc_RKG_RI <- data.frame(VarCorr(RKG_RI),comp=c("Variance"))
round(icc_RKG_RI[1,4]/(sum(icc_RKG_RI[,4])),3)

# Example 3b: Fixed and Random Effects in General Multilevel Models for Two-Level Nested Outcomes ####

## Two-Level Empty Means, Random Intercept for Math Outcome (for pupil p in school s) ####
Empty_RI <- lmer(
  formula = MAT_score ~ 1 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Empty_RI, ddf="Satterthwaite"); llikAIC(Empty_RI, chkREML=FALSE)
ranova(Empty_RI, reduce.term=TRUE)

icc_unc <- data.frame(VarCorr(Empty_RI),comp=c("Variance"))
round(icc_unc[1,4]/(sum(icc_unc[,4])),3)

#Design effect using mean #students per school: = 1 + ((n – 1) * ICC) = 1 + [((188774/3151)−1)*.29] = 18.08368
# Effective sample size: Neffective = (#Total Obs) / Design Effect = 188774 / 18.08368 = 10438.92 
# 95% random effect confidence interval for the intercept across schools:  Fixed effect ± 1.96*SQRT(random variance) 
# 484.556 ± 1.96 * SQRT(4162) =  358.1094 to 611.0026 
# 95% of our sample’s schools are predicted to have school mean math from 358.1094 to 611.0026
  
## Adding Fixed Effects of Student (Level 1) and School Proportion (Level 2) ####

# Sex
Math_Sex_RI <- lmer(
  formula = MAT_score ~ 1 + SexFem + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_Sex_RI, ddf="Satterthwaite"); llikAIC(Math_Sex_RI, chkREML=FALSE)

#ranova(Math_Sex_RI, reduce.term=TRUE)
icc_Math_Sex_RI <- data.frame(VarCorr(Math_Sex_RI),comp=c("Variance"))
round(icc_Math_Sex_RI[1,4]/(sum(icc_Math_Sex_RI[,4])),3)
(icc_unc[1,4] - icc_Math_Sex_RI[1,4]) / icc_unc[1,4] #Pseudo R2 level 2

Math_SexBG_RI <- lmer(
  formula = MAT_score ~ 1 + SexFem + SexFem_BG_50 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_SexBG_RI, ddf="Satterthwaite"); llikAIC(Math_SexBG_RI, chkREML=FALSE)
# Sex at level 2 is not significant

#Mother's Educational level
Math_EdMom_RI <- lmer(
  formula = MAT_score ~ 1 + EdMomBas + EdMomHE + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdMom_RI, ddf="Satterthwaite"); llikAIC(Math_EdMom_RI, chkREML=FALSE)

icc_Math_EdMom_RI <- data.frame(VarCorr(Math_EdMom_RI),comp=c("Variance"))
round(icc_Math_EdMom_RI[1,4]/(sum(icc_Math_EdMom_RI[,4])),3)
(icc_unc[1,4] - icc_Math_EdMom_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 to check if it was reduced

Math_EdMomBG_RI <- lmer(
  formula = MAT_score ~ 1 + EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdMomBG_RI, ddf="Satterthwaite"); llikAIC(Math_EdMomBG_RI, chkREML=FALSE)

icc_Math_EdMomBG_RI <- data.frame(VarCorr(Math_EdMomBG_RI),comp=c("Variance"))
round(icc_Math_EdMomBG_RI[1,4]/(sum(icc_Math_EdMomBG_RI[,4])),3)
(icc_unc[1,4] - icc_Math_EdMomBG_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_Math_EdMomBG_RI[2,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$Math_EdMomBG <- predict(Math_EdMomBG_RI, re.form=NA, na.action = na.exclude)
(cor.test(data$Math_EdMomBG, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1

#Father's Educational level
Math_EdDad_RI <- lmer(
  formula = MAT_score ~ 1 + EdDadBas + EdDadHE + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdDad_RI, ddf="Satterthwaite"); llikAIC(Math_EdDad_RI, chkREML=FALSE)

icc_Math_EdDad_RI <- data.frame(VarCorr(Math_EdDad_RI),comp=c("Variance"))
round(icc_Math_EdDad_RI[1,4]/(sum(icc_Math_EdDad_RI[,4])),3)
(icc_unc[1,4] - icc_Math_EdDad_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 to check if it was reduced

Math_EdDadBG_RI <- lmer(
  formula = MAT_score ~ 1 + EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdDadBG_RI, ddf="Satterthwaite"); llikAIC(Math_EdDadBG_RI, chkREML=FALSE)

icc_Math_EdDadBG_RI <- data.frame(VarCorr(Math_EdDadBG_RI),comp=c("Variance"))
round(icc_Math_EdDadBG_RI[1,4]/(sum(icc_Math_EdDadBG_RI[,4])),3)
(icc_unc[1,4] - icc_Math_EdDadBG_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_Math_EdDadBG_RI[2,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$Math_EdDadBG <- predict(Math_EdDadBG_RI, re.form=NA, na.action = na.exclude)
(cor.test(data$Math_EdDadBG, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1

#Family income
Math_FamIncome_RI <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_FamIncome_RI, ddf="Satterthwaite"); llikAIC(Math_FamIncome_RI, chkREML=FALSE)

icc_Math_FamIncome_RI <- data.frame(VarCorr(Math_FamIncome_RI),comp=c("Variance"))
round(icc_Math_FamIncome_RI[1,4]/(sum(icc_Math_FamIncome_RI[,4])),3)
(icc_unc[1,4] - icc_Math_FamIncome_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 to check if it was reduced

Math_FamIncomeBG_RI <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + 
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Math_FamIncomeBG_RI, ddf="Satterthwaite"); llikAIC(Math_FamIncomeBG_RI, chkREML=FALSE)

icc_Math_FamIncomeBG_RI <- data.frame(VarCorr(Math_FamIncomeBG_RI),comp=c("Variance"))
round(icc_Math_FamIncomeBG_RI[1,4]/(sum(icc_Math_FamIncomeBG_RI[,4])),3)
(icc_unc[1,4] - icc_Math_FamIncomeBG_RI[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_Math_FamIncomeBG_RI[2,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$Math_FamIncomeBG <- predict(Math_FamIncomeBG_RI, re.form=NA, na.action = na.exclude)
(cor.test(data$Math_FamIncomeBG, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1

## Adding a Random Effect of Student Free/Reduced Lunch (over Schools) ####

#Mother's Educational level
Math_EdMomBG_RIS <- lmer(
  formula = MAT_score ~ 1 + EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 + (1 + EdMomBas_RBD + EdMomHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdMomBG_RIS, ddf="Satterthwaite")

ranova(Math_EdMomBG_RIS, reduce.term=TRUE)
# 95% random effects CI for the random slope
VC_EdMomBG_RIS <- data.frame(VarCorr(Math_EdMomBG_RIS))
FE_slp1_EdMomBG_RIS <- Math_EdMomBG_RIS@beta[2]
RF_slp1_EdMomBG_RIS <- VC_EdMomBG_RIS[2,'sdcor']
FE_slp1_EdMomBG_RIS - (1.96 * RF_slp1_EdMomBG_RIS); FE_slp1_EdMomBG_RIS+ (1.96 * RF_slp1_EdMomBG_RIS)

FE_slp2_EdMomBG_RIS <- Math_EdMomBG_RIS@beta[3]
RF_slp2_EdMomBG_RIS <- VC_EdMomBG_RIS[3,'sdcor']
FE_slp2_EdMomBG_RIS - (1.96 * RF_slp2_EdMomBG_RIS); FE_slp2_EdMomBG_RIS+ (1.96 * RF_slp2_EdMomBG_RIS)

#Father's Educational level
Math_EdDadBG_RIS <- lmer(
  formula = MAT_score ~ 1 + EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 + (1 + EdDadBas_RBD + EdDadHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(Math_EdDadBG_RIS, ddf="Satterthwaite")

ranova(Math_EdDadBG_RIS, reduce.term=TRUE)
# 95% random effects CI for the random slope
VC_EdDadBG_RIS <- data.frame(VarCorr(Math_EdDadBG_RIS))
FE_slp1_EdDadBG_RIS <- Math_EdDadBG_RIS@beta[2]
RF_slp1_EdDadBG_RIS <- VC_EdDadBG_RIS[2,'sdcor']
FE_slp1_EdDadBG_RIS - (1.96 * RF_slp1_EdDadBG_RIS); FE_slp1_EdDadBG_RIS+ (1.96 * RF_slp1_EdDadBG_RIS)

FE_slp2_EdDadBG_RIS <- Math_EdDadBG_RIS@beta[3]
RF_slp2_EdDadBG_RIS <- VC_EdDadBG_RIS[3,'sdcor']
FE_slp2_EdDadBG_RIS - (1.96 * RF_slp2_EdDadBG_RIS); FE_slp2_EdDadBG_RIS+ (1.96 * RF_slp2_EdDadBG_RIS)

#Family income
Math_FamIncomeBG_RIS <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + 
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD|RBD),
  REML = TRUE,
  data = data)
summary(Math_FamIncomeBG_RIS, ddf="Satterthwaite")

ranova(Math_FamIncomeBG_RIS, reduce.term=TRUE)
#FamIncome_Q2_RBD in (1 + FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD | RBD)  2.779e-07
#FamIncome_Q3_RBD in (1 + FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD | RBD)  3.414e-05
#FamIncome_Q4_RBD in (1 + FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD | RBD)  9.921e-15
#FamIncome_Q5_RBD in (1 + FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD | RBD)  1.200e-09
# 95% random effects CI for the random slope
VC_FamIncomeBG_RIS <- data.frame(VarCorr(Math_FamIncomeBG_RIS))
FE_slp1_FamIncomeBG_RIS <- Math_FamIncomeBG_RIS@beta[2]
RF_slp1_FamIncomeBG_RIS <- VC_FamIncomeBG_RIS[2,'sdcor']
FE_slp1_FamIncomeBG_RIS - (1.96 * RF_slp1_FamIncomeBG_RIS); FE_slp1_FamIncomeBG_RIS+ (1.96 * RF_slp1_FamIncomeBG_RIS)

FE_slp2_FamIncomeBG_RIS <- Math_FamIncomeBG_RIS@beta[3]
RF_slp2_FamIncomeBG_RIS <- VC_FamIncomeBG_RIS[3,'sdcor']
FE_slp2_FamIncomeBG_RIS - (1.96 * RF_slp2_FamIncomeBG_RIS); FE_slp2_FamIncomeBG_RIS+ (1.96 * RF_slp2_FamIncomeBG_RIS)

FE_slp3_FamIncomeBG_RIS <- Math_FamIncomeBG_RIS@beta[4]
RF_slp3_FamIncomeBG_RIS <- VC_FamIncomeBG_RIS[4,'sdcor']
FE_slp3_FamIncomeBG_RIS - (1.96 * RF_slp3_FamIncomeBG_RIS); FE_slp3_FamIncomeBG_RIS+ (1.96 * RF_slp3_FamIncomeBG_RIS)

FE_slp4_FamIncomeBG_RIS <- Math_FamIncomeBG_RIS@beta[5]
RF_slp4_FamIncomeBG_RIS <- VC_FamIncomeBG_RIS[5,'sdcor']
FE_slp4_FamIncomeBG_RIS - (1.96 * RF_slp4_FamIncomeBG_RIS); FE_slp4_FamIncomeBG_RIS+ (1.96 * RF_slp4_FamIncomeBG_RIS)

## MLM with significant fixed and random effects ####
MLM_EdMom <- lmer(
  formula = MAT_score ~ 1 + EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    (1 + EdMomBas_RBD + EdMomHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_EdMom, ddf="Satterthwaite"); llikAIC(MLM_EdMom, chkREML=FALSE)

ranova(MLM_EdMom, reduce.term=TRUE)

icc_MLM_EdMom <- data.frame(VarCorr(MLM_EdMom),comp=c("Variance"))[c(1:3,7),]
round(icc_MLM_EdMom[1,4]/(sum(icc_MLM_EdMom[,4])),3)
(icc_unc[1,4] - icc_MLM_EdMom[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_MLM_EdMom[4,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$MLM_EdMom <- predict(MLM_EdMom, re.form=NA, na.action = na.exclude)
(cor.test(data$MLM_EdMom, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1


MLM_EdMom_EdDada <- lmer(
  formula = MAT_score ~ 1 + 
    EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 +
    (1 + EdMomBas_RBD + EdMomHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_EdMom_EdDada, ddf="Satterthwaite"); llikAIC(MLM_EdMom_EdDada, chkREML=FALSE)

MLM_EdMom_EdDadb <- lmer(
  formula = MAT_score ~ 1 + 
    EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 +
    (1 + EdMomBas_RBD + EdMomHE_RBD + EdDadBas_RBD + EdDadHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_EdMom_EdDadb, ddf="Satterthwaite"); llikAIC(MLM_EdMom_EdDadb, chkREML=FALSE)

#LRT
DevTest = -2*(logLik(MLM_EdMom_EdDada)-logLik(MLM_EdMom_EdDadb))
RegPvalue = pchisq((DevTest), df=3, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

icc_MLM_EdMom <- data.frame(VarCorr(MLM_EdMom),comp=c("Variance"))[c(1:3,7),]
round(icc_MLM_EdMom[1,4]/(sum(icc_MLM_EdMom[,4])),3)
(icc_unc[1,4] - icc_MLM_EdMom[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_MLM_EdMom[4,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$MLM_EdMom <- predict(MLM_EdMom, re.form=NA, na.action = na.exclude)
(cor.test(data$MLM_EdMom, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1


MLM_Full_lvl1a <- lmer(
  formula = MAT_score ~ 1 + 
    EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + 
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + EdMomBas_RBD + EdMomHE_RBD + EdDadBas_RBD + EdDadHE_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_Full_lvl1a, ddf="Satterthwaite"); llikAIC(MLM_Full_lvl1a, chkREML=FALSE)

MLM_Full_lvl1b <- lmer(
  formula = MAT_score ~ 1 + 
    EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + 
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + EdMomBas_RBD + EdMomHE_RBD + EdDadBas_RBD + EdDadHE_RBD +
       FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_Full_lvl1b, ddf="Satterthwaite"); llikAIC(MLM_Full_lvl1b, chkREML=FALSE)

#LRT
DevTest = -2*(logLik(MLM_EdMom_EdDada)-logLik(MLM_EdMom_EdDadb))
RegPvalue = pchisq((DevTest), df=3, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

icc_MLM_EdMom <- data.frame(VarCorr(MLM_EdMom),comp=c("Variance"))[c(1:3,7),]
round(icc_MLM_EdMom[1,4]/(sum(icc_MLM_EdMom[,4])),3)
(icc_unc[1,4] - icc_MLM_EdMom[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model
(icc_unc[2,4] - icc_MLM_EdMom[4,4]) / icc_unc[2,4] #PseudoR2 at level 1 relative to empty model

data$MLM_EdMom <- predict(MLM_EdMom, re.form=NA, na.action = na.exclude)
(cor.test(data$MLM_EdMom, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1


MLM_Full <- lmer(
  formula = MAT_score ~ 1 + 
    EdMomBas + EdMomHE + EdMomBas_BG_40 + EdMomHE_BG_40 +
    EdDadBas + EdDadHE + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2 + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 + 
    TypeHumanist + SectorPriv +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + EdMomBas_RBD + EdMomHE_RBD + EdDadBas_RBD + EdDadHE_RBD +
       FamIncome_Q2_RBD + FamIncome_Q3_RBD + FamIncome_Q4_RBD + FamIncome_Q5_RBD|RBD),
  REML = TRUE,
  data = data)
summary(MLM_Full, ddf="Satterthwaite"); llikAIC(MLM_Full, chkREML=FALSE)

icc_MLM_EdMom <- data.frame(VarCorr(MLM_EdMom),comp=c("Variance"))[c(1:3,7),]
round(icc_MLM_EdMom[1,4]/(sum(icc_MLM_EdMom[,4])),3)
(icc_unc[1,4] - icc_MLM_EdMom[1,4]) / icc_unc[1,4] #PseudoR2 at level 2 relative to empty model

data$MLM_EdMom <- predict(MLM_EdMom, re.form=NA, na.action = na.exclude)
(cor.test(data$MLM_EdMom, data$MAT_score, method="pearson")$estimate)^2 #TotalR2 relative to empty model 1


















