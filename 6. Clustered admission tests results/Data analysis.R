# 1. Data sets ####
library(plyr)
library(dplyr)

# Test-takers data set
registered <- readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PDT_2021_PUB_MRUN.csv")
registered <- select(registered,
                     MRUN, RBD,CODIGO_ENS,RAMA_EDUCACIONAL,DEPENDENCIA,MATE_ACTUAL,PUNTAJES_PROCESO)

schools <- data.frame(registered %>%
  group_by(RBD) %>% 
  summarise(n_original = n()))

registered <- data.frame(filter(registered, PUNTAJES_PROCESO == 1 & CODIGO_ENS %in% c(310,410,510,610,710,810,910) & MATE_ACTUAL > 149))
registered$PUNTAJES_PROCESO <- NULL
registered$CODIGO_ENS <- NULL
registered <- registered %>% filter(RBD != 99999)

# the sample are students who participated for the first time in the 2021 admission process
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
data_recode <- data %>%
  mutate(
  TypeHumanist = recode(RAMA_EDUCACIONAL,
                        'H1' = 1,
                        'T1' = 0,
                        'T2' = 0,
                        'T3' = 0,
                        'T4' = 0,
                        'T5' = 0),
  SectorSubsidized = ifelse(DEPENDENCIA == 3, 1,0),
  SectorPriv = ifelse(DEPENDENCIA == 4, 1,0),
  EdMomHS = ifelse(EDUCACION_MADRE >= 6 & EDUCACION_MADRE <= 9, 1,
                    ifelse(is.na(EDUCACION_MADRE) == T | EDUCACION_MADRE == 13, NA,0)),
  EdMomHE = ifelse(EDUCACION_MADRE >= 10 & EDUCACION_MADRE <= 12, 1,
                   ifelse(is.na(EDUCACION_MADRE) == T | EDUCACION_MADRE == 13, NA,0)),
  EdDadHS = ifelse(EDUCACION_PADRE >= 6 & EDUCACION_PADRE <= 9, 1,
                    ifelse(is.na(EDUCACION_PADRE) == T | EDUCACION_PADRE == 13, NA,0)),
  EdDadHE = ifelse(EDUCACION_PADRE >= 10 & EDUCACION_PADRE <= 12, 1,
                   ifelse(is.na(EDUCACION_PADRE) == T | EDUCACION_PADRE == 13, NA,0)),
  #category 13 means IDK
  FamIncome_Q2 = ifelse(INGRESO_PERCAPITA_GRUPO_FA == 3 | INGRESO_PERCAPITA_GRUPO_FA == 4, 1, 0),
  FamIncome_Q3 = ifelse(INGRESO_PERCAPITA_GRUPO_FA == 5 | INGRESO_PERCAPITA_GRUPO_FA == 6, 1, 0),
  FamIncome_Q4 = ifelse(INGRESO_PERCAPITA_GRUPO_FA == 7 | INGRESO_PERCAPITA_GRUPO_FA == 8, 1, 0),
  FamIncome_Q5 = ifelse(INGRESO_PERCAPITA_GRUPO_FA == 9 | INGRESO_PERCAPITA_GRUPO_FA == 10, 1, 0))

data <- select(data_recode, RBD, MATE_ACTUAL, TypeHumanist:FamIncome_Q5) %>% arrange(RBD)
rm(data_recode)

schoolsb <- data %>%
  group_by(RBD) %>% 
  summarise(n_analysis = n())

schools <- left_join(schools, schoolsb, by = "RBD")
rm(schoolsb)

schools <- schools %>% mutate(
  n_analysis = ifelse(is.na(n_analysis) == T, 0, n_analysis),
  prop_valid = n_analysis/n_original)

schools <- schools %>% 
  filter(prop_valid >= .65 & n_analysis > 9)

schools$filter <- 1

data <- left_join(data, 
          schools %>% select(RBD, filter), by = "RBD")
data <- data %>% filter(filter == 1)
data$filter <- NULL
rm(schools)

# 2. Data Transformation #####

# How many students do we need for each school? 135 schools have 1 student    
# data %>% group_by(RBD) %>% summarise(n = n()) %>% arrange(n) %>% View()

## 2.1 Level-2, Group Mean (GM) predictors ####
rbd_means <- data %>% #school means
  group_by(RBD) %>%
  summarise(
    EdMomHS_RBD = mean(EdMomHS, na.rm = T),
    EdMomHE_RBD = mean(EdMomHE, na.rm = T),
    EdDadHS_RBD = mean(EdDadHS, na.rm = T),
    EdDadHE_RBD = mean(EdDadHE, na.rm = T),
    FamIncome_Q2_RBD = mean(FamIncome_Q2, na.rm = T),
    FamIncome_Q3_RBD = mean(FamIncome_Q3, na.rm = T),
    FamIncome_Q4_RBD = mean(FamIncome_Q4, na.rm = T),
    FamIncome_Q5_RBD = mean(FamIncome_Q5, na.rm = T))

## 2.2 Level 2 predictors centered at a constant ####
rbd_means <- rbd_means %>% # Difference between group mean and constant values
  mutate(
    EdMomHS_BG_40 = EdMomHS_RBD - .40,
    EdMomHE_BG_40 = EdMomHE_RBD - .40,
    EdDadHS_BG_40 = EdDadHS_RBD - .40,
    EdDadHE_BG_40 = EdDadHE_RBD - .40,
    FamIncome_Q2_BG_30 = FamIncome_Q2_RBD - .30,
    FamIncome_Q3_BG_30 = FamIncome_Q3_RBD - .30,
    FamIncome_Q4_BG_30 = FamIncome_Q4_RBD - .30,
    FamIncome_Q5_BG_30 = FamIncome_Q5_RBD - .30)

## 2.3 Level 1 predictors centered at a constant ####
data <- left_join(data, rbd_means, by = "RBD")

rm(rbd_means)

data <- data %>% #school-mean-centered predictors for random components
  mutate(
    EdMomHS_CMC = EdMomHS - EdMomHS_RBD,
    EdMomHE_CMC = EdMomHE - EdMomHE_RBD,
    EdDadHS_CMC = EdDadHS - EdDadHS_RBD,
    EdDadHE_CMC = EdDadHE_RBD,
    FamIncome_Q2_CMC = FamIncome_Q2 - FamIncome_Q2_RBD,
    FamIncome_Q3_CMC = FamIncome_Q3 - FamIncome_Q3_RBD,
    FamIncome_Q4_CMC = FamIncome_Q4 - FamIncome_Q4_RBD,
    FamIncome_Q5_CMC = FamIncome_Q5 - FamIncome_Q5_RBD)

## 2.4 Predictors ####
data <- data %>%
  select(RBD, #ID variable
         MATE_ACTUAL, # Dependent variable
         #Level 2 predictors
         TypeHumanist, SectorSubsidized, SectorPriv,                                     
         # Within group variation of level 1 predictors (Dummy-coded variables)
         EdMomHS, EdMomHE,
         EdDadHS, EdDadHE,
         FamIncome_Q2, FamIncome_Q3, FamIncome_Q4, FamIncome_Q5,
         # Between group variation (Grand mean centering) of level 1 predictors
         EdMomHS_BG_40, EdMomHE_BG_40,
         EdDadHS_BG_40, EdDadHE_BG_40,
         FamIncome_Q2_BG_30, FamIncome_Q3_BG_30, FamIncome_Q4_BG_30, FamIncome_Q5_BG_30,
         # CMC variables for random components
         EdMomHS_CMC, EdMomHE_CMC,
         EdDadHS_CMC, EdDadHE_CMC,
         FamIncome_Q2_CMC, FamIncome_Q3_CMC, FamIncome_Q4_CMC, FamIncome_Q5_CMC)

#data %>% select(RBD,EdMomBas,EdMomBas_RBD,EdMomBas_BG_40,EdMomBas_WG_40,EdMomBas_CMC) %>% View()

# 3. Data analysis ####
library(nlme) # To estimate MLMs with R structures
library(emmeans) # To get model-implied means
library(lmerTest) # To get Satterthwaite DDF in lmer
library(performance) # To get ICC in lmer
library(margins) # To get predicted values like Stata does
library(ordinal) #MGLM for Ordinal Outcome Variable (Holmes, p.151)

## 3.1 Single-Level Empty Means, Random Intercept ####
Empty_RI <- lmer(
  formula = MATE_ACTUAL ~ 1 + (1|RBD),
  REML = TRUE,
  data = data)
summary(Empty_RI, ddf="Satterthwaite"); llikAIC(Empty_RI, chkREML=FALSE)

ranova(Empty_RI, reduce.term=TRUE)

ICC_Empty_RI <- data.frame(VarCorr(Empty_RI),comp=c("Variance"))
round(ICC_Empty_RI[1,4]/(sum(ICC_Empty_RI[,4])),3)

168816 / (1 + ((70.34 - 1) * .29)) #Design effect using mean #students per school
#Our power to detect level-1 effects will be approximately that of an independent sample of 7,997 students

## 3.2 School predictors (level 2) ####
School_RI <- lmer(
  formula = MATE_ACTUAL ~ 1 + TypeHumanist + SectorSubsidized + SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RI, ddf="Satterthwaite"); llikAIC(School_RI, chkREML=FALSE)

School_RIb <- lmer(
  formula = MATE_ACTUAL ~ 1 + TypeHumanist + SectorSubsidized + SectorPriv + TypeHumanist:SectorSubsidized +
    TypeHumanist:SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RIb, ddf="Satterthwaite"); llikAIC(School_RIb, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(School_RIb),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3) #Random intercept variance leftover after controlling for school type and sector
((ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4]) * 100 #Pseudo R2 level 2
#The three level-2 fixed effects of type and sector explained 35.36% of the school-level intercept variance in math scores

data$PredSchool_RIb <- predict(School_RIb, re.form=NA, na.action = na.exclude)
rPredSchool_RIb <- cor.test(data$PredSchool_RIb, data$MAT_score, method="pearson")
(rPredSchool_RIb$estimate^2)*100 #Total R2
#The three fixed effects of type and sector explained 10.3% of the total variance in math scores

## 3.3 Empty Means, Random Intercept models for Student-Level predictors ####

empty <- data %>% mutate(
  edmom = ifelse(EdMomHS == 1, 1, ifelse(EdMomHE == 1, 2, 0)),
  eddad = ifelse(EdDadHS == 1, 1, ifelse(EdDadHE == 1, 2, 0)),
  faminc = ifelse(FamIncome_Q2 == 1, 1,
                  ifelse(FamIncome_Q2 == 1,2,
                         ifelse(FamIncome_Q3 == 1, 3, 
                                ifelse(FamIncome_Q4 == 4,1,0)))),
  edmom = factor(edmom),
  eddad = factor(eddad),
  faminc = factor(faminc))


#Mother's Educational level
#GLM models to compute ICC
emp_mom <- clmm(edmom~1+(1|RBD), data = empty)

EdMom_EM_RI <- lmer(
  formula = MAT_score ~ 1 + EdMomBas_WG_40 + EdMomHE_WG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(EdMom_EM_RI, ddf="Satterthwaite"); llikAIC(EdMom_EM_RI, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(EdMom_EM_RI),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 to check if it was reduced

EdMom_EM_RIb <- lmer(
  formula = MAT_score ~ 1 + EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(EdMom_EM_RIb, ddf="Satterthwaite"); llikAIC(EdMom_EM_RIb, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(EdMom_EM_RIb),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$EdMom_EM_RIb <- predict(EdMom_EM_RIb, re.form=NA, na.action = na.exclude)
(cor.test(data$EdMom_EM_RIb, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

#Father's Educational level
EdDad_EM_RI <- lmer(
  formula = MAT_score ~ 1 + EdDadBas_WG_40 + EdDadHE_WG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(EdDad_EM_RI, ddf="Satterthwaite"); llikAIC(EdDad_EM_RI, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(EdDad_EM_RI),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 to check if it was reduced

EdDad_EM_RIb <- lmer(
  formula = MAT_score ~ 1 + EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(EdDad_EM_RIb, ddf="Satterthwaite"); llikAIC(EdDad_EM_RIb, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(EdDad_EM_RIb),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$EdDad_EM_RIb <- predict(EdDad_EM_RIb, re.form=NA, na.action = na.exclude)
(cor.test(data$EdDad_EM_RIb, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

#Family income
FamIncome_EM_RI <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 + (1|RBD),
  REML = TRUE,
  data = data)
summary(FamIncome_EM_RI, ddf="Satterthwaite"); llikAIC(FamIncome_EM_RI, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(FamIncome_EM_RI),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 to check if it was reduced

FamIncome_EM_RIb <- lmer(
  formula = MAT_score ~ 1 + FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + (1|RBD),
  REML = TRUE,
  data = data)
summary(FamIncome_EM_RIb, ddf="Satterthwaite"); llikAIC(FamIncome_EM_RIb, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(FamIncome_EM_RIb),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$FamIncome_EM_RIb <- predict(FamIncome_EM_RIb, re.form=NA, na.action = na.exclude)
(cor.test(data$FamIncome_EM_RIb, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

# All level 1 predictors (Grand-mean-centered) had significant level 2 residual variance.

## 3.4 Adding L2 and L1 Grand-mean-centered predictors ####

# Adding Mother education
School_EdMom <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + TypeHumanist:SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_EdMom, ddf="Satterthwaite"); llikAIC(School_EdMom, chkREML=FALSE)

School_EdMomb <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_EdMomb, ddf="Satterthwaite"); llikAIC(School_EdMomb, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(School_EdMomb),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$School_EdMomb <- predict(School_EdMomb, re.form=NA, na.action = na.exclude)
(cor.test(data$School_EdMomb, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

# Adding Dad education
School_EdMomb_EdDad <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + 
    EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_EdMomb_EdDad, ddf="Satterthwaite"); llikAIC(School_EdMomb_EdDad, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(School_EdMomb_EdDad),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$School_EdMomb_EdDad <- predict(School_EdMomb_EdDad, re.form=NA, na.action = na.exclude)
(cor.test(data$School_EdMomb_EdDad, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

School_EdMomb_EdDad_FamIncome <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + 
    EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_EdMomb_EdDad_FamIncome, ddf="Satterthwaite"); llikAIC(School_EdMomb_EdDad_FamIncome, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(School_EdMomb_EdDad_FamIncome),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$School_EdMomb_EdDad_FamIncome <- predict(School_EdMomb_EdDad_FamIncome, re.form=NA, na.action = na.exclude)
(cor.test(data$School_EdMomb_EdDad_FamIncome, data$MAT_score, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

161015 / (1 + ((51 - 1) * .09)) #Design effect using mean #students per school

# Our conditional model has reduced the design effect because a greater proportion of level-2
# random intercept variance was explained (.764) relative to level-1 residual variance (.024).
# This means that our power to detect level-1 effects has improved — it is currently approximately 
# that of an independent sample of 29275 students (versus 10424 from the empty means model).  

## 3.5 Adding Random Effects of Student predictors (over Schools) ####
MLM_RE_EdMom <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + 
    EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + 
    (1 + EdMomBas_CMC + EdMomHE_CMC|RBD),
  REML = TRUE,
  data = data)
summary(MLM_RE_EdMom, ddf="Satterthwaite"); llikAIC(MLM_RE_EdMom, chkREML=FALSE)

ranova(MLM_RE_EdMom, reduce.term=TRUE)

#ICC_cond <- data.frame(VarCorr(MLM_RE_EdMom),comp=c("Variance"))[c(1:3,7),]
#round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
#(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
#(ICC_Empty_RI[2,4] - ICC_cond[4,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

MLM_RE_EdMom_EdDad <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + 
    EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + 
    (1 + EdMomBas_CMC + EdMomHE_CMC + EdDadBas_CMC + EdDadHE_CMC|RBD),
  REML = TRUE,
  data = data)
summary(MLM_RE_EdMom_EdDad, ddf="Satterthwaite"); llikAIC(MLM_RE_EdMom_EdDad, chkREML=FALSE)

#ICC_cond <- data.frame(VarCorr(MLM_RE_EdMom_EdDad),comp=c("Variance"))[c(1:5,16),]
#round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
#(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
#(ICC_Empty_RI[2,4] - ICC_cond[6,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

DevTest = -2*(logLik(MLM_RE_EdMom)-logLik(MLM_RE_EdMom_EdDad))
RegPvalue = pchisq((DevTest), df=9, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

MLM_RE_EdMom_EdDad_FamIncome <- lmer(
  formula = MAT_score ~ 1 + TypeHumanist + SectorPriv + 
    EdMomBas_WG_40 + EdMomHE_WG_40 + EdMomBas_BG_40 + EdMomHE_BG_40 + 
    EdDadBas_WG_40 + EdDadHE_WG_40 + EdDadBas_BG_40 + EdDadHE_BG_40 +
    FamIncome_Q2_WG_30 + FamIncome_Q3_WG_30 + FamIncome_Q4_WG_30 + FamIncome_Q5_WG_30 +
    FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 + 
    (1 + EdMomBas_CMC + EdMomHE_CMC + EdDadBas_CMC + EdDadHE_CMC +
       FamIncome_Q2_CMC + FamIncome_Q3_CMC + FamIncome_Q4_CMC + FamIncome_Q5_CMC|RBD),
  REML = TRUE,
  data = data)
summary(MLM_RE_EdMom_EdDad_FamIncome, ddf="Satterthwaite"); llikAIC(MLM_RE_EdMom_EdDad_FamIncome, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(MLM_RE_EdMom_EdDad_FamIncome),comp=c("Variance"))[c(1:9,46),]
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[10,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

DevTest = -2*(logLik(MLM_RE_EdMom_EdDad)-logLik(MLM_RE_EdMom_EdDad_FamIncome))
RegPvalue = pchisq((DevTest), df=9, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue



























## 3.6 Pseudo standardized slopes
## 3.6 Effect size via 95% Random Effect CIs