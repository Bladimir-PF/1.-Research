# 1. Data sets ####
library(plyr)
library(dplyr)

# Test-takers data set
registered <- readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PDT_2021_PUB_MRUN.csv")
registered <- select(registered,
                     MRUN, RBD,CODIGO_ENS,RAMA_EDUCACIONAL,DEPENDENCIA,MATE_ACTUAL,PUNTAJES_PROCESO)
registered <- registered %>% filter(RBD != 99999)

schools <- data.frame(registered %>%
                        group_by(RBD) %>% 
                        summarise(n_original = n()))

registered <- data.frame(filter(registered, PUNTAJES_PROCESO == 1 & CODIGO_ENS %in% c(310,410,510,610,710,810,910) & MATE_ACTUAL > 149))
registered$PUNTAJES_PROCESO <- NULL
registered$CODIGO_ENS <- NULL

# the sample are students who participated for the first time in the 2021 admission process
# (to enter higher education in 2021) and attended schools with regular curricula (general curriculum
# for secondary school students).

# SES (complementary information) data set
SES <- readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/B_Socioeconomico/B_SOCIOECONOMICO_DOMICILIO_PDT_2021_PUB_MRUN.csv")
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

schools$RBD_ID <- 1:nrow(schools)

set.seed(12123)
train <- sample(2400, 1200)
development <- schools[train,]
rm(train)
development <- development %>% select(RBD_ID) %>% mutate(train = 1)

schools <- left_join(schools, development, by = "RBD_ID")
rm(development)

schools <- schools %>% mutate(train = ifelse(is.na(train) == T, 0, train))
schools <- select(schools, RBD, train)

data <- left_join(data, schools, by = "RBD")
rm(schools)

data <- data %>% filter(train < 2)

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
         FamIncome_Q2_CMC, FamIncome_Q3_CMC, FamIncome_Q4_CMC, FamIncome_Q5_CMC,
         train)

empty <- data %>% mutate(
  edmom = ifelse(EdMomHS == 1, 1, ifelse(EdMomHE == 1, 2, 0)),
  eddad = ifelse(EdDadHS == 1, 1, ifelse(EdDadHE == 1, 2, 0)),
  faminc = ifelse(FamIncome_Q2 == 1, 1,
                  ifelse(FamIncome_Q3 == 1, 2,
                         ifelse(FamIncome_Q4 == 1, 3, 
                                ifelse(FamIncome_Q5 == 1, 4,0)))),
  edmom = factor(edmom),
  eddad = factor(eddad),
  faminc = factor(faminc))

empty <- empty %>% select(RBD, edmom, eddad, faminc, train) %>% filter(train == 1)

#data2 <- filter(data, train == 0)
data <- filter(data, train == 1)

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
round(ICC_Empty_RI[1,4]/(sum(ICC_Empty_RI[,4])),3) #0.321

85974 / (1 + ((71 - 1) * .319)) #Design effect using mean #students per school
#Our power to detect level-1 effects will be approximately that of an independent sample of 3,685 students

## 3.2 Empty Means, Random Intercept models for Student-Level predictors ####

#Mother's Educational level
emp_mom1 <- clm(edmom ~ 1, data = empty)
emp_mom2 <- clmm(edmom ~ 1 + (1|RBD), data = empty, nAGQ = 5L, link = "logit")
anova(emp_mom1, emp_mom2)

# 1 / (1 + exp(-1 * -1.634156))
# 1 / (1 + exp(-1 * 0.737291))
# prop.table(table(empty$edmom))

RI_var <- data.frame(VarCorr(emp_mom2))
RI_var / (RI_var + (pi ^ 2 / 3)) #0.3365497

#Father's Educational level
emp_dad1 <- clm(eddad ~ 1, data = empty)
emp_dad2 <- clmm(eddad ~ 1 + (1|RBD), data = empty, nAGQ = 5L, link = "logit")
anova(emp_dad1, emp_dad2)

RI_var <- data.frame(VarCorr(emp_dad2))
RI_var / (RI_var + (pi ^ 2 / 3)) #0.4019054

#Family income
emp_finc1 <- clm(faminc ~ 1, data = empty)
emp_finc2 <- clmm(faminc ~ 1 + (1|RBD), data = empty, link = "logit")
anova(emp_finc1, emp_finc2)

RI_var <- data.frame(VarCorr(emp_finc2))
RI_var / (RI_var + (pi ^ 2 / 3)) #0.3102628

rm(empty, emp_mom1, emp_mom2, emp_dad1, emp_dad2, emp_finc1, emp_finc2, RI_var)
# All level 1 predictors have significant level 2 residual variance.






## 3.3 Adding L1 predictors and their L2 versions (centered at .40), plus RE ####

# Adding Mother education: L1 AND L2
MomEd_L1L2 <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE +
    EdMomHS_BG_40 + EdMomHE_BG_40 + (1|RBD),
  REML = TRUE,
  data = data,
  control = lmerControl(optimizer ="Nelder_Mead"))
summary(MomEd_L1L2, ddf="Satterthwaite"); llikAIC(MomEd_L1L2, chkREML=FALSE)

# Adding Mother education: L1 AND L2 plus RE
MomED_RE <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE +
    EdMomHS_BG_40 + EdMomHE_BG_40 +
    (1 + EdMomHS_CMC + EdMomHE_CMC|RBD),
  REML = TRUE,
  data = data,
  control = lmerControl(optimizer ="Nelder_Mead"))
summary(MomED_RE, ddf="Satterthwaite"); llikAIC(MomED_RE, chkREML=FALSE)

#LRT
DevTest = -2*(logLik(MomEd_L1L2)-logLik(MomED_RE))
RegPvalue = pchisq((DevTest), df=7, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

#Conditional ICC
ICC_cond <- data.frame(VarCorr(MomED_RE),comp=c("Variance"))
ICC_cond <- ICC_cond[c(1:3,7),]
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[4,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$MomED_RE <- predict(MomED_RE, re.form=NA, na.action = na.exclude)
(cor.test(data$MomED_RE, data$MATE_ACTUAL, method="pearson")$estimate)^2 #Total R2 relative to empty model 1
#0.2338347 Total R2

# Adding Dad education: L1 AND L2
DadED_L1L2 <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE + EdDadHS + EdDadHE +
    EdMomHS_BG_40 + EdMomHE_BG_40 + EdDadHS_BG_40 + EdDadHE_BG_40 +
    (1 + EdMomHS_CMC + EdMomHE_CMC|RBD),
  REML = TRUE,
  data = data,
  control = lmerControl(optimizer ="Nelder_Mead"))
summary(DadED_L1L2, ddf="Satterthwaite"); llikAIC(DadED_L1L2, chkREML=FALSE)

# Adding Dad education: L1 AND L2 plus RE
DadED_RE <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE + EdDadHS + EdDadHE +
    EdMomHS_BG_40 + EdMomHE_BG_40 + EdDadHS_BG_40 + EdDadHE_BG_40 +
    (1 + EdMomHS_CMC + EdMomHE_CMC + EdDadHS_CMC + EdDadHE_CMC|RBD),
  REML = TRUE,
  data = data)
summary(DadED_RE, ddf="Satterthwaite"); llikAIC(DadED_RE, chkREML=FALSE)

# LRT
DevTest = -2*(logLik(DadED_L1L2)-logLik(DadED_RE))
RegPvalue = pchisq((DevTest), df=9, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

# Conditional ICC
ICC_cond <- data.frame(VarCorr(DadED_RE),comp=c("Variance"))
ICC_cond <- ICC_cond[c(1:5,16),]
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[6,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$DadED_RE <- predict(DadED_RE, re.form=NA, na.action = na.exclude)
(cor.test(data$DadED_RE, data$MATE_ACTUAL, method="pearson")$estimate)^2 #Total R2 relative to empty model 1
# 0.2515326 

# Adding Family income: L1 and L2
FamInc_L1L2 <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE + EdDadHS + EdDadHE + FamIncome_Q2 +  + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 +
    EdMomHS_BG_40 + EdMomHE_BG_40 + EdDadHS_BG_40 + EdDadHE_BG_40 + FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + EdMomHS_CMC + EdMomHE_CMC + EdDadHS_CMC + EdDadHE_CMC|RBD),
  REML = TRUE,
  data = data)
summary(FamInc_L1L2, ddf="Satterthwaite"); llikAIC(FamInc_L1L2, chkREML=FALSE)

# Adding Family income: L1 and L2 plus RE
FamInc_RE <- lmer(
  formula = MATE_ACTUAL ~ 1 + 
    EdMomHS + EdMomHE + EdDadHS + EdDadHE + FamIncome_Q2 +  + FamIncome_Q3 + FamIncome_Q4 + FamIncome_Q5 +
    EdMomHS_BG_40 + EdMomHE_BG_40 + EdDadHS_BG_40 + EdDadHE_BG_40 + FamIncome_Q2_BG_30 + FamIncome_Q3_BG_30 + FamIncome_Q4_BG_30 + FamIncome_Q5_BG_30 +
    (1 + EdMomHS_CMC + EdMomHE_CMC + EdDadHS_CMC + EdDadHE_CMC +
       FamIncome_Q2_CMC + FamIncome_Q3_CMC + FamIncome_Q4_CMC + FamIncome_Q5_CMC |RBD),
  REML = TRUE,
  data = data)
summary(FamInc_RE, ddf="Satterthwaite"); llikAIC(FamInc_RE, chkREML=FALSE)

# LRT
DevTest = -2*(logLik(FamInc_L1L2) - logLik(FamInc_RE))
RegPvalue = pchisq((DevTest), df=30, lower.tail=FALSE)
MixPvalue = RegPvalue/2
DevTest; RegPvalue; MixPvalue

ICC_cond <- data.frame(VarCorr(FamInc_RE),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3)
(ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4] #PseudoR2 at level 2 relative to empty model
(ICC_Empty_RI[2,4] - ICC_cond[2,4]) / ICC_Empty_RI[2,4] #PseudoR2 at level 1 relative to empty model

data$FamInc_RE <- predict(FamInc_RE, re.form=NA, na.action = na.exclude)
(cor.test(data$FamInc_RE, data$MATE_ACTUAL, method="pearson")$estimate)^2 #Total R2 relative to empty model 1

144431 / (1 + ((60 - 1) * .107)) #Design effect using mean #students per school

# Our conditional model has reduced the design effect because a greater proportion of level-2
# random intercept variance was explained (0.75214) relative to level-1 residual variance (0.0206479).
# This means that our power to detect level-1 effects has improved — it is currently approximately 
# that of an independent sample of 19749.9 students (versus 7997.499 from the empty means model).  

## 3.4 School predictors (level 2) ####
School_RI <- lmer(
  formula = MATE_ACTUAL ~ 1 + TypeHumanist + SectorSubsidized + SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RI, ddf="Satterthwaite"); llikAIC(School_RI, chkREML=FALSE)

School_RI_int <- lmer(
  formula = MATE_ACTUAL ~ 1 + TypeHumanist + SectorSubsidized + SectorPriv +
    TypeHumanist:SectorSubsidized + TypeHumanist:SectorPriv + (1|RBD),
  REML = TRUE,
  data = data)
summary(School_RI_int, ddf="Satterthwaite"); llikAIC(School_RI_int, chkREML=FALSE)

ICC_cond <- data.frame(VarCorr(School_RI_int),comp=c("Variance"))
round(ICC_cond[1,4]/(sum(ICC_cond[,4])),3) #Random intercept variance leftover after controlling for school type and sector
# 0.165
((ICC_Empty_RI[1,4] - ICC_cond[1,4]) / ICC_Empty_RI[1,4]) * 100 #Pseudo R2 level 2
#The three level-2 fixed effects of type and sector explained 58.76% of the school-level intercept variance in math scores

data$School_RI_int <- predict(School_RI_int, re.form=NA, na.action = na.exclude)
(cor.test(data$School_RI_int, data$MATE_ACTUAL, method="pearson")$estimate)^2 #Total R2 relative to empty model 1
# The three fixed effects of type and sector, plus their interaction,
# explained 0.1909587  of the total variance in math scores

# 4. Cross-validation: validation set approach (James, Gareth p. 213)
## End ####




























## 3.6 Pseudo standardized slopes ####
mod_coef <- summary(MLM_RE_EdMom_EdDad_FamIncome)$coef
mod_RE <- data.frame(VarCorr(MLM_RE_EdMom_EdDad_FamIncome))
# Mother's educational level

mod_coef[10] * (sd(data$EdMomHS_BG_40, na.rm = T) / mod_RE[2,4])
mod_coef[11] * (sd(data$EdMomHE_BG_40, na.rm = T) / mod_RE[3,4])

mod_coef[2] * (sd(data$EdMomHS, na.rm = T) / mod_RE[46,4])
mod_coef[3] * (sd(data$EdMomHE, na.rm = T) / mod_RE[46,4])


# Father's educational level

mod_coef[4]
mod_coef[5]

mod_coef[12]
mod_coef[13]

# Family income

mod_coef[6]
mod_coef[7]
mod_coef[8]
mod_coef[9]

mod_coef[14]
mod_coef[15]
mod_coef[16]
mod_coef[17]

## 3.6 Effect size via 95% Random Effect CIs ####
