
d2011 <- haven::read_sav("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2011/PSU Admisión 2011.sav")
table(d2011$dependencia)
d2011$dependencia <- recode(d2011$dependencia,
       '1' = 1L,
       '2' = 1L,
       '3' = 2L,
       '4' = 3L,
       '5' = 2L)

d2011 %>% group_by(dependencia) %>% summarise(m_mat = mean(matematica[matematica>0], na.rm = T),
                                             m_leng = mean(lenguaje[lenguaje>0], na.rm = T),
                                             m_nem = mean(nem[nem>0], na.rm = T),
                                             n = n())



data <- haven::read_sav("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2012/PSU Admisión 2012.sav")

library(dplyr)
library(ggplot2)
library(tidyr)
round(prop.table(table(data$Dependencia)),3)
scores2012 <- data %>% group_by(Dependencia) %>% summarise(m_mat = mean(matematica_actual, na.rm = T),
                                             m_leng = mean(lenguaje_actual, na.rm = T),
                                             m_prom = mean(promedio_actual[promedio_actual>0], na.rm = T),
                                             m_nem = mean(nem[nem>0], na.rm = T),
                                             n = n())
data$Dependencia <- factor(data$Dependencia)
levels(data$Dependencia)

data %>% drop_na(Dependencia) %>% filter(Dependencia != 2) %>% 
ggplot(., aes(Dependencia, promedio_actual))+
  geom_boxplot(aes(fill = Dependencia))+
  geom_hline(yintercept = 500, color = 'red')+
  annotate("text", x=0.6, y=530, label= "500", color = 'red')+
  scale_fill_discrete(labels=c('Public', 'Private'))+
  labs(x = 'Dependency', y = "Average score - Leng and Maths")+
  scale_x_discrete(labels=c("1" = "Pu", "3" = "Pr"))+
  theme_bw()

data %>% drop_na(Dependencia) %>% filter(Dependencia != 2) %>% 
  ggplot(., aes(Dependencia, nem))+
  geom_boxplot(aes(fill = Dependencia))+
  geom_hline(yintercept = 500, color = 'red')+
  annotate("text", x=1.65, y=480, label= "500", color = 'red')+
  scale_fill_discrete(labels=c('Public', 'Private'))+
  labs(x = 'Dependency', y = "Average score - GPA")+
  scale_x_discrete(labels=c("1" = "Pu", "3" = "Pr"))+
  theme_bw()

d2013 <- haven::read_sav("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2013/PSU Admisión 2013.sav")

d2013 %>% group_by(administracion) %>% summarise(m_mat = mean(MATE[MATE>0], na.rm = T),
                                             m_leng = mean(LENG[LENG>0], na.rm = T),
                                             m_nem = mean(NEM[NEM>0], na.rm = T),
                                             n = n())

d2019 <-readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PSU_2019_PRIV_MRUN.csv")

d2019 %>% group_by(DEPENDENCIA) %>% summarise(m_mat = mean(MATE_ACTUAL[MATE_ACTUAL>0], na.rm = T),
                                                 m_leng = mean(LENG_ACTUAL[LENG_ACTUAL>0], na.rm = T),
                                                 m_nem = mean(PTJE_NEM[PTJE_NEM>0], na.rm = T),
                                                 m_rkg = mean(PTJE_RANKING[PTJE_RANKING>0], na.rm = T))

d2020 <-readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PSU_2020_PUB_MRUN.csv")

d2020 %>% group_by(DEPENDENCIA) %>% summarise(m_mat = mean(MATE_ACTUAL[MATE_ACTUAL>0], na.rm = T),
                                              m_leng = mean(LENG_ACTUAL[LENG_ACTUAL>0], na.rm = T),
                                              m_nem = mean(PTJE_NEM[PTJE_NEM>0], na.rm = T),
                                              m_rkg = mean(PTJE_RANKING[PTJE_RANKING>0], na.rm = T))

d2021 <-readr::read_csv2("C:/Users/geral/OneDrive - University of Iowa/CIDCIE/1. Bases de Datos/3. DEMRE/PSU 2019 - 2021/A_Inscritos_puntajes/A_INSCRITOS_PUNTAJES_PDT_2021_PUB_MRUN.csv")

d2021 %>% group_by(DEPENDENCIA) %>% summarise(m_mat = mean(MATE_ACTUAL[MATE_ACTUAL>0], na.rm = T),
                                              m_leng = mean(CLEC_ACTUAL[CLEC_ACTUAL>0], na.rm = T),
                                              m_nem = mean(PTJE_NEM[PTJE_NEM>0], na.rm = T),
                                              m_rkg = mean(PTJE_RANKING[PTJE_RANKING>0], na.rm = T))

meansPSU <- readr::read_csv("C:/Users/geral/OneDrive - University of Iowa/PhD portfolio/1.-Research/5. Admission to higher education in Chile/meansPSU.csv")
meansPSU <- meansPSU %>%
  gather(Factor, Average_score, Math, Language, GPA, Ranking)
ggplot(meansPSU, aes(as.factor(year), Average_score))+
  geom_point(aes(color = Factor))+
  geom_line(aes(group = Factor, color = Factor))+
  scale_y_continuous(expand = c(0, 0), limits = c(450, 700))+
 facet_grid(. ~ dep)+
  labs(x = 'Year of the admission process', y = "Average score")+
  theme_bw()
