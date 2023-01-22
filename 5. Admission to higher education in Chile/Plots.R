plotmathsex <- ggplot(data, aes(x=as.factor(INCOME), y = MATH, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(y = "PSU Math", fill = "Income")+
  scale_fill_discrete("Income", breaks = c("0", "1", "2"),
                      labels = c("Low", "Medium", "High"))+
  scale_x_discrete(breaks = c("0", "1", "2" ),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotlensex <- ggplot(data, aes(x=as.factor(INCOME), y = LEN, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "PSU Language", fill = "Income")+
  scale_x_discrete(breaks = c("0", "1", "2" ),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotrkgsex <- ggplot(data, aes(x=as.factor(INCOME), y = RKG, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "PSU Language", fill = "Income")+
  scale_x_discrete(breaks = c("0", "1", "2" ),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotnemsex <- ggplot(data, aes(x=as.factor(INCOME), y = NEM, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "PSU Language", fill = "Income")+
  scale_fill_discrete("Sector", breaks = c("0", "1", "2"),
                      labels = c("Low", "Medium", "High"))+
  scale_x_discrete(breaks = c("0", "1", "2" ),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

pp1 <- (plotrkgsex+plotnemsex)/(plotlensex+plotmathsex)
pp1 + plot_annotation(
  title = "Boxplots for Selected Admission Variables by Gender"
)


















plotmathsec <- ggplot(data, aes(x=as.factor(INCOME), y = MATH, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "PSU Math", fill = "Income")+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotlensec <- ggplot(data, aes(x=as.factor(INCOME), y = LEN, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "Language", fill = "Income")+
  scale_fill_discrete("Income", breaks = c("0", "1", "2"),
                      labels = c("Low", "Medium", "High"))+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Low", "Medium", "High"))+
  theme(axis.title.x=element_blank())


plotrkgsec <- ggplot(data, aes(x=as.factor(INCOME), y = RKG, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "Ranking", fill = "Income")+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotnemsec <- ggplot(data, aes(x=as.factor(INCOME), y = NEM, fill = as.factor(INCOME)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Income", y = "NEM", fill = "Gender")+
  scale_fill_discrete("Income", breaks = c("0", "1", "2"),
                      labels = c("Low", "Medium", "High"))+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Low", "Medium", "High"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

pp2 <- (plotmathsec+plotlensec)/(plotrkgsec+plotnemsec)

pp2 + plot_annotation(
  title = "Boxplots for Selected Admission Variables by Sector"
)











plotmathsec <- ggplot(data=subset(data, !is.na(ED_PARENTS)), aes(x=as.factor(ED_PARENTS), y = MATH, fill = as.factor(ED_PARENTS)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Level", y = "PSU Math", fill = "Level")+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Primary", "Secondary", "Higher"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotlensec <- ggplot(data=subset(data, !is.na(ED_PARENTS)), aes(x=as.factor(ED_PARENTS), y = LEN, fill = as.factor(ED_PARENTS)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Level", y = "Language", fill = "Level")+
  scale_fill_discrete("Level", breaks = c("0", "1", "2"),
                      labels = c("Primary", "Secondary", "Higher"))+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Primary", "Secondary", "Higher"))+
  theme(axis.title.x=element_blank())


plotrkgsec <- ggplot(data=subset(data, !is.na(ED_PARENTS)), aes(x=as.factor(ED_PARENTS), y = RKG, fill = as.factor(ED_PARENTS)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Level", y = "Ranking", fill = "Level")+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Primary", "Secondary", "Higher"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotnemsec <- ggplot(data=subset(data, !is.na(ED_PARENTS)), aes(x=as.factor(ED_PARENTS), y = NEM, fill = as.factor(ED_PARENTS)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Level", y = "NEM", fill = "Level")+
  scale_fill_discrete("Level", breaks = c("0", "1", "2"),
                      labels = c("Primary", "Secondary", "Higher"))+
  scale_x_discrete(breaks = c("0", "1", "2"),
                   labels = c("Primary", "Secondary", "Higher"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

pp2 <- (plotmathsec+plotlensec)/(plotrkgsec+plotnemsec)

pp2 + plot_annotation(
  title = "Boxplots for Selected Admission Variables by Sector"
)










plotmathsec <- ggplot(data, aes(x=as.factor(TYPE), y = MATH, fill = as.factor(TYPE)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Type", y = "PSU Math", fill = "Type")+
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("Humanist", "Technical"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotlensec <- ggplot(data, aes(x=as.factor(TYPE), y = LEN, fill = as.factor(TYPE)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Type", y = "Language", fill = "Type")+
  scale_fill_discrete("Type", breaks = c("0", "1"),
                      labels = c("Humanist", "Technical"))+
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("Humanist", "Technical"))+
  theme(axis.title.x=element_blank())


plotrkgsec <- ggplot(data, aes(x=as.factor(TYPE), y = RKG, fill = as.factor(TYPE)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Type", y = "Ranking", fill = "Type")+
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("Humanist", "Technical"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

plotnemsec <- ggplot(data, aes(x=as.factor(TYPE), y = NEM, fill = as.factor(TYPE)))+
  geom_boxplot()+
  theme_bw()+
  labs(x = "Type", y = "NEM", fill = "Gender")+
  scale_fill_discrete("Type", breaks = c("0", "1"),
                      labels = c("Humanist", "Technical"))+
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("Humanist", "Technical"))+
  scale_fill_discrete(guide = "none")+
  theme(axis.title.x=element_blank())

pp2 <- (plotmathsec+plotlensec)/(plotrkgsec+plotnemsec)

pp2 + plot_annotation(
  title = "Boxplots for Selected Admission Variables by Sector"
)

