
##################################
## Import et Preparation script ##
##################################

getwd() #verifier l'emplacement du projet, ici C:/Users/ajleconte/Desktop/almond
#setwd("D:/almond") #permet de modifier l'emplacement de travail
source("packages_use.R") #appel le script contenant tous les packages

test_control <- read.csv2("D:/these/INRAE/EAG/test_reponse_controle2.csv", sep=";") 
attach(test_control) #attention avec attach, bien quand on a juste 1 dataframe pour eviter le $

test_control$File_name <- as.character(test_control$File_name)
test_control$dose_ug <- as.character(test_control$dose_ug)

test_control$VOC <- as.factor(test_control$VOC)
test_control$Insecte <- as.factor(Insecte)


##################################
##     Modelisation data        ##
##################################

test_control$VOC=relevel(as.factor(test_control$VOC), ref="control")

#Modele avec effet aleatoire
reg_control <-lmer(R1S1peak ~ dose_ug+VOC+dose_ug:VOC+(1|Insecte), data = test_control) # car echantillonnage desequilibre
model <- Anova(reg_control)
model

summary(reg_control)
plot(reg_control)


# Verification des residus
shapiro.test(residuals(reg_control)) #0.9 normalite residus

test_control$grp <- interaction(test_control$dose_ug, test_control$VOC, sep="_" ) # Creer une colonne avec interaction dose et cov
bartlett.test(residuals(reg_control)~test_control$grp) #homogeneite des variances, modele ok

res <- test_control %>% tukey_hsd(R1S1peak~grp) # non obligatoire, permet la comparaison deux à deux
res


##################################
##     Comparaison moyenne      ##
##################################

n <- length(levels(test_control$Insecte)) #compte le nombre d'insecte/replicas

moyennes <- aggregate(R1S1peak~VOC+dose_ug, data=test_control, FUN=mean) #realise la moyenne par dose et par cov

names(moyennes)[3] <- "mean" #modifie le nom de colonne 3

etypes=aggregate(R1S1peak~VOC+dose_ug, data=test_control, FUN=sd) #ecart-type par cov et dose
names(etypes)[3] <- "sd" 

df_moyennes=merge(moyennes,etypes,by= c("VOC","dose_ug"))
df_moyennes

df_moyennes$errstd = df_moyennes$sd/sqrt(n) #erreur standard pour les barres d'erreur

df_moyennes$mean <- abs(df_moyennes$mean)


write.csv2(df_moyennes, here::here("D:/these/INRAE/EAG/data_output","df_moyennes.csv"),row.names = FALSE) 

# Representation graphique

plot_mean <- ggplot(data = df_moyennes, aes(x = dose_ug, y = mean, fill = VOC)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Set2") +
  geom_errorbar(aes(ymin = mean - errstd, ymax = mean + errstd), width=.1,  position=position_dodge(0.9)) +
  theme_classic() + scale_x_discrete(name = "Dose (µg)") + scale_y_continuous(name = "Amplitude average (mV)")  + coord_cartesian(ylim = c(0, 2))


plot_f <- plot_mean +
  geom_segment(x=1, xend=2, y=0.83, yend=0.83, col="black") + # trait horizontal
  geom_segment(x=1, xend=1, y=0.83, yend=0.81, col="black") + # trait gauche
  geom_segment(x=2, xend=2, y=0.83, yend=0.81, col="black") + #trait droit
  annotate("text", x=1.5, y=0.87, label = "ns") +
  
  geom_segment(x=1, xend=3, y=1.3, yend=1.3, col="black") + 
  geom_segment(x=1, xend=1, y=1.3, yend=1.28, col="black") + 
  geom_segment(x=3, xend=3, y=1.3, yend=1.28, col="black") + 
  annotate("text", x=2, y=1.32, label = "***") +
  
  geom_segment(x=1, xend=4, y=2, yend=2, col="black") + 
  geom_segment(x=1, xend=1, y=2, yend=1.98, col="black") + 
  geom_segment(x=4, xend=4, y=2, yend=1.98, col="black") + 
  annotate("text", x=2.5, y=2.03, label = "***") +
  
  geom_segment(x=2, xend=3, y=1.1, yend=1.1, col="black") + 
  geom_segment(x=2, xend=2, y=1.1, yend=1.07, col="black") + 
  geom_segment(x=3, xend=3, y=1.1, yend=1.07, col="black") + 
  annotate("text", x=2.5, y=1.13, label = "*") +
  
  geom_segment(x=2, xend=4, y=1.84, yend=1.84, col="black") + 
  geom_segment(x=2, xend=2, y=1.84, yend=1.82, col="black") + 
  geom_segment(x=4, xend=4, y=1.84, yend=1.82, col="black") + 
  annotate("text", x=3, y=1.86, label = "**") +
  
  geom_segment(x=3, xend=4, y=1.72, yend=1.72, col="black") + 
  geom_segment(x=3, xend=3, y=1.72, yend=1.70, col="black") + 
  geom_segment(x=4, xend=4, y=1.72, yend=1.70, col="black") + 
  annotate("text", x=3.5, y=1.76, label = "ns") 

plot_f

