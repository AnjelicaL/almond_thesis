
##################################
## Import et Preparation script ##
##################################

getwd() #verifier l'emplacement du projet, ici C:/Users/ajleconte/Desktop/almond
setwd("D:/almond") #permet de modifier l'emplacement de travail
source("packages_use.R") #appel le script contenant tous les packages

test_control <- read.csv2("D:/these/INRAE/EAG/test_reponse_controle.csv", sep=";") #ouvre l'emplacement de travail pour selection manuelle fichier
attach(test_control) #attention avec attach, bien quand on a juste 1 dataframe pour eviter le $

test_control$File_name <- as.character(test_control$File_name)
test_control$dose_µg <- as.character(test_control$dose_µg)
test_control$time <- abs((R1S1time - R1S2time)/1000) # conversion temps de reponse en secondes

##################################
##     Normalisation data       ##
##################################

ggqqplot(test_control$R1S1peak)
shapiro.test(test_control$R1S1peak) # 0.0009

# Valeur absolue
#test_control$bas_peak <- abs(test_control$R1S1peak)
#test_control$data_norm <- log10(bas_peak)

# Normalisation par log
test_control$data_norm <-log10(max(R1S1peak+1) - R1S1peak)
shapiro.test(test_control$data_norm) # 0.2904
ggqqplot(test_control$data_norm)

# Test homognéité variance
fligner.test(data_norm ~ VOC, data = test_control) # heterodascticite pour insecte et dose 0.03 et 0.02 mais pas VOC 0.1771

##################################
##     Modelisation data        ##
##################################

#Modele avec effet aleatoire
reg_control <-lmer(data_norm ~ dose_µg+VOC+dose_µg:VOC+(1|Insecte), data = test_control) # car echantillonnage desequilibre
model <- Anova(reg_control)
model2 <- anova(reg_control, type = "III")

# Test de l'interet effet aleatoire : comparaison avec et sans
ranova(reg_control) # 0.007 effet à garder

# Calcul ICC (favoriser anova ou modele mixte)

## Modele avec intercept et facteur aleatoire
reg_control0 <- lme(data_norm ~1, random=~1|Insecte, data = test_control)
VarCorr(reg_control0) #ou regarder le summary(reg_control0)
icc_reg0 <- 0.00181/(0.00181+0.01492) #0.1
VarCorr(reg_control)
icc_regcontrol <- 0.003212/(0.007435+0.003212)


## calcul automatique
#icc_auto <- ICC1.lmer(data_norm, Insecte, test_control)

### OU
reg_control0 <- lmer(data_norm ~ 1 + (1|Insecte), data = test_control)
anova(reg_control0, reg_control)
summary(reg_control0)

#TukeyHSD(reg_control, conf.level = 0.95)
test_control$grp <- interaction(test_control$dose_µg, test_control$VOC, sep="_" ) # Creer une colonne avec interaction dose et cov
#HSD.test(reg_control, "grp")
res <- test_control %>% tukey_hsd(data_norm~grp)
res
##################################
##        Verif residus         ##
##################################
shapiro.test(residuals(reg_control)) #0.4 normalite residus
bartlett.test(residuals(reg_control)~test_control$grp) #homogeneite des variances, modele ok





################################################################################################################################################################################################################

##################################
##     modele lineaire (lm)     ##
##################################

# modele test lm au lieu de lmer
reg_lmfull <- lm(data_norm ~ dose_µg+VOC+dose_µg:VOC, data = test_control)
reg_lm <- lm(data_norm ~ dose_µg+VOC, data = test_control)
anova(reg_lm)
summary(reg_lm)

#modele test sur grp
reg_grp <- lm(data_norm ~ grp, data = test_control)
anova(reg_grp)
summary(reg_grp)


################################################################################################################################################################################################################


##################################
##     Comparaison moyenne      ##
##################################

mean_control <- read.csv2("D:/these/INRAE/EAG/moyenne_testcontrol.csv", sep=";")

mean_control$Dose <- as.character(mean_control$Dose)
mean_control$Moyenne <- abs(mean_control$Moyenne) #mettre en absolu pour un graph plus visuel

ggqqplot(mean_control$Moyenne)
shapiro.test(mean_control$Moyenne) #0.8292, normalite ok


#pvalue a ajouter sur le graph :
#t.test(test_control$data_norm, test_control$dose_µg, paired = T)
#pairwise.t.test(test_control$data_norm, test_control$dose_µg, paired=TRUE) 


res_comp1 <-test_control %>%
  pairwise_t_test(
    data_norm ~ dose_µg, 
    p.adjust.method = "bonferroni"
  )

# Methode 1: Representation graphique avec lettre, pas de comparaison entre cov car lmer nous montre une interaction ns
library("RColorBrewer") #library pour couleur
plot_mean1 <- ggplot(data = mean_control, aes(x = Dose, y = Moyenne, fill = COV)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Set2") +
  geom_errorbar(aes(ymin = Moyenne - Erreur_std, ymax = Moyenne + Erreur_std),width=.1,  position=position_dodge(0.9)) +
  theme_classic() + scale_x_discrete(name = "Dose (µg)") + scale_y_continuous(name = "Amplitude average (mV)") + coord_cartesian(ylim = c(0, 1.8))

plot_final <- plot_mean1 +
  annotate("text", x=0.88, y=1.74, label = "a", fontface="bold", size = 4.5)+
  annotate("text", x=1.75, y=1.74, label = "a", fontface="bold", size = 4.5)+
  annotate("text", x=2.15, y=1.74, label = "a", fontface="bold", size = 4.5)+
  annotate("text", x=2.75, y=1.74, label = "c", fontface="bold", size = 4.5)+
  annotate("text", x=3.15, y=1.74, label = "c", fontface="bold", size = 4.5)+
  annotate("text", x=3.77, y=1.74, label = "c", fontface="bold", size = 4.5) + 
  annotate("text", x=4.2, y=1.74, label = "c", fontface="bold", size = 4.5) 



# Methode 2 : manuelle (peu recommande)

plot_mean2 <- ggplot(data = mean_control, aes(x = Dose, y = Moyenne, fill = COV)) + 
  geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(palette = "Set2") +
  geom_errorbar(aes(ymin = Moyenne - Erreur_std, ymax = Moyenne + Erreur_std),width=.1,  position=position_dodge(0.9)) +
  theme_classic() + scale_x_discrete(name = "Dose (µg)") + scale_y_continuous(name = "Amplitude average (mV)") + coord_cartesian(ylim = c(0, 2))


plot_f <- plot_mean2 +
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
  
