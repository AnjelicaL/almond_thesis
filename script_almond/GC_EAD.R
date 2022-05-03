#install.packages(c("Rwave", "gplots","SciViews"))
library("Rwave")
library("ggplot2")
library("SciViews")
library("gridExtra")

# Chargement data
getwd()
setwd("D:/almond") # choisir l'emplacement de travail
Tab_EAD= read.csv2("D:/these/INRAE/SPME/GC_EAD_22411000.csv",sep=";",header=T)

# Vecteurs

Tab_EAD$Time_min <- Tab_EAD$Time_ms/60000 # conversion temps ms en min
Tab_EAD$Time_sec <- Tab_EAD$Time_ms/1000 # conversion temps ms en sec

Time_min <- Tab_EAD[,"Time_min"] # Vecteur temps
EAG <- Tab_EAD[,"EAGwave"] # Récupérer un vecteur avec les données EAD brutes
FID <- Tab_EAD[, "FIDwave"]
FqEch <- 500 # Hz, soit toutes les 2ms
FqSwaf <- 1 #Hz


#methode vincent
scale_vwt <- FqEch/FqSwaf
w0 <- 2*pi


# Signal EAG
EAGComp<- vwt(EAG, scale_vwt, w0) 
EAGWave<- Mod(EAGComp) 
df_EAG<-data.frame(Time_min,EAGWave)


# Signal FID a mettre en valeur absolue pour saturation
Tab_EAD$FID_abs <- abs(Tab_EAD$FIDwave)
FIDabs <- Tab_EAD[,"FID_abs"]
FIDComp<- vwt(FIDabs, scale_vwt, w0) 
FIDWave<- Mod(FIDComp) 
df_FID<-data.frame(Time_min,FIDWave)


# Visualtisation 
fidplot <- ggplot(df_FID) + geom_line(aes(x = Time_min, y=FIDWave))+ theme_classic() + labs(x = "Time (min)", y = "FID")

eagplot <- ggplot(df_EAG)+ geom_line(aes(x = Time_min, y = EAGWave), color = "red")+ theme_classic()+ labs(y = "EAG (mV)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x = element_blank()) + ylim(0,0.2)

grid.arrange(eagplot, fidplot, nrow = 2, ncol = 1)
