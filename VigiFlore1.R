library(tidyverse)
#library(vegan)
#library(BiodiversityR)
library(ggplot2)
#library(nlme)
#library(gridExtra)
#library(ade4)
#library(factoextra)
#library(ape)
#library(picante)
#library(car)
library(TR8)
library("writexl")


############## MISE EN FORME DES DONNEES ###############

# EXTRACTION DES DONNEES

tendtempo <- read.delim("TendTempo570EspCommunes.txt", header = TRUE, sep = "\t", dec = ".") 
tendtempo <- tendtempo[-491,] #une ligne en double 
#Connaitre le nombre de valeurs différentes dans chaque colonnes
sapply(tendtempo, function(x) length(unique(x)))
#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(tendtempo,function(x) sum(is.na(x))) # 0  
summary(tendtempo)

polliservice <- read.delim("PollinationService.txt", header = TRUE, sep = "\t", dec = ".") 
sapply(polliservice, function(x) length(unique(x)))
sapply(polliservice,function(x) sum(is.na(x))) # 0  

traits570 <- read.delim("Traits570EspCommunes_Pollinisation.txt", header = TRUE, sep = "\t", dec = ".")
sapply(traits570, function(x) length(unique(x)))
sapply(traits570,function(x) sum(is.na(x))) #Nombreuses valeurs manquantes 

## VERIFICATION DES DONNÉES : si on a bien les mêmes espèces entre les 2 framadate

# tendtempo<-data.frame(tendtempo,verification_esp=NA)
# for (i in 1:nrow(tendtempo)){
#   for (j in 1:nrow(traits570)){
#     if (tendtempo$Esp[i]==traits570$Esp[j]){
#       tendtempo$verification_esp[i]=TRUE 
#     }
#   }
# }
# 
# sum(is.na(tendtempo$verification_esp)) # = 0, on retrouve bien les mêmes espèces 
# tendtempo$verification_esp <- NULL

# ENLEVER LES OUTLIERS

# outlierKD <- function(dt, var) {
#   var_name <- eval(substitute(var),eval(dt))
#   na1 <- sum(is.na(var_name))
#   m1 <- mean(var_name, na.rm = T)
#   par(mfrow=c(2, 2), oma=c(0,0,3,0))
#   boxplot(var_name, main="With outliers")
#   hist(var_name, main="With outliers", xlab=NA, ylab=NA)
#   outlier <- boxplot.stats(var_name)$out
#   mo <- mean(outlier)
#   var_name <- ifelse(var_name %in% outlier, NA, var_name)
#   boxplot(var_name, main="Without outliers")
#   hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
#   title("Outlier Check", outer=TRUE)
#   na2 <- sum(is.na(var_name))
#   cat("Outliers identified:", na2 - na1, "n")
#   cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
#   cat("Mean of the outliers:", round(mo, 2), "n")
#   m2 <- mean(var_name, na.rm = T)
#   cat("Mean without removing outliers:", round(m1, 2), "n")
#   cat("Mean if we remove outliers:", round(m2, 2), "n")
#   response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
#   if(response == "y" | response == "yes"){
#     dt[as.character(substitute(var))] <- invisible(var_name)
#     assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
#     cat("Outliers successfully removed", "n")
#     return(invisible(dt))
#   } else{
#     cat("Nothing changed", "n")
#     return(invisible(var_name))
#   }
# }
# 
# outlierKD(tendtempo, mean)

### Compléter les valeurs manquantes des traits

#Afficher les lignes dont la valeur dans la colonne "colonne" est NA
traits570[is.na(traits570$beg_flow_fr_CATMINAT),]

my_species<-as.character(traits570$Esp)
my_traits<-c("Flower.Color")
my_Data<-tr8(species_list = my_species, download_list = my_traits)
print(my_Data)
Flower_Color_Plants <-as.data.frame.table(extract_traits(my_Data))

### ENREGISTRER FICHIER EXCEL

write_xlsx(traits570,"~/Desktop/DataStageEtienne/traits570.xlsx")

############## ETUDES PRELIMINAIRES ###############

ggplot(tendtempo,aes(x=mean))+
  geom_histogram(color="black",fill="white")+
  labs(x="Tendency",y="Count")+
  ggtitle(" Count of species tendency ")

#shapiro test, p-value inversé, normalité si p-value>0.05
shapiro.test(tendtempo$mean) #non normal: pvalue=8.4e-10

test_qtynec <- data.frame(tendtempo$Esp,tendtempo$mean)
test_qtynec <- data.frame(test_qtynec,nectar_qty=NA)

colnames(test_qtynec) <- c("Esp","mean_tend","nectar_qty")

for (i in 1:nrow(tendtempo)){
   for (j in 1:nrow(traits570)){
     if (tendtempo$Esp[i]==traits570$Esp[j]){
       test_qtynec$nectar_qty[i] <- as.character(traits570$nectar_qty_BIOLFLOR[j])
     }
   }
}

sapply(test_qtynec,function(x) sum(is.na(x))) #243 NA, c'est beaucoup...

test_qtynec <- test_qtynec %>% filter(!is.na(nectar_qty))

test_qtynec %>% ggplot(aes(x=nectar_qty,y=mean_tend))+
  geom_boxplot()+
  labs(x="Nectar Quantity",y="Mean tendency")

test_flow <- data.frame(tendtempo$Esp,tendtempo$mean)
test_flow <- data.frame(test_flow,beg_flow=NA,end_flow=NA)

colnames(test_flow) <- c("Esp","mean_tend","beg_flow","end_flow")
for (i in 1:nrow(test_flow)){
  for (j in 1:nrow(traits570)){
    if (test_flow$Esp[i]==traits570$Esp[j]){
      test_flow$beg_flow[i] <- traits570$beg_flow_fr_CATMINAT[j]
      test_flow$end_flow[i] <- traits570$end_flow_fr_CATMINAT[j]
      
    }
  }
}

sapply(test_flow,function(x) sum(is.na(x))) #196 NA, c'est beaucoup...

test_flow <- test_flow %>% filter(!is.na(beg_flow),!is.na(end_flow))
summary(test_flow)

#Observations graphiques 
test_flow %>% ggplot()+
  geom_point(aes(x=beg_flow,y=mean_tend))+
  labs(x="beg_flow",y="Mean tendency")

#Les dates de début et de fin de floraison prises indép ne semblent pas influencer la tendance des esp., à voir avec les 2 


# TRACER TENDANCE TEMPO EN FONCTION % ENTOMOGAME 


test_tend_poll <- tendtempo
test_tend_poll <- data.frame(test_tend_poll,info_entomogame=NA)

colnames(test_tend_poll) <- c("Esp","nbReleves","mean","sd","X2.5.","X97.5.","info_entomogame")
for (i in 1:nrow(test_tend_poll)){
  for (j in 1:nrow(traits570)){
    if (test_tend_poll$Esp[i]==traits570$Esp[j]){
      test_tend_poll$info_entomogame[i] <- traits570$DepPoll_.InfoEntomogame[j]
    }
  }
}

sapply(test_tend_poll,function(x) sum(is.na(x))) #20 NA 
test_tend_poll <- test_tend_poll %>% filter(!is.na(info_entomogame)) #On filtre

# Convertir la colonne info_entomogame en facteur 

ggplot(test_tend_poll,aes(x=as.factor(info_entomogame), y=mean)) + 
  geom_boxplot()
#on observe pas de tendance particulière
summary(lm(info_entomogame~mean,data=test_tend_poll))
# p-value: 0.05965, effet non significatif du % d'info entomogame sur la tendance
