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


library(xlsx)
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
sapply(traits570,function(x) sum(is.na(x)))



# Il manque:
# 34% des valeurs de début/fin de floraison CATMINAT
# 16% couleur de fleur CATMINAT
# 43% qté de nectar BIOLFLOR 
# 2 type de fleur 
# 26 % vecteur de poll LEDA
# 19 % vecteur de poll BIOLFLOR 
# 14 % vecteur de poll CATMINAT
# 20 info entomogame

# On complète les données 

library(readxl)
monDataset <- read_excel("multi_traits.xlsx")

# On rejoute le trait:  type de fleur MUELLER

my_species <-monDataset$Esp
my_traits <- c("flw_muell") # Brot
my_Data <- tr8(species_list=my_species, download_list=my_traits)
# ajouter synonyms=TRUE pour interroger la synonymie, attention pour certaines sp ca fait beuguer la fonction
traits_dataframe <- extract_traits(my_Data) # pour conversion en dataframe

monDataset <- data.frame(monDataset, traits_dataframe$flw_muell)
names(monDataset)[names(monDataset) == 'traits_dataframe.flw_muell'] <- 'flw_muell_BIOLFLORE'

sapply(monDataset, function(x) length(unique(x)))
sapply(monDataset,function(x) sum(is.na(x)))

# Il manque:
# 2% des valeurs de début/fin de floraison CATMINAT
# 2% couleur de fleur CATMINAT
# 2 type de fleur 
# 26 % vecteur de poll LEDA
# 19 % vecteur de poll BIOLFLOR 
# 2 % vecteur de poll CATMINAT
# 4 info entomogame
#50 UV reflexion pattern
# 16% type de fleur MUELLER


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

### Compléter les valeurs manquantes des traits

#Afficher les lignes dont la valeur dans la colonne "colonne" est NA
traits570[is.na(traits570$beg_flow_fr_CATMINAT),]

my_species<-as.character(traits570$Esp)
my_traits<-c("Flower.Color")
my_Data<-tr8(species_list = my_species, download_list = my_traits)
print(my_Data)
Flower_Color_Plants <-as.data.frame.table(extract_traits(my_Data))

### ENREGISTRER FICHIER EXCEL et liste espèces

list_esp<- data.frame(traits570$Esp)
write.table(list_esp,  "list_esp.txt",quote=F, sep = "\t",row.names=F, col.names=F)

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
test_tend_poll <- data.frame(test_tend_poll,info_entomogame=NA,Fam=NA)

colnames(test_tend_poll) <- c("Esp","nbReleves","mean","sd","X2.5.","X97.5.","info_entomogame","Fam")
for (i in 1:nrow(test_tend_poll)){
  for (j in 1:nrow(traits570)){
    if (test_tend_poll$Esp[i]==traits570$Esp[j]){
      test_tend_poll$info_entomogame[i] <- traits570$DepPoll_.InfoEntomogame[j]
      test_tend_poll$Fam[i] <- as.character(traits570$Fam[j])
    }
  }
}

sapply(test_tend_poll,function(x) sum(is.na(x))) #20 NA 
test_tend_poll <- test_tend_poll %>% filter(!is.na(info_entomogame)) #On filtre

ggplot(test_tend_poll,aes(x=as.factor(info_entomogame), y=mean)) +  # Convertir la colonne info_entomogame en facteur 
  geom_boxplot()
#on n'observe pas de tendance particulière
model <- lm(info_entomogame~mean,data=test_tend_poll)
plot(model)
hist(resid(model))
shapiro.test(resid(model)) #Hypothèse non validée
summary(model) 
# p-value: 0.05965, effet non significatif du % d'info entomogame sur la tendance
AIC(model) #AIC= 5544

#En fonction de la famille
ggplot(test_tend_poll,aes(x=mean,fill=Fam))+
  geom_histogram()+
  labs(x="Tendency",y="Count")+
  ggtitle(" Count of species tendency ")
       
# Reflection au UV

test_UVreflection <- data.frame(tendtempo$Esp,tendtempo$mean)
test_UVreflection <- data.frame(test_UVreflection,UV_reflection_pattern=NA)

colnames(test_UVreflection) <- c("Esp","mean_tend","UV_reflection_pattern")

for (i in 1:nrow(tendtempo)){
  for (j in 1:nrow(monDataset)){
    if (tendtempo$Esp[i]==monDataset$Esp[j]){
      test_UVreflection$UV_reflection_pattern[i] <- as.character(monDataset$UV_reflexion_pattern_BIOLFLORE[j])
    }
  }
}

sapply(test_UVreflection,function(x) sum(is.na(x))) #271 NA, c'est beaucoup...

test_UVreflection <- test_UVreflection %>% filter(!is.na(UV_reflection_pattern))

test_UVreflection %>% ggplot(aes(x=UV_reflection_pattern,y=mean_tend))+
  geom_boxplot()+
  labs(x="UV reflection pattern",y="Mean tendency")

 # pas de tendance observable de manière qualitative, à compléter avec test stat
