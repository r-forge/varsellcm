##############################################################################################################
# Exercice 1
##############################################################################################################

rm(list=ls())
require(FactoMineR)

# importation des données
credit <- read.table("Documents/enseignements/GIS4A/datasets/credit.csv",header=TRUE)

# statistiques descriptives
summary(credit)
# la variable age n'est pas comprise comme qualitative, on la transforme donc
credit[,"Age"] <- factor(credit[,"Age"])
summary(credit)

# affiche chaque variable afin de vérifier qu'il n'y ait pas de modalités rares.
par(mfrow=c(4,3),mar=c(2,4,2,2))
for (i in 1:ncol(credit)){
  plot(credit[,i])
}

# Ici seul un individu prend la modalité sideècar pour la variable Marche. On la regroupe alors dans la modalité Moto
# Il faudra bien indiquer ce "regroupement de modalité!"
levels(credit[,"Marche"])[5] <- "Moto"
levels(credit[,"Marche"])[2] <- "Moto_et_side-car"


# pour déterminer les profils de comportements bancaires, nous allons mettre en actif les variables correspondant aux
# informations bancaires (les 5 premieres). Le choix des variables actives est très important car elles seules participent
# à la construction des axes de l'ACM, autrement dit, seules ces variables sont utilisées pour calculer les distances entre
# les individus. Les autres variables sont mises en suppléementaires.

res.mca <- MCA(credit, quali.sup=6:11,graph=FALSE)

# on calcule la somme du nombre des modalités -1 pour savoir combien d'axes retenir
critere <- 0
for (u in 1:5){
  critere <- critere + length(levels(credit[,u]))-1
}

nb_axes <- 1/5
# Le nombre d'axes à conserver est donc:
sum(res.mca$eig[,1]>=nb_axes)

par(mfrow=c(2,2))
plot(res.mca,invisible=c("var","quali.sup"))
plot(res.mca,invisible=c("var","quali.sup"),habillage="Marche")
plot(res.mca,invisible="ind")
plot(res.mca,choix="var")


##############################################################################################################
# Exercice 2
##############################################################################################################

rm(list=ls())
require(FactoMineR)

chiens <- read.table("Documents/enseignements/GIS4A/datasets/chiens.txt",sep="\t",header=TRUE,row.names=1)


# On recode les modalités
chiens[,"Taille"] <- factor(chiens[,"Taille"])
levels(chiens[,"Taille"]) <- c("petite_taille","taille_moyenne","grande_taille")


chiens[,"Poids"] <- factor(chiens[,"Poids"])
levels(chiens[,"Poids"]) <- c("petite_poids","poids_moyenne","poids_eleve")


chiens[,"Veloc."] <- factor(chiens[,"Veloc."])
levels(chiens[,"Veloc."]) <- c("lent","assez_rapide","tres_rapide")


chiens[,"Intell."] <- factor(chiens[,"Intell."])
levels(chiens[,"Intell."]) <- c("peu_intelligent","intelligence_moyenne","tres_intelligent")


chiens[,"Affect."] <- factor(chiens[,"Affect."])
levels(chiens[,"Affect."]) <- c("peu_affectueux","affectueux")


chiens[,"Agress."] <- factor(chiens[,"Agress."])
levels(chiens[,"Agress."]) <- c("peu_agressif","agressif")


chiens[,"Fonct."] <- factor(chiens[,"Fonct."])
levels(chiens[,"Fonct."]) <- c("chien_de_compagnie","chien_de_chasse","utilite")

# ACM sur Burt
res.burt <- MCA(chiens,quali.sup=6,method="Burt")

# ACM sur disjonctif complet
res.acm <- MCA(chiens,quali.sup=6)
res.acm$eig