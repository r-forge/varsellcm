##############################################################################################################
# Exercice 1
##############################################################################################################

rm(list=ls())
require(FactoMineR)
# On charge le jeu de données
tabac <- read.table("/home/matthieu/Documents/enseignements/GIS4A/datasets/tabac.txt",sep="\t",header=T)

# La première colonne identifie les adjectifs
row.names(tabac)=tabac[,1]
tabac <- as.matrix(tabac[,-1])

# On effectue le test du chi2
chisq.test(tabac)

# On effectue ACF
res.ca <- CA(tabac)

# Affiche un résumé des résultats
summary(res.ca)


##############################################################################################################
# Exercice 2
##############################################################################################################
rm(list=ls())
require(FactoMineR)
# On charge le jeu de données
data(children)

# On effectue le test du chi2
chisq.test(children[-c(15:18),-c(6:8)])

# On effectue ACF
res.ca <- CA (children, col.sup = 6:8, row.sup = 15:18)

# Affiche un résumé des résultats
summary(res.ca)