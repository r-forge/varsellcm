rm(list=ls())
# On charge les données
data("heart")
# Clustering en 2 classes
truc=VarSelCluster(heart[,-13], 2)

VarSelShiny(truc)
