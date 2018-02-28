rm(list=ls())
require(VarSelLCM)
# On charge les données
data("heart")
# Clustering en 2 classes
heart[1,1] <- NA
truc=VarSelCluster(heart[,-13], 2)
out <- VarSelCluster(heart[,-13], 1:4)
summary(out)
# Diagramme en cercle pour le pouvoir discriminant
plot(truc, type="pie")
# Diagramme en barre pour le pouvoir discriminant
plot(truc, type="bar")
# Une variable entiere
plot(truc, y="Age", type="boxplot")
plot(truc, y="Age", type="cdf")
# Une variable continue
plot(truc, y="RestBloodPressure", type="boxplot")
plot(truc, y="RestBloodPressure", type="cdf")
# Une variable categorielle
plot(truc, y="Sex")

VarSelShiny(truc)
