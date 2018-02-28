rm(list=ls())
# On charge les données
data("heart")
# Clustering en 2 classes
truc=VarSelCluster(heart[,-13], 2)
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
