rm(list=ls())
require(VarSelLCM)
# On charge les données
data("heart")
# Clustering en 2 classes
heart[1,1] <- NA
truc <- VarSelCluster(heart[,-13], 2)

BIC(truc)

coef(truc)

predict(truc, heart[1:4,-13])

# Diagramme en cercle pour le pouvoir discriminant
plot(truc)
# Diagramme en barre pour le pouvoir discriminant
plot(truc, type="bar")

plot(truc, type="prob-class")
# Une variable entiere
plot(truc, y="Age")
plot(truc, y="Age", type="cdf")
# Une variable continue
plot(truc, y="RestBloodPressure")
plot(truc, y="RestBloodPressure", type="cdf")
# Une variable categorielle
plot(truc, y="Sex")

VarSelShiny(truc)
