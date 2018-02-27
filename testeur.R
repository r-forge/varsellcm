rm(list=ls())
# On charge les données
data("heart")
# Clustering en 2 classes
obj=VarSelCluster(heart[,-13], 2)
# Diagramme en cercle pour le pouvoir discriminant
plot(obj, type="pie")
# Diagramme en barre pour le pouvoir discriminant
plot(obj, type="bar")
# Une variable entiere
plot(obj, y="Age", type="boxplot")
plot(obj, y="Age", type="cdf")
# Une variable continue
plot(obj, y="RestBloodPressure", type="boxplot")
plot(obj, y="RestBloodPressure", type="cdf")
# Une variable categorielle
plot(obj, y="Sex")
