##############################################################################################################
# Exercice 1
##############################################################################################################

rm(list=ls())

# création de l'échantillon
n <- 30
x1 <- cbind(rnorm(n,0,1),rnorm(n,0,1))
x2 <- cbind(rnorm(n,0,1),rnorm(n,10,1))
x3 <- cbind(rnorm(n,10,1),rnorm(n,0,1))

# on centre et on réduit
x <- scale(rbind(x1,x2,x3))
colnames(x)=c("X1","X2")

# création du vecteur d'appartenance aux lois
cl <- c(rep(1,n),rep(2,n),rep(3,n))

par(mfrow=c(1,1))
# représentation du nuage de points
plot(x,col=cl,axes="FALSE")
abline(v=0)
abline(h=0)
title("nuage de points")

# on coupe la fenêtre graphique en 4
par(mfrow=c(2,2))

# projection sur X1
plot(x,col=cl,axes="FALSE")
xnew <- cbind(x[,1],rep(0,3*n))
abline(v=0)
abline(h=0)
points(xnew,col=cl,pch=4)
title("projection sur X1")

# projection sur X2
plot(x,col=cl,axes="FALSE")
xnew <- cbind(rep(0,3*n),x[,2])
abline(v=0)
abline(h=0)
points(xnew,col=cl,pch=4)
title("projection sur X2")

# calcul des vecteurs et des valeurs propres de cov(x)
eigen(cov(x))

# on extrait les deux vecteurs propres
u = eigen(cov(x))$vectors

# calcul des coordonées sur les axes principaux
dim=as.matrix(x)%*%u

# représentation des individus dans le plan principale
plot(dim,col=cl,axes="FALSE",xlab="1ere composante principale",ylab="2nd composante principale")
abline(v=0)
abline(h=0)

#projection sur la 1ere composante
xnew <- cbind(dim[,1],rep(0,3*n))
points(xnew,col=cl,pch=4)
title("projection sur la 1ere composante principale")


# formule de reconstitution
x_reconstitue=dim%*%t(u)
mean((x-x_reconstitue)^2)

# formule de reconstitution selon les axes étudiés
x_comp1=as.matrix(dim[,1])%*%t(u[,1])
x_axe1 <- cbind(x[,1],rep(0,3*n))
x_axe2 <- cbind(rep(0,3*n),x[,2])

[2,]  0.7071068 -0.7071068
# écarts quadratiques moyen entre x et ses reconstitutions
mean((x-x_axe1)^2)
mean((x-x_axe2)^2)
mean((x-x_comp1)^2)

# nuage de points et meilleur reconstitution de x
plot(x,col=cl,axes="FALSE")
abline(v=0)
abline(h=0)
title("nuage de points avec reconstitution")
abline(0,-1,col="blue",lty=2,lwd=2)
points(x_comp1,col=cl,pch=4)



##############################################################################################################
# Exercice 2
##############################################################################################################

rm(list=ls())
library(mvtnorm)
library(FactoMineR)
par(mfrow=c(1,1))
n <- 19
mu <- c(0,0)
sigma <- matrix(c(1,-0.7,-0.7,1),2,2)
x <- rmvnorm(n,mu,sigma)
x <- rbind(x,c(20,20))
colnames(x)=c("x1","x2")
plot(x)


par(mfrow=c(2,2))
res.acp <- PCA(x)
res.acp$ind$contrib

res.acp <- PCA(x,ind.sup=20)
summary(res.acp)




##############################################################################################################
# Exercice 3
##############################################################################################################

rm(list=ls())
library(FactoMineR)
x <- read.table("/home/matthieu/Documents/enseignements/GIS4A/datasets/denrees.txt",sep="\t")
par(mfrow=c(1,2))
res.acp <- PCA(x)

# 
# remarques:
#   Deux composantes principales suffisent pour représenter plus de 88% de l'inertie.
#   Seule la catégorie ouvrier est mal représentée.
#   Le premier axe met en évidence l'opposition qui existe entre cadres supérieurs et agriculteurs.
#   Les autres catégories s'échellonnent le long de cet axe selon la hiérarchie sociale habituelle.
#   Le second axe est caractéristiques des inactifs (ctr 76.5) qui sont opposés à presque toutes les autres catégories.
#   La premiere composante principale mesure donc la répartition de la consommation entre aliments ordinaires bon
#   marché et aliments plus recherchés. L'opposition entre individus le long de l'axe1 reflète donc l'oppposition
#   entre consommations ordinaires et consommations recherchées.
#   La seconde composante principale est liée essentiellement à la consommation de pommes de terre dont une valeur élevée
#   caractérise les inactifs.


##############################################################################################################
# Exercice 5
##############################################################################################################
rm(list=ls())
library(pixmap)
te <- read.pnm("Bureau/Lena_soderberg.ppm")

# affiche l' image
plot(te)

# affiche chaque couleur de base
par(mfrow=c(2,2))
cp=te
cp@red=cp@red*1
cp@green=cp@green*0
cp@blue=cp@blue*0
plot(cp)
cp=te
cp@red=cp@red*0
cp@green=cp@green*1
cp@blue=cp@blue*0
plot(cp)
cp=te
cp@red=cp@red*0
cp@green=cp@green*0
cp@blue=cp@blue*1
plot(cp)

# creation de la matrice ACP
# en ligne les pixels (individus)
# en colonne les couleurs RGB (variables)
data <- cbind(as.vector(te@red),as.vector(te@green),as.vector(te@blue))

# ACP
res.pca=PCA(data,ncp=3,graph=FALSE,scale.unit=FALSE)
summary(res.pca)

rec = reconst(res.pca,ncp=1)
im <- te
im@red <- matrix(rec[,1],nrow(te@red),ncol(te@red))
im@green <- matrix(rec[,2],nrow(te@green),ncol(te@green))
im@blue <- matrix(rec[,3],nrow(te@blue),ncol(te@blue))


im@red[im@red<0]=0
im@green[im@green<0]=0
im@blue[im@blue<0]=0
im@red[im@red>1]=1
im@green[im@green>1]=1
im@blue[im@blue>1]=1
plot(im)




##############################################################################################################
# Exercice 6
##############################################################################################################
rm(list=ls())
library(pixmap)
library(FactoMineR)

# on charge les images
bruite <- list();
for (u in 1:20){
  bruite[[u]] <- read.pnm(paste("Documents/enseignements/GIS4A/datasets/snake/snake_",u,".pgm",sep=""))
}

# creation de la matrice pour l'acp
# en ligne on a les images
# en colonne chaque pixel
data <- matrix(0,20,386*588)
for (k in 1:20){
  data[k,] <- as.vector(bruite[[k]]@grey)
}

# acp
res.pca <- PCA(data,ncp=15,scale.unit=FALSE,graph=FALSE)
summary(res.pca)

# Affichage de l'image 1 bruite
k <- 1
par(mfrow=c(3,2),mar=c(1,1,1,1))
plot(bruite[[k]])
title("Image originale")

# affichage des images reconstituées
for (np in c(1,3,6,8,12)){
  rec = reconst(res.pca,ncp=np)
  debruite <- bruite
  debruite[[k]]@grey  <- as.matrix(rec[k,],386,588)
  debruite[[k]]@grey [which(debruite[[k]]@grey <0)] <- 0
  debruite[[k]]@grey [which(debruite[[k]]@grey >1)] <- 1
  plot(debruite[[k]])
  title(paste("Rec. APC",np,"axes"))
}
