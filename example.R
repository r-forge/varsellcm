# Package loading
require(VarSelLCM)

# Data loading:
# x contains the observed variables
# z the known statu (i.e. 1: absence and 2: presence of heart disease)
data(heart)
ztrue <- heart[,"Class"]
x <- heart[,-13]

# Cluster analysis without variable selection
res_without <- VarSelCluster(x, 2, vbleSelec = FALSE)

# Cluster analysis with variable selection (with parallelisation)
res_with <- VarSelCluster(x, 2, nbcores = 2, initModel=40)

# Comparison of the BIC for both models:
# variable selection permits to improve the BIC
BIC(res_without)
BIC(res_with)

# Estimated partition
fitted(res_with)

# Estimated probabilities of classification
head(fitted(res_with, type="probability"))

# Summary of the probabilities of missclassification
plot(res_with, type="probs-class")

# Confusion matrices and ARI (only possible because the "true" partition is known):
# variable selection decreases the misclassification error rate
table(ztrue, fitted(res_without))
table(ztrue, fitted(res_with))
ARI(ztrue,  fitted(res_without))
ARI(ztrue, fitted(res_with))

# Summary of the best model
summary(res_with)

# Discriminative power of the variables (here, the most discriminative variable is MaxHeartRate)
plot(res_with)

# More detailed output
print(res_with)

# Print model parameter
coef(res_with)

# Boxplot for the continuous variable MaxHeartRate
plot(x=res_with, y="MaxHeartRate")

# Empirical and theoretical distributions (to check that the distribution is well-fitted)
plot(res_with, y="MaxHeartRate", type="cdf")

# Summary of categorical variable
plot(res_with, y="Sex")

# Probabilities of classification for new observations 
predict(res_with, newdata = x[1:3,])

# Imputation by posterior mean for the first observation
not.imputed <- x[1,]
imputed <- VarSelImputation(res_with, x[1,], method = "sampling")
rbind(not.imputed, imputed)

# Opening Shiny application to easily see the results
VarSelShiny(res_with)
