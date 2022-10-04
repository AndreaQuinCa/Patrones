library(readxl)
library(ggplot2)
library(ggbiplot)
library(cluster)
library(Rankcluster)
library(grid)
library("ape")

library(gridExtra)
library(lattice)

library(tidyverse)
library(class)
library(MASS)
library(caret)
library(ggplot2)
library(kernlab)

# --------------------------------------------------------- FUNCTIONS ------------------------------------------------------------------

" * function to generate a logistic model, it receives the fomula to adjust, the data and trn_ratio
   
  * if trn_ratio = 1, then it builds the model with all the data
"

gen_logit_model <- function(formula, data, trn_ratio=1, iter=1){
  acc <- c()
  res <- c()
  
  for (it in 1:iter){
    # index of train and test data
    index <- seq(1, nrow(data))
    train.index <- sample(nrow(data), trn_ratio * nrow(data))
    test.index <- index[-train.index]
    
    # get trainning and test data
    data.trn <- data[train.index, ]
    data.test <- data[test.index, ]
    
    
    #  BUILD MODEL 
    fit <- glm(formula = formula, 
               data = data.trn,
               family = binomial(link = "logit"))
    
    if (iter==1)
      print(summary(fit))
    
    if (trn_ratio < 1){
      pred <- predict(fit,  data.test,  type="response")
      pred <- as.integer(pred > 0.5)
      acc<- c(acc, mean(pred == data.test$Gender))
    }
    res <- c(res, fit$deviance)
  }
  
  if (trn_ratio < 1){
    print(paste("mean acc ", mean(acc)))
    print(paste("var acc ", var(acc)))
  }
  
  print(paste("mean dev ", mean(res)))
  print(paste("var dev ", var(res)))
  
  return(list(model=fit, acurracy_vector=acc, deviance_vector=res))
}


gen_lda_model <- function(formula, data, trn_ratio=1, iter=1){
  acc <- c()
  res <- c()
  
  for (it in 1:iter){
    # index of train and test data
    index <- seq(1, nrow(data))
    train.index <- sample(nrow(data), trn_ratio * nrow(data))
    test.index <- index[-train.index]
    
    # get trainning and test data
    data.trn <- data[train.index, ]
    data.test <- data[test.index, ]
    
    
    #  BUILD MODEL 
    fit <- lda(formula, data.trn)
    
    if (iter==1){
      print(summary(fit))
      #plot(fit)
    }
    
    if (trn_ratio < 1){
      pred <- predict(fit,  data.test)
      acc<- c(acc, mean(pred$class == data.test$Gender))
    }
  }
  
  if (trn_ratio < 1){
    print(paste("mean acc ", mean(acc)))
    print(paste("var acc ", var(acc)))
  }
  
  return(list(model=fit, acurracy_vector=acc))
}


# --------------------------------------------------------- READ AND CLEAN DATA --------------------------------------------------------- 

# Read 2 blocks of data
data <-as.data.frame(read_excel("sog_agg_country.xlsx", sheet="Data")) 

# Filter data by Male and Female
data <- data[data$Gender == "Male" | data$Gender == "Female", ]

# Rename rows
row.names(data) <- paste(data$Country, data$Gender)

# remove non usefull coumns (year = -1, internet penetration = -4, gender ratio = -12)
data <- data[, c(-1, -4, -12)]

# generate column of gender as int
#data$Gender <- as.integer(data$Gender == 'Female')

" << IMPORTANT >> 

  1. numeric variables are from column 4 to end in data.
  2. variable data.original contains data without being normalized.
  3. variable data will contain normalized data.
  
"

# assess variance in data (remove non numeric columns)
apply(data[, c(-1, -2, -3)], 2, var)


# Normalize data
data.nrm <- data
data.nrm[, c(-1, -2, -3)] <- scale(data.nrm[, c(-1, -2, -3)])


# retrieve data by regions and remove region variable

" LABEL CODE ->

  * NA  : North America
  * EAP : East Asia & Pacific
  * SA  : South Asia
  * LAC : Latin American and Caribbean
  * MENA: Middle East and North Africa
  * ECA : Europe and Central Asia
  * SSA : Sub-Saharan Africa
  
"

data.NA <- data[data$Region == "North America", -1]
data.EAP <- data[data$Region == "East Asia & Pacific", -1]
data.SA <- data[data$Region == "South Asia", ]
data.LAC <- data[data$Region == "Latin America and Caribbean", -1]
data.MENA <- data[data$Region == "Middle East and North Africa", -1]
data.ECA <- data[data$Region == "Europe and Central Asia", -1]
data.SSA <- data[data$Region == "Sub-Saharan Africa", -1]

# data by region normalized
data.NA.nrm <- data.nrm[data.nrm$Region == "North America", -1]
data.EAP.nrm <- data.nrm[data.nrm$Region == "East Asia & Pacific", -1]
data.SA.nrm <- data.nrm[data.nrm$Region == "South Asia", -1]
data.LAC.nrm <- data.nrm[data.nrm$Region == "Latin America and Caribbean", -1]
data.MENA.nrm <- data.nrm[data.nrm$Region == "Middle East and North Africa", -1]
data.ECA.nrm <- data.nrm[data.nrm$Region == "Europe and Central Asia", -1]
data.SSA.nrm <- data.nrm[data.nrm$Region == "Sub-Saharan Africa", -1]


# ---------------------------------------------------------- EXPLORATORY ----------------------------------------------------------
#pairs(data.nrm[, c(-1, -2, -3)])

data.diff <- data[data$Gender=='Male', c(-1, -2, -3)] - data[data$Gender=='Female', c(-1, -2, -3)] 
apply(data.diff[, c(-1, -2, -3)], 2, mean)

# ----------------------------------------------------------- PCA ------------------------------------------------------------------
# Remove Data$Gender to do PCA (column 3) 
data.pca <- prcomp(data.nrm[, c(-1,-2, -3)])
data.proj <- data.pca$x
summary(data.pca)


" * In the next PCA for all regions, we remove Data.<Region>$Gender to do PCA.
  * That is the column(2) in all data.<Region>
"

NA.pca <- prcomp(data.NA.nrm[, c(-1, -2)])
NA.proj <- NA.pca$x
summary(NA.pca)

EAP.pca <- prcomp(data.EAP.nrm[, c(-1, -2)])
EAP.proj <- EAP.pca$x
summary(EAP.pca)

SA.pca <- prcomp(data.SA.nrm[, c(-1, -2)])
SA.proj <- SA.pca$x
summary(SA.pca)

LAC.pca <- prcomp(data.LAC.nrm[, c(-1, -2)])
LAC.proj <- LAC.pca$x
summary(LAC.pca)

MENA.pca <- prcomp(data.MENA.nrm[, c(-1, -2)])
MENA.proj <- MENA.pca$x
summary(MENA.pca)

ECA.pca <- prcomp(data.ECA.nrm[, c(-1, -2)])
ECA.proj <- ECA.pca$x
summary(ECA.pca)

SSA.pca <- prcomp(data.SSA.nrm[, c(-1, -2)])
SSA.proj <- SSA.pca$x
summary(SSA.pca)



# ----------------------------------------------------------- K-MEANS -----------------------------------------------------------

# FIND BEST k
k <- 8
n <- nrow(data.nrm)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.nrm[, c(-1, -2, -3)], var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.nrm[, c(-1, -2, -3)], centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")


# ********************************* Plot with k = 2 ***************************************
"Hopefully we will find that women are in one cluster and men in the other cluster"


# ALL Regions
data.kmeans <- kmeans(data.nrm[, c(-1, -2, -3)], centers = 3)

tmp.tabla <- as.data.frame(cbind(data.proj[, 1], data.proj[, 2], data.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data$Gender, data$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

(ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00", "chocolate"))
  + ggtitle("All Regions")
)

# North America
NA.kmeans <- kmeans(data.NA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(NA.proj[, 1], NA.proj[, 2], NA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.NA$Gender, data.NA$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

NA.plot <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00"))
  + ggtitle("North America")
)

# EAST ASIA

# find best k
k <- 8
n <- nrow(data.EAP.nrm)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.EAP.nrm[, c(-1, -2, -3)], var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.EAP.nrm[, c(-1, -2, -3)], centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")



EAP.kmeans <- kmeans(data.EAP.nrm[, c(-1, -2)], centers = 4)

tmp.tabla <- as.data.frame(cbind(EAP.proj[, 1], EAP.proj[, 2], EAP.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.EAP$Gender, data.EAP$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")


EAP.plot <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluter", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00", "chocolate", "red"))
  + ggtitle("East Asia & Pacific")
)


# South Asia
SA.kmeans <- kmeans(data.SA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(SA.proj[, 1], SA.proj[, 2], SA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.SA$Gender, data.SA$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

SA.plot <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00"))
  + ggtitle("South Asia")
)


# Latin America and Caribbean

# find best k
k <- 8
n <- nrow(data.LAC.nrm)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.LAC.nrm[, c(-1, -2, -3)], var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.LAC.nrm[, c(-1, -2, -3)], centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")



LAC.kmeans <- kmeans(data.LAC.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(LAC.proj[, 1], LAC.proj[, 2], LAC.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.LAC$Gender, data.LAC$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

LAC.plot <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00"))
  + ggtitle("Latin America and Caribbean")
)


# Middle East and North Africa
MENA.kmeans <- kmeans(data.MENA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(MENA.proj[, 1], MENA.proj[, 2], MENA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.MENA$Gender, data.MENA$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

MENA.plot<- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00"))
  + ggtitle("Middle East and North Africa")
)


# EUROPE AND CENTRAL ASIA (best results k=2, 3, 4)


# find best k because 2 clusters leaves men and women mixed in clusters
k <- 8
n <- nrow(data.ECA.nrm)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.ECA.nrm[, c(-1, -2)], var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.ECA.nrm[, c(-1, -2)], centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")

ECA.kmeans <- kmeans(data.ECA.nrm[, c(-1, -2)], centers = 3)

tmp.tabla <- as.data.frame(cbind(ECA.proj[, 1], ECA.proj[, 2], ECA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.ECA$Gender, data.ECA$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

ECA.plot2 <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00", "chocolate"))
  + ggtitle("Europe and Central Asia")
)



# Sub Saharan Africa 
SSA.kmeans <- kmeans(data.SSA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(SSA.proj[, 1], SSA.proj[, 2], SSA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.SSA$Gender, data.SSA$Country)
colnames(tmp.tabla) <- c("pc1", "pc2", "Cluster", "Gender", "Country")

SSA.plot <- (ggplot(tmp.tabla, aes(pc1, pc2, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  + geom_text(aes(label=Country), hjust="middle", vjust="top", size=3)
  + labs(colour="Cluster", shape="Gender")
  + scale_color_manual(values=c("blue", "#E69F00"))
  + ggtitle("Sub-Saharan Africa")
)




# --------------------------------------------------------------- H CLUST (AVOID) --------------------------------------------------------------------

colors <- c("red", "blue")

# Complete Linkage
data.distMat <- daisy(data.LAC.nrm[, c(-1, -2, -3)], metric = "euclidean")
data.hclust <- hclust(data.distMat, method = "median")
plot(data.hclust)

clus4 <- cutree(data.hclust, 2)
plot(as.phylo(data.hclust),  type ="fan", tip.color= colors[clus4], cex=0.8)

h <- heatmap(as.matrix(data.LAC[, c(-1, -2, -3)]), margins = c(8, 3))






# --------------------------------------------------------------------- PREDICTION ---------------------------------------------------------------------
# convert Gender to int
data$Gender <- as.integer(data$Gender == 'Female')

# full model
full.model <- gen_logit_model(as.formula("Gender ~ ."), data[, c(-1, -2)], trn_ratio = 0.8)

full.lda_model <- gen_lda_model(Gender~., data.nrm[,c(-1, -2)], trn_ratio = 0.8, iter=100)



# predictors related to cultural opinion (ACCEPTED)
str_formula <- paste("Gender ~ a5_r + c3_neutral + c3_disagree + c4_housewife_other")
cultural_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio =0.8, iter=100)


# predictors related to financial situation (ACCEPTED)
str_formula <- paste("Gender ~  b2_self + b6_wage + b6_non_wage + b3_fullyindependent + b5_man_other ")

financial_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=1, iter=1)


# predictors related to ALL house care (REJECTED)
str_formula <- paste("Gender ~ c1_care + c1a_all + c1a_occasionally + c1b_spouse  + c1b_nonspouse + c1c_hours_care", 
                     "+ c1d_increase + c1d_same + c1d_decrease + c2_animals + c2_clean + c2_cook + c2_none", 
                     "+ c2_shop  + c2a_no + c2a_yes + c2b_spouse + c2c_hours_chores + c2d_increase + c2d_same",
                     "+ c2d_decrease")

house_care_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8)



# predictors related to Some house care (ACCEPTED)
str_formula <- paste("Gender ~  c2_animals + c2_clean + c2_cook + c2_shop  + c2c_hours_chores")
house_care_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8, iter=100)


# man expected predictors (ACCEPTED)
str_formula <- paste("Gender ~ c2_business + c2_farm + c2_water_fuel + c2_hhmanage")
man_factord.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8, iter=100)






