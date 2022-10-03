


# --------------------------------------------------------- FUNCTIONS ------------------------------------------------------------------

" * function to generate a logistic model, it receives the fomula to adjust, the data and trn_ratio
   
  * if trn_ratio = 1, then it builds the model with all the data
"

gen_logit_model <- function(formula, data, trn_ratio=1){
  acc <- c()
  res <- c()
  for (it in 1:100){
    # index of train and test data
    index <- seq(1, nrow(data))
    train.index <- sample(nrow(data), trn_ratio * nrow(data))
    test.index <- index[-train.index]
    
    # get trainning and test data
    data.trn <- data[train.index, ]
    data.test <- data[test.index, ]
    
    
# -------------------------------------------------------------- BUILD MODEL --------------------------------------------------------------
    
    
    fit <- glm(formula = formula, 
               data = data.trn,
               family = binomial(link = "logit"))
    
    
    summary(fit)
    
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



# --------------------------------------------------------- READ AND CLEAN DATA --------------------------------------------------------- 

# Read 2 blocks of data
data <-as.data.frame(read_excel("sog_agg_country.xlsx", sheet="Data")) 

# Filter data by Male and Female
data <- data[data$Gender == "Male" | data$Gender == "Female", ]

# Rename rows
row.names(data) <- paste(data$Country, data$Gender)

# remove non usefull columns
data <- data[, c(-1, -4, -12)]

# transform gender to int label
data$Gender <- as.integer(data$Gender == 'Female')


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

data.diff <- data[data$Gender=="Female", c(-1, -2, -3)] - data[data$Gender=="Male", c(-1, -2, -3)] 
diffs = sort(apply(data.diff[, c(-1, -2, -3)], 2, mean))

# stándar summary
summary(data)

# Frequencies
table.describe <- describe(data, descript = 'Descriptive Statistics')
table.describe
Hmisc::latex(table.describe)  # save description in latex

# Summary around gender
attach(data)
data(data)

# diferencias más fuertes hombres más alto
par(mfrow=c(2,4))
plsmo(b2_self,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b3_fullyindependent,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a5_r,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2b_spouse,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a3_yes,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d4_vehicle,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2_water_fuel,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

mtext("Female", cex=2, col = "purple", adj = 2.2, line = -2.5)
mtext("Male", cex=2, col = "green", adj = 1.9, line = -6)



# diferencias más fuertes mujeres más alto
par(mfrow=c(2,4))
plsmo(c2_clean,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2_cook,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b3_dependent,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a3_no,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b4_disagree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_school,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2d_increase,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

mtext("Female", cex=2, col = "purple", adj = 2.2, line = -2.5)
mtext("Male", cex=2, col = "green", adj = 1.9, line = -6)

# ----------------------------------------------------------- PCA Global -----------------------------------------------------------------
pca <- prcomp(data.nrm[, c(-1,-2, -3)])
summary(pca)  #En las primeras dos c omponentes se explica el .42 de la varianza
pca$rotation # observamos los pesos de las variables originales en cada componente

p1 <- pca$rotation[,1] # pesos de las variables originales en la i-?sima componente
p2 <- pca$rotation[,2]

# colors by type of question
colors_question = c(rep("purple",4), rep("orange", 6-4), 
                    rep("darkolivegreen4", 7-6),  rep("brown", 9-7), 
                    rep("darkolivegreen4", 13-9),  rep("purple", 17-13),
                    rep("darkolivegreen4", 25-17),  rep("dodgerblue3", 51-25),
                    rep("purple", 55-51),  rep("brown", 57-55),
                    rep("darkolivegreen4", 63-57),  rep("purple", 72-63),
                    rep("red", 86-72))

# PC1 plot loadings
plot(1:length(p1), p1, "l", xlab = "Question",  ylab = "PC1 loadings")
text(1:length(p1), p1, labels=names(p1), cex=0.7, font=2, col = colors_question)
text(0, 0.19, "Equality opinions", pos = 4, col = "purple", cex = 1)
text(0, 0.18, 'Non domestic work', pos = 4, col = "orange", cex = 1)
text(0, 0.17, 'Economic-Financial', pos = 4, col = "darkolivegreen4", cex = 1)
text(0, 0.16, 'Demographic', pos = 4, col = "brown", cex = 1)
text(0, 0.15, 'Domestic work', pos = 4, col = "dodgerblue3", cex = 1)
text(0, 0.14, 'COVID', pos = 4, col = "red", cex = 1)

# PC2 plot loadings
plot(1:length(p2), p2, "l", xlab = "Question",  ylab = "PC2 loadings")
text(1:length(p2), p2, labels=names(p2), cex=0.7, font=2, col = colors_question)
text(2, 0.23, "Equality opinions", pos = 4, col = "purple", cex = 1)
text(2, 0.22, 'Non domestic work', pos = 4, col = "orange", cex = 1)
text(2, 0.21, 'Economic-Financial', pos = 4, col = "darkolivegreen4", cex = 1)
text(2, 0.20, 'Demographic', pos = 4, col = "brown", cex = 1)
text(2, 0.19, 'Domestic work', pos = 4, col = "dodgerblue3", cex = 1)
text(2, 0.18, 'COVID', pos = 4, col = "red", cex = 1)

#Projection - color by region

# Gráfica con colores-región
colors_region <- c()
for (i in 1:length(data.nrm$Region)){
  if(data.nrm$Region[i] == 'North America'){
    colors_region[i] <- "blue"
  }
  else if(data.nrm$Region[i] == 'East Asia & Pacific'){
    colors_region[i] <- "darkolivegreen4"
  }
  else if(data.nrm$Region[i] == 'South Asia'){
    colors_region[i] <- "black"
  }
  else if(data.nrm$Region[i] == 'Latin America and Caribbean'){
    colors_region[i] <- "chocolate1"
  }
  else if(data.nrm$Region[i] == 'Middle East and North Africa'){
    colors_region[i] <- "darkgoldenrod"
  }
  else if(data$Region[i] == 'Europe and Central Asia'){
    colors_region[i] <- "dodgerblue3"
  }
  else if(data.nrm$Region[i] == 'Sub-Saharan Africa'){
    colors_region[i] <- "deeppink3"
  }
}


names(colors_region) <- rownames(data.nrm)

proj <- as.data.frame(pca$x)
plot(proj$PC1, proj$PC2, pch = 20)
text(proj$PC1, proj$PC2, labels=data.nrm$Country, col = colors_region, cex=0.9, font=1)
text(-11, 7.5, "Sub-Saharan Africa", pos = 4, col = "deeppink3", cex = 1.2)
text(-11, 7.0, 'South Asia', pos = 4, col = "black", cex = 1.2)
text(-11, 6.5, 'Middle East and North Africa', pos = 4, col = "darkgoldenrod", cex = 1.2)
text(-11, 6.0, 'Europe and Central Asia', pos = 4, col = "dodgerblue3", cex = 1.2)
text(-11, 5.5, 'Latin America and Caribbean', pos = 4, col = "chocolate1", cex = 1.2)
text(-11, 5.0, 'North America', pos = 4, col = "blue", cex = 1.2)
text(-11, 4.5, 'East Asia & Pacific', pos = 4, col = "darkolivegreen4", cex = 1.2)




#------------------------------------------------------------ PCA Regions ----------------------------------------------------------------

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
n <- nrow(data.LAC)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.LAC[, c(-1, -2, -3)], var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.LAC[, c(-1, -2, -3)], centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")


# ********************************* Plot with k = 2 ***************************************
"Hopefully we will find that women are in one cluster and men in the other cluster"


# ALL Regions
data.kmeans <- kmeans(data.nrm[, c(-1, -2, -3)], centers = 2)

tmp.tabla <- as.data.frame(cbind(data.proj[, 1], data.proj[, 2], data.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)

# North America
NA.kmeans <- kmeans(data.NA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(NA.proj[, 1], NA.proj[, 2], NA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.NA$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)

# East Asia
EAP.kmeans <- kmeans(data.EAP.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(EAP.proj[, 1], EAP.proj[, 2], EAP.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.EAP$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")


(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)


# South Asia
SA.kmeans <- kmeans(data.SA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(SA.proj[, 1], SA.proj[, 2], SA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.SA$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)


# Latin America and Caribbean
LAC.kmeans <- kmeans(data.LAC.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(LAC.proj[, 1], LAC.proj[, 2], LAC.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.LAC$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)


# Middle East and North Africa
MENA.kmeans <- kmeans(data.MENA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(MENA.proj[, 1], MENA.proj[, 2], MENA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.MENA$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)


# Europe and Central Asia
ECA.kmeans <- kmeans(data.ECA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(ECA.proj[, 1], ECA.proj[, 2], ECA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.ECA$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
)


# Sub Saharan Africa 
SSA.kmeans <- kmeans(data.SSA.nrm[, c(-1, -2)], centers = 2)

tmp.tabla <- as.data.frame(cbind(SSA.proj[, 1], SSA.proj[, 2], SSA.kmeans$cluster))
tmp.tabla <- cbind(tmp.tabla, data.SSA$Gender)
colnames(tmp.tabla) <- c("x", "y", "Cluster", "Gender")

(ggplot(tmp.tabla, aes(x, y, color = factor(Cluster), shape=factor(Gender))) 
  + geom_point(size=2)
  +labs(colour="Cluster", shape="Gender")
  +scale_color_manual(values=c("blue", "#E69F00"))
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

# full model
full.model <- gen_logit_model(as.formula("Gender ~."), data[, c(-1, -2)], trn_ratio = 0.8)

str_formula <- paste("Gender ~. -a6_r_city -a6_r_noncity -d1 -d4_computer -d4_home",  
                    "-d4_land -d4_none -d4_phone -d6_isolate -d6_job -d6_medical",
                    "-d6_migrate -d6_none -d6_other -d6_personal -d6_school", 
                    "-d6_transport -d6_wait -d7_health -d7_info")

# remove some predictors
non_useful_variables.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8)


# predictors related to cultural opinion
str_formula <- paste("Gender ~ a1_agree + a1_neutral + a1_disagree + a2_opps_other + a5_r + c3_agree", 
                     "+ c3_neutral + c3_disagree + c4_housewife_other")

cultural_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio =0.8)


# predictors related to financial situation
str_formula <- paste("Gender ~ b1_hours_work + b2_self + b3_dependent + b3_fullyindependent + b4_agree + b4_neutral", 
                     "+ b4_disagree + b5_man_other + b6_wage + b6_non_wage + b7_full + b7_limited + b8 + b9")

financial_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8)


# predictors related to house care
str_formula <- paste("Gender ~ c1_care + c1a_all + c1a_occasionally + c1b_spouse  + c1b_nonspouse + c1c_hours_care", 
                     "+ c1d_increase + c1d_same + c1d_decrease + c2_animals + c2_clean + c2_cook + c2_none", 
                     "+ c2_shop  + c2a_no + c2a_yes + c2b_spouse + c2c_hours_chores + c2d_increase + c2d_same",
                     "+ c2d_decrease")

house_care_factors.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8)


# man expected predictors
str_formula <- paste("Gender ~ a3_yes + a3_no + c2_business + c2_farm + c2_water_fuel + c2_hhmanage")
man_factord.model <- gen_logit_model(as.formula(str_formula), data[, c(-1, -2)], trn_ratio=0.8)

#--------------------------------------------- Trees --------------------------------------------------

data <- data[,c(-1, -2)]
# # transform gender to int label
# data$Gender <- c("Male", "Female")[data$Gender+1]
# Parámetros 
vs.prc <- .10
tr.prc <- .8  # porcentaje de entrenamiento para ajustar parámetros
k <- 15  # partición para validación cruzada
csv_file = "stats_gender_tree.csv"

# Partición de datos
data.tr <- sample_frac(data, tr.prc)
data.ts <- setdiff(data, data.tr)

# Para validación cruzada
folds <- createFolds(data$Gender, k=k)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
unlist(lapply(split_up, nrow))

# Para visualizacivón
# data.vs <- sample_frac(data, vs.prc)
#-------------TODO----------
# data.vs <- data.vs[c(1:10, 48:50, 55:58)] 

#------------------------------- Elección de árbol adecuado ----------------------

par(mfrow=c(2,1))  
# Árbol sin restricciones en tamaño y gráfico
tree <- rpart(formula = data.tr$Gender~., data = data.tr, method='class', cp=0.0)
# fancyRpartPlot(tree, main="Árbol completo")

# Gráfica Validación cruzada para seleccionar parámetro alpha/cp
plotcp(tree)

# Información desempeño del árbol
# printcp(tree)

# Poda hasta llegar al árbol más pequeño minimiza el error 
alpha <- tree$cptable[which.min(round(tree$cptable[,"xerror"],2)),"CP"]
ptree <- prune(tree, cp=alpha)

# Gráficos
fancyRpartPlot(ptree, uniform=TRUE, main="Árbol óptimo")

# printcp(ptree) 

#------------------------------- Evalución de Clasificación -----------------------------

# Predicciones de árbol óptimo en conjunto de prueba
preds <- predict(ptree, newdata = data.ts, type = "class")

# Matriz de confusión
my_data1 <- data.frame(data = preds, type = "prediction")
my_data2 <- data.frame(data = data.ts$Gender, type = "real")
names(my_data2) <- names(my_data1)
my_data3 <- rbind(my_data1,my_data2)
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))


# Visualización
par(mfrow=c(1,1))  
y_real <- as.numeric(unlist(my_data3[my_data3$type == "real",1]))
y_preds <- as.numeric(preds)-1

# PCA
pca <- prcomp(data.ts[c(-1)])
summary(pca)
pca$rotation

clus <- as.numeric(data.ts$Gender=='Female')+1
colors <- c("purple", "green")
clus_colors <- colors[clus]
names(clus_colors) <- preds


plot(pca$x[, 1:2], pch = 20, col = clus_colors)
text(pca$x[, 1:2], labels=preds, col = clus_colors, cex=0.9, font=1)

text(-100, 90, "Female", pos = 4, col = "purple", cex = 1.2, font = 2)
text(-100, 80, 'Male', pos = 4, col = "green", cex = 1.2, font = 2)



#-------------------------------- Validación Cruzada Árboles ------------------------

method = 'Decision Tree 1'

cv <- lapply(folds, function(x){
  
  # Split
  training_fold <- data[-x, ]
  test_fold <- data[x, ]
  
  # Classification
  classifier <- rpart(formula = training_fold$Gender~., data = training_fold, method='class', cp=alpha)
  y_pred <- predict(classifier, newdata = test_fold, type = 'class')
  
  # Confusion matrix
  cm <- table(Actual = test_fold$Gender, Predicted = y_pred)
  # print(cm)
  
  # Scores
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision <- diag / colsums 
  accuracy = sum(diag) / n 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  macroF1 = mean(f1)
  return(data.frame(precision, accuracy, recall, macroF1))
})

precision <- lapply(cv, function(x){return(x$precision)})
precision <- mean(as.numeric(unlist(precision)))
accuracy <- lapply(cv, function(x){return(x$accuracy)})
accuracy <- mean(as.numeric(unlist(accuracy)))
recall <- lapply(cv, function(x){return(x$recall)})
recall <- mean(as.numeric(unlist(recall)))
macroF1 <- lapply(cv, function(x){return(x$macroF1)})
macroF1 <- mean(as.numeric(unlist(macroF1)))

df <-  data.frame(method, precision, accuracy, recall, macroF1)
names(df) <- c('Classifier', 'Precision', 'Accuracy', 'Recall', 'macro F1')

df


#--------------------------------------------- Trees --------------------------------------------------

# data <- data[,c(-1, -2)]
# # transform gender to int label
# data$Gender <- c("Male", "Female")[data$Gender+1]
# Parámetros 
vs.prc <- .10
tr.prc <- .8  # porcentaje de entrenamiento para ajustar parámetros
k <- 15  # partición para validación cruzada
csv_file = "stats_gender_tree.csv"

# Partición de datos
data.tr <- sample_frac(data, tr.prc)
data.ts <- setdiff(data, data.tr)

# Para validación cruzada
folds <- createFolds(data$Gender, k=k)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
unlist(lapply(split_up, nrow))

# Para visualizacivón
# data.vs <- sample_frac(data, vs.prc)
#-------------TODO----------
# data.vs <- data.vs[c(1:10, 48:50, 55:58)] 

#------------------------------- Elección de árbol adecuado ----------------------

par(mfrow=c(2,1))  
# Árbol sin restricciones en tamaño y gráfico
tree <- rpart(formula = data.tr$Gender~.-c2_clean-b2_self-a5_r, data = data.tr, method='class', cp=0.0)
# fancyRpartPlot(tree, main="Árbol completo")

# Gráfica Validación cruzada para seleccionar parámetro alpha/cp
plotcp(tree)

# Información desempeño del árbol
# printcp(tree)

# Poda hasta llegar al árbol más pequeño minimiza el error 
alpha <- tree$cptable[which.min(round(tree$cptable[,"xerror"],2)),"CP"]
ptree <- prune(tree, cp=alpha)

# Gráficos
fancyRpartPlot(ptree, uniform=TRUE, main="Árbol óptimo")

# printcp(ptree) 

#------------------------------- Evalución de Clasificación -----------------------------

# Predicciones de árbol óptimo en conjunto de prueba
preds <- predict(ptree, newdata = data.ts, type = "class")

# Matriz de confusión
my_data1 <- data.frame(data = preds, type = "prediction")
my_data2 <- data.frame(data = data.ts$Gender, type = "real")
names(my_data2) <- names(my_data1)
my_data3 <- rbind(my_data1,my_data2)
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))

confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))

#-------------------------------- Validación Cruzada Árboles ------------------------

method = 'Decision Tree 2'

cv <- lapply(folds, function(x){
  
  # Split
  training_fold <- data[-x, ]
  test_fold <- data[x, ]
  
  # Classification
  classifier <- rpart(formula = training_fold$Gender~., data = training_fold, method='class', cp=alpha)
  y_pred <- predict(classifier, newdata = test_fold, type = 'class')
  
  # Confusion matrix
  cm <- table(Actual = test_fold$Gender, Predicted = y_pred)
  # print(cm)
  
  # Scores
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision <- diag / colsums 
  accuracy = sum(diag) / n 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  macroF1 = mean(f1)
  return(data.frame(precision, accuracy, recall, macroF1))
})

precision <- lapply(cv, function(x){return(x$precision)})
precision <- mean(as.numeric(unlist(precision)))
accuracy <- lapply(cv, function(x){return(x$accuracy)})
accuracy <- mean(as.numeric(unlist(accuracy)))
recall <- lapply(cv, function(x){return(x$recall)})
recall <- mean(as.numeric(unlist(recall)))
macroF1 <- lapply(cv, function(x){return(x$macroF1)})
macroF1 <- mean(as.numeric(unlist(macroF1)))

df_new <-  data.frame(method, precision, accuracy, recall, macroF1)
names(df_new) <- c('Classifier', 'Precision', 'Accuracy', 'Recall', 'macro F1')

df <- rbind(df, df_new)

#---------------------------------------Report-----------------------------------------------

# Imprime y guarda comparación numérica
df
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

df$Classifier <- as.character(df$Classifier)
df <- round_df(df, 2)
write.csv(df, csv_file, row.names = FALSE)


#-------------------------------- Validación Cruzada RL---------------

method = 'RL House-chores F'

# transform gender to int label
data$Gender <- as.integer(data$Gender == 'Female')
data <- data[,c(-1, -2)]
# Partición de datos
data.tr <- sample_frac(data, tr.prc)
data.ts <- setdiff(data, data.tr)

# Para validación cruzada
folds <- createFolds(data$Gender, k=k)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
unlist(lapply(split_up, nrow))

cv <- lapply(folds, function(x){
  
  # Split
  training_fold <- data[-x, ]
  test_fold <- data[x, ]
  
  # Classification
  classifier <- glm(Gender~ c2_clean + c2_cook + c2_shop + c2c_hours_chores, family=binomial(link=logit), data=training_fold)
  y_pred <- predict(classifier, newdata = test_fold, type = "response")
  y_pred <- as.integer(y_pred > 0.5)
  
  # Confusion matrix
  cm <- table(Actual = test_fold$Gender, Predicted = y_pred)
  # Scores
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision <- diag / colsums 
  accuracy = sum(diag) / n 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  meanError <- mean(test_fold$type != y_pred) 
  return(data.frame(meanError, macroPrecision, macroRecall, macroF1))
})

macroPrecision <- lapply(cv, function(x){return(x$macroPrecision)})
macroPrecision <- mean(as.numeric(unlist(macroPrecision)))
macroRecall <- lapply(cv, function(x){return(x$macroRecall)})
macroRecall <- mean(as.numeric(unlist(macroRecall)))
macroF1 <- lapply(cv, function(x){return(x$macroF1)})
macroF1 <- mean(as.numeric(unlist(macroF1)))

df_new <-  data.frame(method, macroPrecision, macroRecall, macroF1)
names(df_new) <- c('Classifier', 'macro Precision', 'macro Recall', 'macro F1')
df_r <- df_new


method = 'RL Economics'
cv <- lapply(folds, function(x){
  
  # Split
  training_fold <- data[-x, ]
  test_fold <- data[x, ]
  
  # Classification
  classifier <- glm(Gender~ b2_self + b6_wage + b6_non_wage + b3_fullyindependent + b5_man_other, family=binomial(link=logit), data=training_fold)
  y_pred <- predict(classifier, newdata = test_fold, type = "response")
  y_pred <- as.integer(y_pred > 0.5)
  
  # Confusion matrix
  cm <- table(Actual = test_fold$Gender, Predicted = y_pred)
  # Scores
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision <- diag / colsums 
  accuracy = sum(diag) / n 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  meanError <- mean(test_fold$type != y_pred) 
  return(data.frame(meanError, macroPrecision, macroRecall, macroF1))
})



macroPrecision <- lapply(cv, function(x){return(x$macroPrecision)})
macroPrecision <- mean(as.numeric(unlist(macroPrecision)))
macroRecall <- lapply(cv, function(x){return(x$macroRecall)})
macroRecall <- mean(as.numeric(unlist(macroRecall)))
macroF1 <- lapply(cv, function(x){return(x$macroF1)})
macroF1 <- mean(as.numeric(unlist(macroF1)))


df_new <-  data.frame(method, macroPrecision, macroRecall, macroF1)
names(df_new) <- c('Classifier', 'macro Precision', 'macro Recall', 'macro F1')

df_r <- rbind(df_r, df_new)

# Imprime y guarda comparación numérica

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  return(x)
}

df_r$Classifier <- as.character(df_r$Classifier)
df_r <- round_df(df_r, 2)
write.csv(df_r, "rl.csv", row.names = FALSE)


#-------------------- Descarted
s<-summary(Gender~a3_yes+b1_hours_work+b6_wage+b6_non_wage, data=data)
plot(s, main = "Realizar trabajo no doméstico o doméstico")

s<-summary(Gender~c1_care+c1a_all+c1c_hours_care+c1d_increase+
             c2_animals+c2_business+c2_clean+c2_cook+c2_farm+c2_hhmanage+c2_other+
             c2_shop+c2_water_fuel+c2c_hours_chores+c2d_increase, data=data)
plot(s, main = "Realizar trabajo doméstico")
# creencias
# a 1. cree en equidad? 2. vecinos
# b 1. gastos son responsabilidad del hombre? 2. vecinos
# c 1. el rol de una mujer es cuidar casa y niños? 4. vecinos
par(mfrow=c(3,2)) 
plsmo(a1_agree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a1_disagree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b4_agree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b4_disagree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c3_agree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c3_disagree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

# trabajo no domestico
par(mfrow=c(2,2)) 
plsmo(b6_wage,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b6_non_wage,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b1_hours_work,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a3_yes,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))par(mfrow=c(2,2)) 

plsmo(c2_business,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

# trabajo doméstico
par(mfrow=c(3,2))  
plsmo(c1c_hours_care,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2c_hours_chores,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c1a_all,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c1_care,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2_cook,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(c2_clean,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))


# estado fianciero
par(mfrow=c(3,3))
plsmo(b3_fullyindependent,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b2_self,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a5_r,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

plsmo(d4_home,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d4_land,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d4_vehicle,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

plsmo(b6_wage,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(a3_yes,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b10_no,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))

# decision dinero
par(mfrow=c(1,3))
plsmo(b8,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b9,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b7_full,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))


# precariedad dinero/seguridad
par(mfrow=c(4,3))
plsmo(b7_limited,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d5_agree,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(b10_yes,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_health,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_isolate,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_job,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_medical,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d6_migrate,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d7_food,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d7_loan,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d7_money,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))
plsmo(d7_work,Gender, group=Gender,datadensity=T, col = c("green", "purple"), scat1d.opts = list(lwd=1.5))










