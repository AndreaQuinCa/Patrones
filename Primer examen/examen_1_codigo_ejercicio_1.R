library(cluster)
library("lattice")
library(dplyr)
library(vegan)
library(tsne)
library(ggplot2)
library(GGally)
library(car)
library(ggfortify)
# ------------------------------------------------------- READ DATA -------------------------------------------------------

data <- read.csv('ge_data.csv', header = TRUE, na.strings = "")

data <- as.data.frame(data)

colnames <- names(data)[73:83]
colnames

# ------------------------------------------------------- CLEAN DATA -------------------------------------------------------

data.extend <- data[, c("Region", "Country", "Internet_Penetration", "Gender", colnames)]

data.extend <- data.extend[data.extend$Gender == 'Male', ]

data.extend<- na.omit(data.extend)


# Remove Regions
data.extend <- data.extend[data.extend$Country != "Rest of Middle East and North Africa", ]
data.extend <- data.extend[data.extend$Country != "Rest of latin America and Caribbean", ]
data.extend <- data.extend[data.extend$Country != "Rest of East Asia & Pacific", ]
data.extend <- data.extend[data.extend$Country != "Rest of South Asia", ] 
data.extend <- data.extend[data.extend$Country != "Rest of Sub-Saharan Africa", ] 
# Valores numéricos
data.numeric <- data.extend[,5:15]
rownames(data.numeric) = data.extend$Country


#--------------------------------------Primera exploraci?n de datos---------------------------------------------


head(data.numeric)
summary(data.numeric)
#Calculamos media y varianza 
apply(data.numeric,2,mean)
apply(data.numeric,2,var)
#linealidad y outliers
GGally::ggpairs(data.numeric,upper = list(continuous = wrap("cor", size = 6)))
#exploración de outlier
subset(data.numeric, d6_medical > 40)
plot(data.numeric, pch = c(".", "+")[(rownames(data.numeric) == "Japan") + 1], cex = 1.5)

subset(data.numeric, d6_personal > 35) 
plot(data.numeric, pch = c(".", "+")[(rownames(data.numeric) == "Lithuania") + 1], cex = 1.5)

subset(data.numeric, d6_wait > 40) 
plot(data.numeric, pch = c(".", "+")[(rownames(data.numeric) == "Puerto Rico") + 1], cex = 1.5)

# Estandariza los datos

data.std <- as.data.frame(scale(data.numeric))
apply(data.std,2,mean)
apply(data.std,2,var)

#----------------------------------------------------PCA-----------------------------------------------------
# Aplica PCA

pca <- prcomp(data.std, scale=T)

summary(pca) # resumen de importancia de las componentes

pca$rotation # observamos los pesos de las variables originales en cada componente

p1 <- pca2$rotation[,1] # pesos de las variables originales en la i-?sima componente
p2 <- pca2$rotation[,2]

plot(1:11, p1, "l", xlab = "Question", ylab = "PC1", main= "health    isolate    job    medical    migrate   none    other    personal    school    transport    wait")
plot(1:11, p2, "l", xlab = "Question", ylab = "PC2", main= "health    isolate    job    medical    migrate   none    other    personal    school    transport    wait")

# Visualizaci?n del conjunto de datos original con los nuevos ejes
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Gráfica con colores-región
data.color <- c()
for (i in 1:length(data.extend$Region)){
  if(data.extend$Region[i] == 'North America'){
    data.color[i] <- "blue"
  }
  else if(data.extend$Region[i] == 'East Asia & Pacific'){
    data.color[i] <- "darkolivegreen4"
  }
  else if(data.extend$Region[i] == 'South Asia'){
    data_color[i] <- "black"
  }
  else if(data.extend$Region[i] == 'Latin America and Caribbean'){
    data.color[i] <- "chocolate1"
  }
  else if(data.extend$Region[i] == 'Middle East and North Africa'){
    data.color[i] <- "darkgoldenrod"
  }
  else if(data.extend$Region[i] == 'Europe and Central Asia'){
    data.color[i] <- "dodgerblue3"
  }
  else if(data.extend$Region[i] == 'Sub-Saharan Africa'){
    data.color[i] <- "deeppink3"
  }
}


names(data.color) <- rownames(data.std)

proj <- as.data.frame(pca$x)
plot(proj$PC1, proj$PC2, pch = 20)
text(proj$PC1, proj$PC2, labels=cleanData$Country, col = data.color, cex=1.2, font=1)
text(5, 4, "Sub-Saharan Africa", pos = 4, col = "deeppink3", cex = 0.8)
text(5, 1, 'South Asia', pos = 4, col = "black", cex = 0.8)
text(5, 3, 'Middle East and North Africa', pos = 4, col = "darkgoldenrod", cex = 0.8)
text(5, 2.5, 'Europe and Central Asia', pos = 4, col = "dodgerblue3", cex = 0.8)
text(5, 2, 'Latin America and Caribbean', pos = 4, col = "chocolate1", cex = 0.8)
text(5, 1.5, 'North America', pos = 4, col = "blue", cex = 0.8)
text(5, 3.5, 'East Asia & Pacific', pos = 4, col = "darkolivegreen4", cex = 0.8)

# Visualizaci?n del conjunto de datos original con los nuevos ejes



biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))




#--------------------------------------------------- k-means ------------------------------------------------- 
set.seed(2)

# Observamos que variables son binarias
pairs(cleanData[,3:17])

# Observar que las varianzas son algo distintas
sapply(cleanData[,3:17], var)
# Estandariza las variables a través de sus rangos
data_s <- scale(cleanData[,3:17])
data_s <- as.data.frame(data_s)
# Revisar que sean var = 1
sapply(data_s, var)

# Matriz de disimilitud se calcula con gower, para variables categóricas
dissim <- daisy(data_s, metric = "gower")

# Explorar distancias:

# image plot: asocia colores a la matriz de similitud
levelplot(as.matrix(dissim), xlab = " ", ylab = "Country region")

# Gráfica de WGSS para 1:8 grupos
k <- 8
n <- nrow(data_s)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data_s, var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data_s, centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")




# k-means
#  Se explora k-means para k = 3 y 4:
k = 4

data_cluster <- kmeans(data_s, centers = k)$cluster


# Gráfica con colores-clusters
data_color <- c()
for (i in 1:length(data_cluster)){
  if(data_cluster[i] == 1){
    data_color[i] <- "blue"
  }
  else if(data_cluster[i] == 2){
    data_color[i] <- "red"
  }
  else if(data_cluster[i] == 3){
    data_color[i] <- "green"
  }
  else{
    data_color[i] <- "orange"
  }
  
}

names(data_color) <- names(data_cluster)


# perplexity 20
perp = 19
m_tsne <- tsne(data_s,k=2, perplexity = perp)
plot(m_tsne, pch = 20)
text(m_tsne, labels=cleanData$CountryName, col = data_color, cex=0.9, font=1)

data_pca <- prcomp(data_s)
summary(data_pca)
data_pca$rotation
plot(data_pca$x[, 1:2], pch = 20)
text(data_pca$x[, 1:2], labels=cleanData$CountryName, col = data_color, cex=0.9, font=1)
