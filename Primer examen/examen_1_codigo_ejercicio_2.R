library(cluster)
library(graphics)

# seleccionar columnas
library(dplyr)

library("MVA")
library("ape")
library(Rankcluster)


library(hclust)

library("lattice")


#----------------------------------- READ DATA -------------------------------------------------------

data <- read.csv('food.csv', header = TRUE, na.strings = "", sep = " ", quote = '"')

data <- as.data.frame(data)

colnames <- names(data)[9:14]

colnames
# ------------------------------------------------------- CLEAN DATA -------------------------------------------------------

extended.data <- data[, c("cluster.id", colnames)]

extended.data<- na.omit(extended.data)


# ---------------------------------------------------------- HCLUST ---------------------------------------------------------- 


# Estandarizar datos
numeric.data <- extended.data[,2:7]
names(numeric.data) <- c("fat", "calories", "carbohydrates", "protein", "cholesterol", "saturated.fat")

data.std <- scale(numeric.data)

# Heatmap
heatmap(as.matrix(data.std))

# Quitar observación alejada
data.std <- data.std[-c(866),]
m.inv <- function(x) -x
data.std <- as.data.frame(data.std)

cols_inv <- select(data.std, calories)

cols_inv <- apply(cols_inv, 2, m.inv)

data.ok <- select(data.std, "fat", "carbohydrates", "protein", "cholesterol", "saturated.fat")

data.df <- cbind(data.ok, cols_inv)

data.df <- as.data.frame(scale(data.df))

heatmap(as.matrix(data.df))



disMat <- daisy(data.df)

# Complete Linkage
colors <- c("cornflowerblue", "blueviolet", "chartreuse3", "orange", "cyan", "purple",
            "brown2", "deepskyblue3", "aquamarine3", "blue4", "coral1", "goldenrod", 
            "red", "blue", "green 3", "skyblue", "black")

cluster <- hclust(disMat,  method = "average")
plot(cluster)
clus4 <- cutree(cluster, 15)
clus4[90] != clus4[471]
plot(as.phylo(cluster),  type ="fan", tip.color= colors[clus4[1:960]], label.offset = 1, cex=0.7)


#--------------------------------------------------- k-means ------------------------------------------------- 

#linealidad y outliers
library(tsne)
library(ggplot2)
library(GGally)
library(car)
library(ggfortify)
GGally::ggpairs(data.df,upper = list(continuous = wrap("cor", size = 6)))



# Matriz de disimilitud se calcula con gower, para variables categóricas
dissim <- daisy(data.df)

# Explorar distancias:

# image plot: asocia colores a la matriz de similitud
levelplot(as.matrix(dissim), xlab = "Info Nutricional", ylab = "Alimento" )

# Gráfica de WGSS vs k
k <- 20
n <- nrow(data.df)
wss <- rep(0, k)
wss[1] <- (n - 1) * sum(sapply(data.df, var))

for (i in 2:k){
  wss[i] <- sum(kmeans(data.df, centers = i)$withinss)
}

plot(1:k, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")


#  Se explora k-means para k = 12:
k = 12

# PCA

pca <- prcomp(data.df)
summary(pca)
pca$rotation

p1 <- pca$rotation[,1] # pesos de las variables originales en la i-?sima componente
p2 <- pca$rotation[,2]

plot(1:6, p1, "l", xlab = "Info. Nutricional", ylab = "PC1", main= "fat    calories    carbohydrates    protein    cholesterol   saturated.fat    calories")
plot(1:6, p2, "l", xlab = "Info. Nutricional", ylab = "PC2", main= "fat    calories    carbohydrates    protein    cholesterol   saturated.fat    calories")


# k-means
set.seed(2)
clus <- kmeans(data.df, centers = k)$cluster

clus_colors <- colors[clus[1:960]]
names(clus_colors) <- names(clus)

plot(pca$x[, 1:2], pch = 20)
text(pca$x[, 1:2], labels=rownames(pca$x[, 1:2]), col = clus_colors, cex=0.9, font=1)
