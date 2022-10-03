library("RColorBrewer")  # Hexadecimal color specification
library(geometry)

library(cluster)
library("lattice")
library(dplyr)
library(vegan)
library(tsne)
#--------------------------------Creación Datos--------------------------------#

# Función para construir lunas
runif_in_semicircle <- function(n, radius = 1, center_x, upper=TRUE){
  theta <- runif(n, 0, pi)
  r <- radius
  sign = -1
  if (upper){
    sign = 1
  }
  return(sign*cbind(r*cos(theta)+center_x, r*sin(theta)))
}


# Lunas a data frame
moon_1 <- runif_in_semicircle(30, center_x = .8)
moon_2 <- runif_in_semicircle(30, center_x = .8, upper=FALSE)

x1 <- moon_1[,1]
y1 <- moon_1[,2]
x2 <- moon_2[,1]
y2 <- moon_2[,2]

x <- c(x1,x2)
y <- c(y1,y2)

data <- data.frame(x,y)
names(data) <- c("x", "y")

# Gráfica de datos:

# Colores
col_moon_1 = brewer.pal(n = 8, name = "YlOrRd" )
col_moon_2 = brewer.pal(n = 8, name = "Purples")

# Función para unir gráficas
plot_two <- function(x1, y1, x2, y2, col1 = col_moon_1, col2 = col_moon_2){
  plot(x1,y1,ylim=range(c(y1,y2)),xlim=range(c(x1,x2)),pch=21,  bg=col1)
  par(new=TRUE)
  plot(x2,y2, ylim=range(c(y1,y2)),xlim=range(c(x1,x2)),pch=21,bg=col2)
  
}

# Graficado
plot_two(x1, y1, x2, y2, "red", "blue")


#------------------------------------------Matriz de similitud----------------------------------------

make_kernel_matrix <- function(X, sigma){
  n <- nrow(X)
  k.mat <- matrix(data = 0, nrow = n, ncol=n)
  
  for (i in 1:n){
    for (j in 1:n){
      k.mat[i, j] = exp(- sum((X[i, ] - X[j,])^2)/sigma )
    }
  }
  
  return (k.mat)
}

make_laplace_mat <- function(w){
  
  n <- nrow(w)
  
  laplace.mat <- matrix(data = 0, nrow = n, ncol=n)
  
  row.sums = rowSums(w)
  
  for (i in 1:n){
    
    laplace.mat[i, i] = row.sums[i]
    
  }
  
  laplace.mat = laplace.mat - w
  
  return (laplace.mat)
}


delta <- 2.
kernel.mat <- make_kernel_matrix(data, delta)

# Explorar distancias:

# image plot: asocia colores a la matriz de similitud
levelplot(as.matrix(kernel.mat))

laplace.mat <- make_laplace_mat(kernel.mat)


eigen.pairs <- eigen(laplace.mat)

eigen.vecs <- eigen.pairs$vectors

# Dos eigenvectores menores
e <- 2
y <- eigen.vecs[,(ncol(eigen.vecs)-e+1):ncol(eigen.vecs)]



#--------------------------------------------------- k-means ------------------------------------------------- 
set.seed(2)
y <- as.data.frame(y)

# Observar que las varianzas son algo distintas
sapply(y, var)

# Estandariza las variables 
data_s <- scale(y)
data_s <- as.data.frame(y)

# Revisar que sean var = 1
sapply(data_s, var)


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
#  Se explora k-means
k = 2

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

plot(data, pch = 20,  col = data_color)



