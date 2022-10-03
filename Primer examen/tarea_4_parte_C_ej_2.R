library(cluster)
library("lattice")
library(dplyr)
library(vegan)
library(tsne)
# ------------------------------------------------------- READ DATA -------------------------------------------------------

data <- read.csv('OxCGRT_latest.csv', header = TRUE, na.strings = "")

data <- as.data.frame(data)

colnames <- names(data)[7:21]


# ------------------------------------------------------- CLEAN DATA -------------------------------------------------------

cleanData <- data[, c("CountryName", "Date", colnames)]

cleanData <- cleanData[cleanData$Date == 20210101, ]

cleanData<- na.omit(cleanData)

# CHOSE Country Representation

interest <- cleanData[cleanData$CountryName == "United States", ]
usa_rep <- data.frame("United States", 20210101, t(round(apply(interest[,3:17], MARGIN = 2 , FUN = mean))))
colnames(usa_rep) <- c("CountryName", "Date", colnames)

interest <- cleanData[cleanData$CountryName == "United Kingdom", ]
unitedK_rep <- data.frame("United Kingdom", 20210101, t(round(apply(interest[,3:17], MARGIN = 2 , FUN = mean))))
colnames(unitedK_rep) <- c("CountryName", "Date", colnames)

interest <- cleanData[cleanData$CountryName == "Brazil", ]
Brazil_rep  <- data.frame("Brazil", 20210101, t(round(apply(interest[,3:17], MARGIN = 2 , FUN = mean))))
colnames(Brazil_rep) <- c("CountryName", "Date", colnames)



# Remove USA, Brazil, United Kingdom observations
cleanData <- cleanData[cleanData$CountryName != "Brazil", ]
cleanData <- cleanData[cleanData$CountryName != "United States", ]
cleanData <- cleanData[cleanData$CountryName != "United Kingdom", ]

# Add Contry Representation
cleanData <- rbind(cleanData, usa_rep)
cleanData <- rbind(cleanData, Brazil_rep)
cleanData <- rbind(cleanData, unitedK_rep)



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
