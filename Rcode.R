#Installing necessarry packages

install.packages("ggplot2")
install.packages("hrbrthemes")
install.packages("car")
install.packages("dendextend")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("readxl")

library(ggplot2)
library(hrbrthemes)
library(car)
library(readxl)

#Import dataset: data <- read_excel("data.xlsx")

as.factor(data$Level)
as.factor(data$Phase)

#bi-plot 4 Levels vs 2 Levels

ggplot(data, aes(x=AlFeSi, y=MgO, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))

ggplot(data, aes(x=AlFeSi, y=MgO, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))

#Import dataset omitting sample #9 and #20: data2 <- read_excel("data2.xlsx")

as.factor(data2$Level)
as.factor(data2$Phase)

#bi-plot 4 Levels vs 2 Levels

ggplot(data2, aes(x=AlFeSi, y=MgO, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))
ggplot(data2, aes(x=AlFeSi, y=MgO, color=Level)) + geom_point(size=6) + 
  labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))

ggplot(data2, aes(x=AlFeSi, y=MgO, color=Phase)) + geom_boxplot() + 
  labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))
ggplot(data2, aes(x=AlFeSi, y=MgO, color=Level)) + geom_boxplot() + 
  labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))

#bi-plots according to all major elements vs Al+Fe/Si

#Na2O vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=Na2O, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=Na2O, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=Na2O, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=Na2O, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Na"["2"] ~ "O"))

#P2O5 vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=P2O5, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=AlFeSi, y=P2O5, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=AlFeSi, y=P2O5, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=AlFeSi, y=P2O5, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("P"["2"] ~ "O"["5"]))

#K2O vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=K2O, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=K2O, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=K2O, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=K2O, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("K"["2"] ~ "O"))

#CaO vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=CaO, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=AlFeSi, y=CaO, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=AlFeSi, y=CaO, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=AlFeSi, y=CaO, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("CaO"))

#TiO2 vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=TiO2, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=AlFeSi, y=TiO2, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=AlFeSi, y=TiO2, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=AlFeSi, y=TiO2, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Ti" ~ "O"["2"]))

#MnO vs Al+Fe/Si

ggplot(data2, aes(x=AlFeSi, y=MnO, color=Phase)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=MnO, color=Level)) + geom_point(size=6) + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=MnO, color=Phase)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=AlFeSi, y=MnO, color=Level)) + geom_boxplot() + labs(x = expression("Al"["2"] ~ "O"["3"]~ "+ (Fe"["2"]~ "O"["3"]~ "/SiO"["2"]~ ")"))+ labs(y = expression("Mn" ~ "O"))

#bi-plots according to all major elements vs Si+Al

#Na2O vs Si+Al

ggplot(data2, aes(x=SiAl, y=Na2O, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=Na2O, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=Na2O, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+labs(y = expression("Na"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=Na2O, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+labs(y = expression("Na"["2"] ~ "O"))

#P2O5 vs Si+Al

ggplot(data2, aes(x=SiAl, y=P2O5, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=SiAl, y=P2O5, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=SiAl, y=P2O5, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("P"["2"] ~ "O"["5"]))

ggplot(data2, aes(x=SiAl, y=P2O5, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("P"["2"] ~ "O"["5"]))

#K2O vs Si+Al

ggplot(data2, aes(x=SiAl, y=K2O, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=K2O, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=K2O, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("K"["2"] ~ "O"))

ggplot(data2, aes(x=SiAl, y=K2O, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("K"["2"] ~ "O"))

#CaO vs Si+Al

ggplot(data2, aes(x=SiAl, y=CaO, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=SiAl, y=CaO, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=SiAl, y=CaO, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("CaO"))

ggplot(data2, aes(x=SiAl, y=CaO, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("CaO"))

#TiO2 vs Si+Al

ggplot(data2, aes(x=SiAl, y=TiO2, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=SiAl, y=TiO2, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=SiAl, y=TiO2, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Ti" ~ "O"["2"]))

ggplot(data2, aes(x=SiAl, y=TiO2, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Ti" ~ "O"["2"]))

#MnO vs Si+Al

ggplot(data2, aes(x=SiAl, y=MnO, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=SiAl, y=MnO, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=SiAl, y=MnO, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Mn" ~ "O"))

ggplot(data2, aes(x=SiAl, y=MnO, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("Mn" ~ "O"))

#Mg+Mn+Fe vs Si+Al

ggplot(data2, aes(x=SiAl, y=MgMnFe, color=Phase)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("MgO"+"MnO"+"Fe"["2"]~ "O"["3"]))

ggplot(data2, aes(x=SiAl, y=MgMnFe, color=Level)) + geom_point(size=6) + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("MgO"+"MnO"+"Fe"["2"]~ "O"["3"]))

ggplot(data2, aes(x=SiAl, y=MgMnFe, color=Phase)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("MgO"+"MnO"+"Fe"["2"]~ "O"["3"]))

ggplot(data2, aes(x=SiAl, y=MgMnFe, color=Level)) + geom_boxplot() + labs(x = expression("Si O"["2"]~ "+ Al"["2"]~ "O"["3"]))+ labs(y = expression("MgO"+"MnO"+"Fe"["2"]~ "O"["3"]))

#Descriptives

library(tidyverse)
library(ggpubr)
library(rstatix)

level <- as.factor(data$Level)
phase<- as.factor(data$Phase)

summary(data)
#Calculation of the coefficient of variation

dataset <- data.frame(data$Na2O,	data$MgO,	data$Al2O3,	data$SiO2,	data$P2O5,	data$K2O,	data$CaO,	data$TiO2,	data$MnO,	data$Fe2O3, stringsAsFactors = FALSE)

sapply(dataset, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)


#KW Test
kruskal.test(data$Na2O ~ phase, data = data)
kruskal.test(data$MgO ~ phase, data = data)
kruskal.test(data$Al2O3 ~ phase, data = data)
kruskal.test(data$SiO2 ~ phase, data = data)
kruskal.test(data$P2O5 ~ phase, data = data)
kruskal.test(data$K2O ~ phase, data = data)
kruskal.test(data$CaO ~ phase, data = data)
kruskal.test(data$TiO2 ~ phase, data = data)
kruskal.test(data$MnO ~ phase, data = data)
kruskal.test(data$Fe2O3 ~ phase, data = data)

#MW (Wilcoxon rank-sum) Test
DATASET <- data.frame(data$Na2O,	data$MgO,	data$Al2O3,	data$SiO2,	data$P2O5,	data$K2O,	data$CaO,	data$TiO2,	data$MnO,	data$Fe2O3, data$Level, stringsAsFactors = TRUE)

wilcox.test(data$Na2O ~ level, data = data, exact = FALSE)
wilcox.test(data$Al2O3 ~ level, data = data, exact = FALSE)
wilcox.test(data$SiO2 ~ level, data = data, exact = FALSE)
wilcox.test(data$P2O5 ~ level, data = data, exact = FALSE)
wilcox.test(data$K2O ~ level, data = data, exact = FALSE)
wilcox.test(data$CaO ~ level, data = data, exact = FALSE)
wilcox.test(data$TiO2 ~ level, data = data, exact = FALSE)
wilcox.test(data$MnO ~ level, data = data, exact = FALSE)
wilcox.test(data$Fe2O3 ~ level, data = data, exact = FALSE)

#PCA
install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")

install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")

install.packages("factoextra")
library("factoextra")

numerical_data <- data[,4:13]
head(numerical_data)
corr_matrix <- cor(numerical_data)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]


fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")


# Graph of the samples
res.pca <- prcomp(data[,4:13],  scale = TRUE)
p <- fviz_pca_ind(res.pca, label="none", habillage=data$Phase,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)

fviz_pca_biplot(res.pca, label ="var")

#contribution of the variables
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="blue", mid="yellow",
                        high="red", midpoint=96) +
  theme_minimal()

#Cluster

#Step 1 : Scale all variables to have mean = 0 and standard deviation = 1

clusterdata <- dataset %>% mutate_all(~(scale(.) %>% as.vector))

#Step 2 : Calculating Euclidean Distances

dist_mat <- dist(clusterdata, method = 'euclidean')

#Step 3 : Clustering w/ Average-linkage method

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)