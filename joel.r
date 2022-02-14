library(readxl)
library(psycho)
library(ggplot2)
library(cluster)
library(tidyverse)
library(fpc)
library(cluster)
library(HSAUR)
library(ggmap)
library(rnaturalearth)
library(corrplot)
library(GGally)
library(mctest)
library(rje)

###### Leitura dos dados

DataSet <- read_excel("C:\\Users\\joeld\\Downloads\\DataSetSemReferencias.xlsx")

###### Tratar os dados como numéricos (pois o R lê os missing data como char)
DataSet$GINI = as.numeric(DataSet$GINI)
DataSet$CPI = as.numeric(DataSet$CPI)
DataSet$HI = as.numeric(DataSet$HI)
DataSet$DI = as.numeric(DataSet$DI)
DataSet$IH = as.numeric(DataSet$IH)

###### Verifica a quantidade de missing data de cada coluna
sapply(DataSet, function(x) sum (is.na(x)))
summary(DataSet)
view(DataSet)

###### Vamos ignorar as variáveis qualitativas, salvando algumas em variáveis do R para referência futura
DataSet$`HDI rank` <- NULL 
Countries <- DataSet$Country
DataSet$Country <- NULL
Regions <- DataSet$Region
DataSet$Region <- NULL
DataSet$Subregion <- NULL

####### Aqui vamos limitar o DataSet aos casos completos, onde não há missing data na linha
DataSet = DataSet[complete.cases(DataSet), ]
summary(DataSet)


####### Aqui verifico a correlação entre IH e DI, para responder uma das perguntas feitas
DIxIH = cor(DataSet$IH, DataSet$DI)
ggplot(DIxIH, aes(DataSet$IH, DataSet$DI))

#Cria uma matriz de correlação entre as variáves do DataSet
ggpairs(DataSet)

##### Padronização, para que possamos verificar correlação de Pearson
DataSet = log1p(DataSet)
DataSet = standardize(DataSet)

###### Verifica a correlação numericamente e depois monta a matriz de COrrelação, agora com os dados normalizados (média 0 em todos)
cor(DataSet)
ggpairs(DataSet)
summary(DataSet)


#SDataSet$PD <- log10(DataSet$PD)
#DataSet$GNI <- log10(DataSet$GNI)
#summary(DataSet)
#ggpairs(DataSet)

####### Cria uma matriz de correlação usando o método de Pearson e a apresenta em um 'heatmap'
correlationMatrix <- cor(DataSet, method = "pearson", use = "complete.obs")
corrplot(correlationMatrix, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)


###### Regracao Linear Multipla - todas as variáveis correlacionadas a DI

model <- lm(DI ~ HDI + LEB + EYS + MYS + GNI + PD + IH + CPI + GINI + HI, data = DataSet)

summary(model)
# Regressao usando apenas HDI, pois LEB, EYS MYS e GNI são redundantes a ele

model = lm(DI ~ HDI + CPI + HI + PD + IH + GINI, data = DataSet )
summary(model)
step(model, direction = "both", scale = 0.6034)

ggplot(model,aes(x = DI, y = HDI + CPI + HI + PD + IH + GINI)) + geom_point() + geom_smooth(method = lm, se = FALSE)


#R ###### Regressão Linear usando os dados relevantes segundo o modelo anterior
model = lm(DI ~ CPI + HDI + PD, data = DataSet )
summary(model)
model$fitted.values
model$coefficients
ggplot(model,aes(x = DI, y = HDI + CPI + PD)) + geom_point() + geom_smooth(method = lm, se = FALSE)

####### Verificação dos residuos e teste shapiro para ver a normalidade dos resíduos
resid(model)
plot(model)
shapiro.test(model$residuals)

##### Detecção de multicolinearidade

dataSetWithoutDI <- DataSet[, 1:10]
omcdiag(dataSetWithoutDI,DataSet$DI)
imcdiag(dataSetWithoutDI,DataSet$DI)

###### Removendo valiaveis colineares
# hdi <- DataSet$HDI

dataSetWithoutColinearity <- DataSet[,c(1,6:10)]
# dataSetWithoutColinearity$HDI <- hdi

View(dataSetWithoutColinearity)

#####nova deteccao de colinearidade
dataSetWithoutDI <- DataSet[, 0:9]
omcdiag(dataSetWithoutColinearity,DataSet$DI)
imcdiag(dataSetWithoutColinearity,DataSet$DI)


###### Regracao Linear Multipla - todas as variáveis

powerSet(1:10)

model <- lm(DI ~ HDI + PD + CPI + GINI, data = DataSet)
#model <- lm(DI ~ HDI, data = DataSet)


###### Sccatter plot


pairs(DataSet)

df.reg.subset = df.reg[, c("LEAB", "EYOS", "MYOS", "GNIPC", "ROH")]

# Imprimindo a matriz de dispersao.
pairs(df.reg.subset, cex = 1.5)

model <- lm(DI ~ HDI + LEB + EYS + MYS + GNI + PD + IH + CPI, data = DataSet)

summary(model)



# q <- ggplot(DataSet, aes(x=HDI, color=Region)) + geom_density()
# q