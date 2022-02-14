# bibliotecas
library(hrbrthemes)
library(gganimate)
library(gapminder)
library(babynames)
library(ggthemes)
library(cowplot)
library(ggplot2)
library(readxl)

# import dos dataframes
vgsale <- read_excel("C:\\Users\\heito\\Desktop\\vgsale.xlsm",
                    sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

vgsales <- read_excel("D:\\Graduação\\Métodos Quantitativos Aplicados à Administração de Empresas\\Seminário 1\\vgsales.xlsx",
                      sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

# os datasets foram tratados manualmente
  # vgsales: dataset com apenas o q3 e o q1
  # vgsale: dataset original (amostra de 5k linhas)

# tratamento das variáveis do dataset para o formato adequado nos dois datasets

# q3 e q1
vgsales$Vendas_EU = as.numeric(vgsales$Vendas_EU)
vgsales$Vendas_JP = as.numeric(vgsales$Vendas_JP)
vgsales$Vendas_NA = as.numeric(vgsales$Vendas_NA)
vgsales$Vendas_Globais = as.numeric(vgsales$Vendas_Globais)
vgsales$Booelana = as.numeric(vgsales$Booelana)
vgsales$Ano = as.numeric(vgsales$Ano)

# 5k
vgsale$Vendas_EU = as.numeric(vgsale$Vendas_EU)
vgsale$Vendas_JP = as.numeric(vgsale$Vendas_JP)
vgsale$Vendas_NA = as.numeric(vgsale$Vendas_NA)
vgsale$Vendas_Globais = as.numeric(vgsale$Vendas_Globais)
vgsale$Booelana = as.numeric(vgsale$Booelana)
vgsale$Ano = as.numeric(vgsale$Ano)

table(vgsales$Booelana)

# análise de regressão linear simples da variável dependente booleana
# com as demais variaveis nos dois datasets

# 5k
summary(lm(vgsale$Booelana~vgsale$Platforma))
summary(lm(vgsale$Booelana~vgsale$Ano))
summary(lm(vgsale$Booelana~vgsale$Gênero))
summary(lm(vgsale$Booelana~vgsale$Vendas_NA))
summary(lm(vgsale$Booelana~vgsale$Vendas_EU))
summary(lm(vgsale$Booelana~vgsale$Vendas_JP))
summary(lm(vgsale$Booelana~vgsale$Vendas_Globais))

# q3 e q1
summary(lm(vgsales$Booelana~vgsales$Platforma))
summary(lm(vgsales$Booelana~vgsales$Ano))
summary(lm(vgsales$Booelana~vgsales$Gênero))
summary(lm(vgsales$Booelana~vgsales$Vendas_NA))
summary(lm(vgsales$Booelana~vgsales$Vendas_EU))
summary(lm(vgsales$Booelana~vgsales$Vendas_JP))
summary(lm(vgsales$Booelana~vgsales$Vendas_Globais))

# odds ratio de Ano
exp(coef(lm(vgsale$Booelana~vgsale$Ano)))
exp(coef(lm(vgsales$Booelana~vgsales$Ano)))

# modelos de regressão linear multipla de booleana e conjuntos de
# variáveis quantitativas no dataset tratado
modelovendas<- glm(Booelana~Vendas_JP+Vendas_NA+Vendas_EU
                   ,data=vgsales,family = binomial(link='logit'))
summary(modelovendas)

# odd ratio do modelo
exp(coef(modelovendas))

# predição do resultado do modelo
predicao <- predict(object = modelovendas, newdata = vgsales, type = 'response')

# ordenação dos valores preditos e gráfico 'S'
limiares <- sort(predicao)  
plot(sort(limiares))

# inclusão dos valores no dataset
vgsales<- data.frame(vgsales, Probs = predicao)

# declaração dos vetores do modelo
acuracia <- c()
sensitividade <- c()
especificidade <- c()
booleana <- vgsales$Booelana


# calculo dos valores do vetor
for (i in 1:length(limiares)){

    limiar <- 0.5
    boolean_predita <- ifelse(predicao > limiar, 'bem','ruim')
    
    # matriz confusão
    confusion <- table(Predito = boolean_predita, Original = booleana)
    
    # valores da amtriz
    vp <- confusion[1,1];vp
    fn <- confusion[2,1];fn
    
    vn <- confusion[2,2];vn
    fp <- confusion[1,2];fp
    
    acuracia[i] <- sum(diag(confusion)/sum(confusion)); acuracia  
    
    sensitividade[i] <- vp/(vp+fn)
    
    especificidade[i] <- vn/(vn+fp)
    
}

# plotagem do gráfico
plot(y = sensitividade, x = limiares[1:length(limiares)], type = "l", col = "red")
lines(y = especificidade, x = limiares[1:length(limiares)], type = "l", col = "blue")
legend("bottomleft", c("sensibilidade", "especificidade"),
       col=c("red","blue"), lty =c(1,1),bty=("n"), cex=1, lwd=1)
abline(v=0.26)


