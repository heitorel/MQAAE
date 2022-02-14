library(hrbrthemes)
library(gganimate)
library(gapminder)
library(babynames)
library(ggthemes)
library(cowplot)
library(ggplot2)

# Data Manipulation
library(dplyr)

# Statistics
library(DescTools)

# Loading the database 
df <- read.csv("D:/Users/Stefany/Desktop/vgsales - dataset original.csv", stringsAsFactors = FALSE)


#Ver os tipos dos dados
glimpse(df)

#Mudando o tipo de algumas colunas para fator
df$Rank <- as.factor(df$Rank)
df$Year <- as.integer(df$Year)
df$NA_Sales <- as.double(df$NA_Sales)
df$EU_Sales <- as.double(df$EU_Sales)
df$JP_Sales <- as.double(df$JP_Sales)
df$Other_Sales <- as.double(df$Other_Sales)
df$Global_Sales <- as.double(df$Global_Sales)


hist(df$Year,
     main="Histograma Ano",
     col="purple",
     border="violet",
)


df$Platform[df$Platform == "PC"] <- 1 # substitutindo a letra B por A
df$Platform[df$Platform != 1] <- 0

df$Platform <- as.integer(df$Platform)

#Ver os tipos dos dados
glimpse(df)

# qtd de 0s e 1s
table(df$Platform)


# colocando a mesma proporcao de 0s e 1s
# Create Training Data
input_ones <- df[which(df$Platform == 1), ]  # all 1's
input_zeros <- df[which(df$Platform == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 


table(trainingData$Platform)

# Build Logit Models and Predict
logitMod <- glm(Platform ~ Genre + Global_Sales, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores



# Model Diagnostics
summary(logitMod)

exp(logitMod$coefficients)

misClassError(testData$Platform, predicted, threshold = optCutOff)


plotROC(testData$Platform, predicted)

Concordance(testData$Platform, predicted)


confusionMatrix(testData$Platform, predicted, threshold = optCutOff)


predicted.data <- data.frame(
  probability.of.Platform=logitMod$fitted.values,
  Platform=trainingData$Platform)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.Platform, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.Platform)) +
  geom_point(aes(color=Platform), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Probabibilade prevista para ser um PC")



