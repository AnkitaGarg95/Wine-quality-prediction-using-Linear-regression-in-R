#loading the library tidyverse
library(tidyverse)

#reading the datasets
red_wine_data <- read_delim('winequality-red.csv',delim = ";")
white_wine_data <- read_delim('winequality-white.csv', delim = ";")

#adding a wine_color column
red_wine_data <- add_column(red_wine_data, wine_color = 0)
white_wine_data <-add_column(white_wine_data, wine_color = 1)


# checking the column names
colnames(red_wine_data)
colnames(white_wine_data)

# reading the summary for each wine data
summary(red_wine_data)
summary(white_wine_data)

# understanding each red and white wine data
str(red_wine_data)
str(white_wine_data)

# so we have 1599 observations for red_wine data and 4898 obseravations for white_wine data

# lets add these both red and white wine tibbles together
wine_data <- rbind(red_wine_data,white_wine_data)
nrow(wine_data)

# removing dublicate va;ues from data
wine_data <- unique( wine_data )
wine_data
nrow(wine_data)
sum(is.na(wine_data))
colnames(wine_data)


# understanding the new wine_data so formed
str(wine_data)
summary(wine_data)
sapply(wine_data, sd)

#understanding distribution for wine quality
summary(wine_data)
str(wine_data$quality)

wine_data_p <- as.factor(wine_data$quality)
str(wine_data_p)
summary(wine_data_p)

plot(wine_data_p)
ggplot(wine_data, aes(x=factor(quality),fill = factor(quality)))+
  geom_bar()+
  theme_minimal()


#Lets check for mullticollinearity
library(GGally)
ggpairs(wine_data)


# lets see the all models that can be formed based on 11 features (no wine_color)
library(olsrr)
quality_model <-lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, data = wine_data)
all_model_data <- ols_step_all_possible(quality_model)
str(all_model_data)
view(all_model_data)


# lets view these values for outliers
library(dplyr)
plot1 <- ggplot(data = filter.wine.data, aes(x = quality, y = `volatile acidity` )) + geom_point()
plot1
plot2 <- ggplot(data = filter.wine.data, aes(x = quality, y = alcohol )) + geom_point()
plot2
plot3 <- ggplot(data = filter.wine.data, aes(x = quality, y = density )) + geom_point()
plot3
plot4 <- ggplot(data = filter.wine.data, aes (x = quality, y = `residual sugar`)) +geom_point()
plot4
plot5 <- ggplot(data = filter.wine.data, aes (x = quality, y = sulphates)) +geom_point()
plot5

# lets see correlation of all parameters with wine quality
cor.test(wine_data$`volatile acidity`, wine_data$quality, method = 'kendall')
cor.test(wine_data$`fixed acidity`, wine_data$quality, method = 'kendall' )
cor.test(wine_data$`citric acid`, wine_data$quality, method = 'kendall' )
cor.test(wine_data$`residual sugar`, wine_data$quality, method = 'kendall' )
cor.test(wine_data$chlorides, wine_data$quality, method = 'kendall' )
cor.test(wine_data$`free sulfur dioxide`, wine_data$quality, method = 'kendall' )
cor.test(wine_data$`total sulfur dioxide`, wine_data$quality, method = 'kendall' )
cor.test(wine_data$density, wine_data$quality, method = 'kendall' )
cor.test(wine_data$pH, wine_data$quality, method = 'kendall' )
cor.test(wine_data$sulphates, wine_data$quality, method = 'kendall' )
cor.test(wine_data$alcohol, wine_data$quality, method = 'kendall' )


# fitting base linear regression model and checking assumptions
Wine_model_1 <- lm(quality ~ `volatile acidity` + alcohol + density +`residual sugar`, data = wine_data )
vif(Wine_model_1)
mean(vif(Wine_model_1))
1/vif(Wine_model_1)
plot(Wine_model_1)
a = durbinWatsonTest(Wine_model_1)
a
summary(Wine_model_1)
confint(Wine_model_1)

# fitting base linear regression model without density and checking assumptions
Wine_model_2 <- lm(quality ~ `volatile acidity` + alcohol +`residual sugar`, data = wine_data )
vif(Wine_model_2)
mean(vif(Wine_model_2))
1/vif(Wine_model_2)
plot(Wine_model_2)
a = durbinWatsonTest(Wine_model_2)
a
summary(Wine_model_2)
confint(Wine_model_2)

# fitting base linear regression model without residual sugar and checking assumptions
Wine_model_3 <- lm(quality ~ `volatile acidity` + alcohol + density, data = wine_data )
vif(Wine_model_3)
mean(vif(Wine_model_3))
1/vif(Wine_model_3)
plot(Wine_model_3)
a = durbinWatsonTest(Wine_model_3)
a
summary(Wine_model_3)
confint(Wine_model_3)


# fitting 4 variable linear regression model with sulphates residual sugar and checking assumptions
Wine_model_4 <- lm(quality ~ `volatile acidity` + alcohol + `residual sugar`+  sulphates, data = wine_data )
vif(Wine_model_4)
mean(vif(Wine_model_4))
1/vif(Wine_model_4)
plot(Wine_model_4)
a = durbinWatsonTest(Wine_model_4)
a
summary(Wine_model_4)
confint(Wine_model_4)


# fitting 5 variable linear regression model with total sulpher di oxide and checking assumptions
Wine_model_5 <- lm(quality ~ `volatile acidity` + alcohol + `residual sugar`+  sulphates + `total sulfur dioxide`, data = wine_data )
vif(Wine_model_5)
mean(vif(Wine_model_5))
1/vif(Wine_model_5)
plot(Wine_model_5)
a = durbinWatsonTest(Wine_model_5)
a
summary(Wine_model_5)
confint(Wine_model_5)

# performing anova test to ensure the significant difference among various regression models built
anova(Wine_model_2, Wine_model_4)
anova(Wine_model_3, Wine_model_4)
anova(Wine_model_4, Wine_model_5)
anova(Wine_model_5, quality_model)


# shuffling the dataset to get improved DW statistic
set.seed(42)
rows <- sample(nrow(wine_data))
wine_data_s <- wine_data[rows, ]
Wine_model_4s <- lm(quality ~ `volatile acidity` + alcohol + `residual sugar`+  sulphates, data = wine_data_s )
vif(Wine_model_4s)
mean(vif(Wine_model_4s))
1/vif(Wine_model_4s)
plot(Wine_model_4s)
a = durbinWatsonTest(Wine_model_4s)
a
summary(Wine_model_4)
confint(Wine_model_4)


# checking cooks distance for model 4
wine_data_s$cooks <- cooks.distance(Wine_model_4s)
plot(sort(wine_data_s$cooks, decreasing=TRUE))
(max(wine_data_s$cooks))

colnames(wine_data_s)



boxplot(wine_data_f$`residual sugar`)
boxplot (wine_data_f$`volatile acidity`)
boxplot(wine_data_f$sulphates)
boxplot(wine_data_f$density)
boxplot(wine_data_f$alcohol)

str(wine_data_f)

summary(wine_data_s)
Wine_model_1 <- lm(quality ~ `volatile acidity` + alcohol + density +`residual sugar`, data = wine_data_f )
vif(Wine_model_1)
mean(vif(Wine_model_1))
1/vif(Wine_model_1)
plot(Wine_model_1)
a = durbinWatsonTest(Wine_model_1)
a
summary(Wine_model_1)
confint(Wine_model_1)

Wine_model_2 <- lm(quality ~ `volatile acidity` + alcohol +`residual sugar`, data = wine_data_f )
vif(Wine_model_2)
mean(vif(Wine_model_2))
1/vif(Wine_model_2)
plot(Wine_model_2)
a = durbinWatsonTest(Wine_model_2)
a
summary(Wine_model_2)
confint(Wine_model_2)

Wine_model_3 <- lm(quality ~ `volatile acidity` + alcohol + density, data = wine_data_f )
vif(Wine_model_3)
mean(vif(Wine_model_3))
1/vif(Wine_model_3)
plot(Wine_model_3)
a = durbinWatsonTest(Wine_model_3)
a
summary(Wine_model_3)
confint(Wine_model_3)

Wine_model_4 <- lm(quality ~ `volatile acidity` + alcohol + `residual sugar`+  sulphates, data = wine_data_f )
vif(Wine_model_4)
mean(vif(Wine_model_4))
1/vif(Wine_model_4)
plot(Wine_model_4)
a = durbinWatsonTest(Wine_model_4)
a
summary(Wine_model_4)
confint(Wine_model_4)

Wine_model_5 <- lm(quality ~ `volatile acidity` + alcohol + `residual sugar`+  sulphates + `total sulfur dioxide`, data = wine_data_f)
vif(Wine_model_5)
mean(vif(Wine_model_5))
1/vif(Wine_model_5)
plot(Wine_model_5)
a = durbinWatsonTest(Wine_model_5)
a
summary(Wine_model_5)
confint(Wine_model_5)

anova(Wine_model_2, Wine_model_4)
anova(Wine_model_3, Wine_model_4)
anova(Wine_model_4, Wine_model_5)
anova(Wine_model_5, quality_model)


# understanding influenctial points if any in model 4
str(wine_data_filter)
wine_data_s$fitted <- Wine_model_4s$fitted
wine_data_s$residuals <- Wine_model_4s$residuals
wine_data_s$standardized.residuals <- rstandard(Wine_model_4s)
wine_data_s
possible.outliers <- subset(wine_data_s, standardized.residuals < -1.96 | standardized.residuals > 1.96)
possible.outliers


# 11 feature model
quality_model <-lm(quality ~ `fixed acidity` + `volatile acidity` + `citric acid` + `residual sugar` + chlorides + `free sulfur dioxide` + `total sulfur dioxide` + density + pH + sulphates + alcohol, data = wine_data_f)
summary(quality_model)
plot(wine_data_f$quality)


  