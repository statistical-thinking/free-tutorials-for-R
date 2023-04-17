####################################
### 1. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
iris

# Univariate Statistik (Teil 1)
summary(iris)

# Univariate Statistik (Teil 2)
boxplot(iris[1:4])

# Univariate Statistik (Teil 3)
setosa <- subset(iris, Species=="setosa")
versicolor <- subset(iris, Species=="versicolor")
virginica <- subset(iris, Species=="virginica")
summary(setosa[c(1:4)])
summary(versicolor[c(1:4)])
summary(virginica[c(1:4)])

# Univariate Statistik (Teil 4)
par(mfrow=c(2,2))
boxplot(setosa[c(1:4)], main="setosa", ylim=c(1,8))
boxplot(versicolor[c(1:4)], main="versicolor", ylim=c(1,8))
boxplot(virginica[c(1:4)], main="virginica", ylim=c(1,8))

# Mustererkennung (Teil 1 und 2)
identification <- subset(iris, Petal.Length>="1" & Petal.Length<="2" & Petal.Width<="1", select=c(Species))
summary(identification)

# Mustererkennung (Teil 3)
identification <- subset(iris, Petal.Length>="3" & Petal.Length<="5" & Petal.Width<="2", select=c(Species))
summary(identification)
identification <- subset(iris, Petal.Length>="4.5" & Petal.Width>="1.6", select=c(Species))
summary(identification)

####################################
### 2. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
mtcars

# Univariate Statistik (Teil 1)
summary(mtcars)
boxplot(mtcars$cyl)

# Univariate Statistik (Teil 2)
help(mtcars)

# Bivariate Statistik (Teil 1)
plot(mtcars$disp, mtcars$hp)
plot(mtcars$hp~mtcars$disp)

# Bivariate Statistik (Teil 2)
abline(lm(mtcars$hp~mtcars$disp))

# Bivariate Statistik (Teil 3)
cor(mtcars$disp, mtcars$hp)

# Bivariate Statistik (Teil 4)
cor(mtcars)

# Multivariate Statistik (Teil 1)
step(lm(data=mtcars, mpg~.), trace=0)

# Multivariate Statistik (Teil 2)
regression <- step(lm(data=mtcars, mpg~.), trace=0)
summary(regression)

####################################
### 3. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
erfolgreich <- cbind(70, 55)
nicht_erfolgreich <- cbind(30, 45)
matrix <- rbind(erfolgreich, nicht_erfolgreich)
matrix

# Abgleich mit Chi-Quadrat-Verteilungstabelle
chisq.test(matrix)

####################################
### 4. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
ToothGrowth

# Univariate Statistik (Teil 1)
summary(ToothGrowth)
boxplot(ToothGrowth$len)
boxplot(ToothGrowth)

# Bivariate Statistik (Teil 1)
boxplot(ToothGrowth$len~ToothGrowth$dose, xlab="dose", ylab="length")

# Subsets interpretieren (Teil 1)
vc <- subset(ToothGrowth, supp=="VC")
oj <- subset(ToothGrowth, supp=="OJ")
vc
oj
summary(vc[c(1:3)])
vc[c(2)]

# Bivariate Statistik (Teil 2)
boxplot(ToothGrowth$len~ToothGrowth$supp, xlab="supp", ylab="length")
par(mfrow=c(1,2))
boxplot(vc[c(1,3)], main="VC")
boxplot(oj[c(1,3)], main="OJ")

# Bivariate Statistik (Teil 3)
t.test(ToothGrowth$len~ToothGrowth$supp)

####################################
### 5. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
install.packages("psych", dependencies=TRUE)
library(psych)
bfi
help(bfi)

# Univariate Statistik (Teil 1)
summary(bfi[c(1:5)])
summary(bfi[c(16:20)])
agreeableness_sum <- (bfi$A1+bfi$A2+bfi$A3+bfi$A4+bfi$A5)/5
summary(agreeableness_sum)
neuroticism_sum <- (bfi$N1+bfi$N2+bfi$N3+bfi$N4+bfi$N5)/5
summary(neuroticism_sum)

# Bivariate Statistik (Teil 1)
cor(bfi[c(1:5)], use="complete.obs")
cor(bfi[c(16:20)], use="complete.obs")

# Bivariate Statistik (Teil 2)
install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(bfi[c(1:25)], use="complete.obs")
corrplot(cor_matrix)

# Faktorenanalyse (Teil 1)
items <- bfi[1:25]
items <- items[complete.cases(items),]
fa(items, nfactors=5, rotate="varimax")

# Faktorenanalyse (Teil 2)
fa.parallel(items)

####################################
### 6. Ü B U N G S A U F G A B E ###
####################################

# Einleitung
iris

# Univariate Statistik (Teil 1)
summary(iris)

# Univariate Statistik (Teil 2)
boxplot(iris[1:4])

# Univariate Statistik (Teil 3)
install.packages("caret", dependencies=TRUE)
library(caret)
x <- iris[,1:4]
y <- iris[,5]
featurePlot(x=x, y=y, plot="box")

# Bivariate Analyse
cor(iris[1:4])

# Machine Learning (Teil 1)
validation_index <- createDataPartition(iris$Species, p=0.80, list=FALSE)
validation <- iris[-validation_index, ]
model <- iris[validation_index, ]
summary(validation)
summary(model)

# Machine Learning (Teil 2)
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
fit.lda <- train(Species ~ ., data=model, method ="lda", metric=metric, trControl=control)
fit.knn <- train(Species ~ ., data=model, method ="knn", metric=metric, trControl=control)
fit.rf <- train(Species ~ ., data=model, method ="rf", metric=metric, trControl=control)

# Machine Learning (Teil 3)
results <- resamples(list(lda=fit.lda, knn=fit.knn, rf=fit.rf))
summary(results)
dotplot(results)

# Machine Learning (Teil 4)
print(fit.lda)

# Machine Learning (Teil 5)
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)