###############################
# STATISTICAL THINKING (2020) #
###############################

####################
# MACHINE LEARNING #
####################

# FULL DATASET
iris

# EXCERPT OF DATASET 
head(iris)

####################
# INSTALL PACKAGES #
####################

# CARET PACKAGE
install.packages("caret", dependencies=TRUE)
library(caret)

########################
# DESCRIPTIVE ANALYSIS #
########################

# SUMMARY
summary(iris)

# GENERAL PLOT
plot(iris)

# BOXPLOTS
boxplot(iris[1:4])

# BOXPLOTS SORTED BY SPECIES
x <- iris[,1:4]
y <- iris[ ,5]
featurePlot(x=x, y=y, plot="box")

######################
# BIVARIATE ANALYSIS #
######################

# CORRELATIONS
cor(iris[1:4])

# PLOT ALL BIVARIATEE RELATIONSHIPS BETWEEN VARIABLES
require(stats); require(graphics)
pairs(iris, panel = panel.smooth, main = "iris data",
col = 3)

#########################################
# MACHINE LEARNING DATASET PREPARATIONS #
#########################################

# SPLITTING DATASET INTO 20% AND 80% SUB-DATASETS
validation_index <- createDataPartition(iris$Species, p=0.80, list=FALSE)

# CREATING SUB-DATASET (VALIDATION)
validation <- iris[-validation_index, ]
summary(validation)
dim(validation)

# CREATING SUB-DATASET (MODEL)
model <- iris[validation_index, ]
summary(model)
dim(model)

###############################
# MACHINE LEARNING ALGORITHMS #
###############################

# CODE FOR CROSS-VALIDATION
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# LINEAR ALGORITHM (LDA - LINEAR DISCRIMINANT ANALYSIS)
fit.lda <- train(Species ~ ., data=model, method ="lda", metric=metric, trControl=control)

# NON-LINEAR ALGORITHM (KNN - K-NEAREST-NEIGHBORS)
fit.knn <- train(Species ~ ., data=model, method ="knn", metric=metric, trControl=control)

# COMPLEX ALGORITHM (RF - RANDOM FOREST)
fit.rf <- train(Species ~ ., data=model, method ="rf", metric=metric, trControl=control)

######################
# TESTING ALGORITHMS #
######################

# COMPARING ACCURACIES
results <- resamples(list(lda=fit.lda, knn=fit.knn, rf=fit.rf))
summary(results)
dotplot(results)

# CHOOSING ONE ALGORITHM
print(fit.lda)

########################
# VALIDATING ALGORITHM #
########################

# MODEL-DATASET
predictions <- predict(fit.lda, model)
confusionMatrix(predictions, model$Species)

# VALIDATION-DATASET
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

#############
# THAT'S IT #
#############