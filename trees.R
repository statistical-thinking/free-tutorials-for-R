###############################
# STATISTICAL THINKING (2022) #
###############################

######################
# DATA VISUALISATION #
######################

# FULL DATASET
trees

# EXCERPT OF DATASET
head(trees)

#############################
# INSTALL REQUIRED PACKAGES #
#############################

# CORRPLOT
install.packages("corrplot", dependencies=TRUE)
library(corrplot)

########################
# DESCRIPTIVE ANALYSIS #
########################

# SUMMARY
summary(trees)

# GENERAL PLOT
plot(trees)

# BOXPLOTS
boxplot(trees)

######################
# BIVARIATE ANALYSIS #
######################

# CORRELATIONS
cor(trees)

# VISUALISE CORRELATIONS
M <- cor(trees)
corrplot(M)

#############
# THAT'S IT #
#############
