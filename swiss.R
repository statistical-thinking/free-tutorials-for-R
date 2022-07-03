###############################
# STATISTICAL THINKING (2022) #
###############################

##################
# BASIC ANALYSIS #
##################

# FULL DATASET
swiss

# EXCERPT OF DATASET 
head(swiss)

########################
# DESCRIPTIVE ANALYSIS #
########################

# SUMMARY
summary(swiss)

# GENERAL PLOT
plot(swiss)

# BOXPLOTS
boxplot(swiss)

######################
# BIVARIATE ANALYSIS #
######################

# CORRELATIONS
cor(swiss)

# PLOT SPECIFIC CORRELATION WITH LINEAR SLOPE
plot(swiss$Fertility, swiss$Education)
abline(lm(swiss$Education ~ swiss$Fertility))

# PLOT ALL BIVARIATEE RELATIONSHIPS BETWEEN VARIABLES
require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "swiss data",
col = 3 + (swiss$Catholic > 50))

#########################
# MULTIVARIATE ANALYSIS #
#########################

# LINEAR REGRESSION
lm(formula = Fertility ~., data = swiss)

#############
# THAT'S IT #
#############
