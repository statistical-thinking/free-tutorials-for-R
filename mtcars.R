###############################
# STATISTICAL THINKING (2020) #
###############################

####################
# REGRESSION MODEL #
####################

# FULL DATASET
mtcars

# EXCERPT OF DATASET 
head(mtcars)

########################
# DESCRIPTIVE ANALYSIS #
########################

# SUMMARY
summary(mtcars)

######################
# BIVARIATE ANALYSIS #
######################

# PLOT SPECIFIC CORRELATION WITH LINEAR SLOPE
plot(mtcars$disp, mtcars$hp)
abline(lm(mtcars$hp ~ mtcars$disp), col="red")

# CORRELATION BETWEEN TWO VARIABLES
cor(mtcars$disp, mtcars$hp)

# CORRELATIONS BETWEEN ALL VARIABLES
cor(mtcars)

###################
# ADDITIONAL PLOT #
###################

# PLOT ALL BIVARIATE RELATIONSHIPS BETWEEN VARIABLES
require(stats); require(graphics)
pairs(mtcars, panel = panel.smooth, main = "mtcars data",
col = 3)

#########################
# MULTIVARIATE ANALYSIS #
#########################

# BASIC LINEAR REGRESSION MODEL (MPG AS DEPENDENT)
step(lm(data = mtcars, mpg ~.), trace = 0, stps = 11)

# BASIC LINEAR REGRESION MODEL AS OBJECT
regression <- step(lm(data = mtcars, mpg ~.), trace = 0, stps = 11)

# SUMMARY OF BASIC LINEAR REGRESSION MODEL
summary(regression)

# ANOTHER BASIC LINEAR REGRESSION MODEL (QSEC AS DEPENDENT)
step(lm(data = mtcars, qsec ~.), trace = 0, stps = 11)

#############
# THAT'S IT #
#############