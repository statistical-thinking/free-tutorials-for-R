###############################
# STATISTICAL THINKING (2020) #
###############################

####################
# CLINICAL STUDIES #
####################

# FULL DATASET
ToothGrowth

# EXCERPT OF DATASET 
head(ToothGrowth)

########################
# DESCRIPTIVE ANALYSIS #
########################

# SUMMARY
summary(ToothGrowth)

# BOXPLOTS
boxplot(ToothGrowth$len ~ ToothGrowth$dose, xlab = "dose", ylab = "length")

##############
# SUBSAMPLES #
##############

# CREATING SUBSAMPLES
vc <- subset(ToothGrowth, supp=="VC")
oj <- subset(ToothGrowth, supp=="OJ")

# SUMMARY SUBSAMPLES
summary(vc[c(1,3)])
summary(oj[c(1,3)])

# BOXPLOTS SUBSAMPLES
par(mfrow=c(1,2))
boxplot(vc[c(1,3)], main="VC")
boxplot(oj[c(1,3)], main="OJ")

#################
# DETAILED PLOT #
#################

# DETAILED PLOT FOR DIFFERENT SUPPLEMENTS
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
xlab = "Toothgrowth and given type of supplement")

##########
# T-TEST #
##########

# CHECK FOR SIGNIFICANT DIFFERENCE
t.test(ToothGrowth$len ~ ToothGrowth$supp)

#############
# THAT'S IT #
#############