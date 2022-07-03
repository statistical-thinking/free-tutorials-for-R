###############################
# STATISTICAL THINKING (2022) #
###############################

####################
# NONLINEAR MODELS #
####################

# USE SAME SET OF RANDOM VARIABLES
set.seed(2000)

########################
# BACKGROUND KNOWLEDGE #
########################

# RNORM -> SIMULATE NORMAL DISTRIBUTION WITH GIVEN N, MEAN and SD
# RUNIF -> SIMULATE UNIFORM DISTRIBUTION WITH GIVEN N, MIN & MAX

##################################################
# EXPLONENTIAL FUNCTIONS WITH NEGATIVE EXPONENTS #
##################################################

# SIMULATED DATA
x <- seq(0,50,1)
y <- runif(1,5,15)*exp(-runif(1,0.01,0.05)*x)+rnorm(51,0,0.5)

# SET STARTING VALUES
a_start<-8
b_start<-2*log(2)/a_start

# ESTIMATION OF GOODNESS OF FIT
m<-nls(y~a*exp(-b*x),start=list(a=a_start,b=b_start))
cor(y,predict(m))

# PLOT
plot(x,y)

# NONLINEAR MODEL 
lines(x,predict(m),col="blue",lty=2,lwd=3)

####################################################
# MICHAELIS-MENTEN EQUATION & LOGARITHMIC FUNCTION #
####################################################

# SIMULATED DATA
y <- ((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)

# ESTIMATION OF GOODNESS OF FIT
m<-nls(y~a*x/(b+x), start=list(a=a_start,b=b_start))
cor(y,predict(m))

# PLOT
plot(x,y)

# NONLINEAR MODEL
lines(x,predict(m),lty=2,col="blue",lwd=3)

#############
# THAT'S IT #
#############
