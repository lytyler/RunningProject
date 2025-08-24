#Lanette Tyler
#Personal Project
#Running by Grade

#Step 1: One-way ANOVA, fixed effects, data from NC home school XC state champs 2024
#Do older boys run faster?

#Preliminary Work####
##read in and view data########################################################
XCstate <- read.csv(file.choose()) #RunningByGrade.csv in R data files folder on desktop
head(XCstate)
summary(XCstate)

##clean things up#############################################################
##change column name
colnames(XCstate)[colnames(XCstate)=='Time..seconds.'] <- 'Seconds'

##filter out grades below 9
XCstate <- subset(XCstate,Grade >8)

##make Grade column a factor
XCstate$Grade <- factor(XCstate$Grade)

##attach data file for ease of programming
attach(XCstate)

#Summarize data##############################################################
##statistics####
mean(Seconds)
median(Seconds)
sd(Seconds)
length(Seconds)
tapply(Seconds,Grade,mean)
tapply(Seconds,Grade,median)
tapply(Seconds,Grade,sd)
tapply(Seconds,Grade,length)

##visuals####
hist(Seconds)
boxplot(Seconds~Grade)

install.packages("MASS")
install.packages("rcompanion")
library(MASS)
library(rcompanion)
plotNormalHistogram(Seconds)

#Attempt 1 to model data#####################################################
##make model####
Model <- lm(Seconds~Grade)
summary(Model)
anova(Model)

##assess model adequacy####
#Diagnostic plots of residuals

###check normality ####
####Q:Q plot
qqnorm(residuals(Model))
#straight line means normality
#pretty straight, but top right has some issues

####Q:Q plot with line
qqline(residuals(Model))
#puts a line through the points of the q:q plot

####Shapiro test of normality
#for whole dataset
shapiro.test(resid(Model))
#Ho is that the data is normal
#it's not normal

####Histogram
res <- residuals(Model)
hist(res)

####Boxplot
boxplot(res,main = "Boxplot of Residuals")

####Shapiro test of normality for each sub group
tapply(Seconds,Grade,shapiro.test)
#not all are normal, only grade 11

###check constant variance####
####Residuals Plot
plot(fitted.values(Model),residuals(Model),xlab="Fitted Values",ylab="Residuals")
####add line to residuals plot
abline(h=0)

####test for equal variance
#####Bartlett Test
install.packages("mvoutlier")
library(mvoutlier)
bartlett.test(Seconds~Grade)

#####Levene Test
install.packages("car")
library(car)
leveneTest(Seconds,Grade)

detach(XCstate)

#Attempt 2 to model Data: Try transformation: square root of data##############
XCstate$SqRtSeconds=sqrt(XCstate$Seconds)
attach(XCstate)

##visuals####
hist(SqRtSeconds)
boxplot(SqRtSeconds~Grade)
plotNormalHistogram(SqRtSeconds)

##make model####
SqRtModel <- lm(SqRtSeconds~Grade)
anova(SqRtModel)
summary(SqRtModel)

##check model assumptions for square root model####

###check normality ####
###Q:Q plot
qqnorm(residuals(SqRtModel))
#straight line means normality
#pretty straight, but top right has some issues (again)

###Q:Q plot with line
qqline(residuals(SqRtModel))
#puts a line through the points of the q:q plot

###Shapiro test of normality
#for whole dataset
shapiro.test(resid(SqRtModel))
#Ho is that the data is normal
#it's not normal

###Histogram
res <- residuals(SqRtModel)
hist(res)

###Boxplot
boxplot(res,main = "Boxplot of Residuals")

#Shapiro test of normality for each sub group
tapply(SqRtSeconds,Grade,shapiro.test)
#not all are normal, only grades 10 (barely) and 11

###check constant variance####
#Residuals Plot
plot(fitted.values(SqRtModel),residuals(SqRtModel),xlab="Fitted Values",ylab="Residuals")
#add line to residuals plot
abline(h=0)

#test for equal variance
###Bartlett Test
install.packages("mvoutlier")
library(mvoutlier)
bartlett.test(SqRtSeconds~Grade)

###Levene Test
install.packages("car")
library(car)
leveneTest(SqRtSeconds,Grade)

detach(XCstate)

#Attempt 3 to model data: Now try transforming Cube Root ######################
##visuals####
XCstate$CbRtSeconds <- (XCstate$Seconds)**(1/3)

hist(XCstate$CbRtSeconds)
boxplot(XCstate$CbRtSeconds)
plotNormalHistogram(CbRtSeconds)


attach(XCstate)

##make model####
#ANOVA of CbRt
CbRtModel <- lm(CbRtSeconds~Grade)
anova(CbRtModel)

## check model assumptions####
###check normality ####
###Q:Q plot
qqnorm(residuals(CbRtModel))
#straight line means normality
#pretty straight, but top right has some issues again

###Q:Q plot with line
qqline(residuals(CbRtModel))
#puts a line through the points of the q:q plot

###Shapiro test of normality
#for whole dataset
shapiro.test(resid(CbRtModel))
#Ho is that the data is normal
#it's not normal

###Histogram
res <- residuals(CbRtModel)
hist(res)

###Boxplot
boxplot(res,main = "Boxplot of Residuals")

#Shapiro test of normality for each sub group
tapply(CbRtSeconds,Grade,shapiro.test)
#not all are normal, only grades 10 and 11

###check constant variance####
#Residuals Plot
plot(fitted.values(CbRtModel),residuals(CbRtModel),xlab="Fitted Values",ylab="Residuals")
#add line to residuals plot
abline(h=0)

#test for equal variance
###Bartlett Test
install.packages("mvoutlier")
library(mvoutlier)
bartlett.test(CbRtSeconds~Grade)

###Levene Test
install.packages("car")
library(car)
leveneTest(CbRtSeconds,Grade)

detach(XCstate)


#Attempt 4 to model data: now try log transformation###########################

##visuals####
XCstate$logSeconds <- log(XCstate$Seconds)
hist(XCstate$logSeconds)
boxplot(XCstate$logSeconds)
boxplot(XCstate$logSeconds~XCstate$Grade)
plotNormalHistogram(logSeconds)

attach(XCstate)

##make model (log)####
#ANOVA of log
LogModel <- lm(logSeconds~Grade)
anova(LogModel)

##check model assumptions
###check normality ####
###Q:Q plot
qqnorm(residuals(LogModel))
#straight line means normality
#pretty straight, but top right has some issues again

###Q:Q plot with line
qqline(residuals(LogModel))
#puts a line through the points of the q:q plot

###Shapiro test of normality
#for whole dataset
shapiro.test(resid(LogModel))
#Ho is that the data is normal
#it's not normal

###Histogram
res <- residuals(LogModel)
hist(res)

###Boxplot
boxplot(res,main = "Boxplot of Residuals")

#Shapiro test of normality for each sub group
tapply(logSeconds,Grade,shapiro.test)
#not all are normal, only grades 10 and 11

###check constant variance####
#Residuals Plot
plot(fitted.values(LogModel),residuals(LogModel),xlab="Fitted Values",ylab="Residuals")
#add line to residuals plot
abline(h=0)

#test for equal variance
###Bartlett Test
install.packages("mvoutlier")
library(mvoutlier)
bartlett.test(logSeconds~Grade)

###Levene Test
install.packages("car")
library(car)
leveneTest(logSeconds,Grade)

detach(XCstate)

#Attempt 5 to make model: gamma distribution###################################
##fit gamma distribution####
install.packages("fitdistrplus")
library(fitdistrplus)

gammaFit <- fitdist(Seconds,distr="gamma",method="mle")
summary(gammaFit)
plot(gammaFit)

#Attempt 6 to model data: try Tukey Transformation############################
#try tukey transformation
##visuals####
TukeyTrans = transformTukey(Seconds)
XCstate$TukeyTrans <- TukeyTrans
hist(TukeyTrans)
boxplot(TukeyTrans)
plotNormalHistogram(TukeyTrans)
boxplot(XCstate$TukeyTrans~Grade)

##make model####
#ANOVA of TukeyTransformed data
TTModel <- lm(XCstate$TukeyTrans~Grade)
anova(TTModel)
summary(TTModel)

##check model assumptions####
###check normality ####
###Q:Q plot
qqnorm(residuals(TTModel))
#straight line means normality
#very nice!

###Q:Q plot with line
qqline(residuals(TTModel))
#puts a line through the points of the q:q plot
#okay maybe not as nice now

###Shapiro test of normality
#for whole dataset
shapiro.test(resid(TTModel))
#Ho is that the data is normal
#it's normal!

###Histogram
res <- residuals(TTModel)
hist(res)

###Boxplot
boxplot(res,main = "Boxplot of Residuals")

#Shapiro test of normality for each sub group
tapply(XCstate$TukeyTrans,Grade,shapiro.test)
#not all are normal, grades 9,10,12 are normal; 11 is not but not terribly far...

###check constant variance####
#Residuals Plot
plot(fitted.values(TTModel),residuals(TTModel),xlab="Fitted Values",ylab="Residuals")
#add line to residuals plot
abline(h=0)

#test for equal variance
###Bartlett Test
install.packages("mvoutlier")
library(mvoutlier)
bartlett.test(XCstate$TukeyTrans~Grade)

###Levene Test
install.packages("car")
library(car)
leveneTest(XCstate$TukeyTrans,Grade)

#Multiple Comparisons - which ones are different?#############################
#use Tukey Transformed Data
TTModelForHSD <- aov(XCstate$TukeyTrans~Grade)
summary(TTModelForHSD)
TukeyHSD(TTModelForHSD)
plot(TukeyHSD(TTModelForHSD))

detach(XCstate)