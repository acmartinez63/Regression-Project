setwd("/Users/alinamartinez/Documents/Code Sample")

install.packages("stats")
library(stats)

install.packages("mice")
library(mice)

install.packages("MASS")
library(MASS)

install.packages("corrplot")
library(corrplot)

install.packages("psych")
library(psych)

install.packages
library(leaps)

install.packages("car")
library(car)

install.packages("openxlsx")
library(openxlsx)

missdata1 <- read.xlsx("Regression Project - merged data.xlsx", sheet = 4, rowNames = TRUE) #, row.names = 1)

#show data set
str(missdata1)

#look for patterns in missing data
md.pattern(missdata1)
#missing data appears random

#Imputation missing data using "mice" package (takes a while)
newdata = complete(mice(data = missdata1, method = "cart") )

#check if Y has linear relationship with regressors``
fullmodel = lm(Y~., data = newdata)

summary(fullmodel) 
#P value < 2.2*10^-16
#Bonferroni Correction: 0.05 / 31 = 0.001613

#Residual Analysis of Full Model
fullmodel.res = rstandard(fullmodel) # t = residuals

mean(fullmodel.res) #mean is approximately 0

yhat = fullmodel$fitted.values
plot(yhat,fullmodel.res, abline(0,0)) #residuals approximately centered around zero

plot(fullmodel, 3) #shows homoscedasticity

plot(fullmodel, 2) #normal distribution of residuals
#no violations of assumptions, therefore, no need to look for transformation


#Check to see approx. number of regressors needed for reduced model
regfit.full = regsubsets(Y~.,newdata, nvmax = 31)
print(regfit.full)

reg.summary <-summary(regfit.full)

plot(reg.summary$adjr2, xlab = "number of variables", ylab = "r square", type = "l")
#Reduced Model will have approx two variables and a maximum r^2 value of 26

#print correlation matrix
corrplot(cor(newdata, method = "pearson"))

#E3 and E5 appear to be correlated with Y
#Genetic variables appear to not be correlated with Y
#E3 and E5 appear to not be correlated with genetic variables
#will only consider models with E3 and E5 as regressors

#look for multicollinearity
vif = car::vif(fullmodel)
vif
#low VIF's suggest no multicollinearity between independent variables

#look at summary of first proposed model
redmodel1= lm(newdata$Y~newdata$E3)
summary(redmodel1)

#perform Partial F test - full model vs reduced model
#reject Ho is p-val < 0.05
anova(redmodel1, fullmodel)
#P value < 2.2*10^-16
#Bonferroni Correction: 0.05/1 = 0.05
#small p val - will add E3

#compare AIC values
AICfull = AIC(stepAIC(fullmodel,method = "both"))
AICmod1 = AIC(stepAIC(redmodel1,method = "both"))

AICfull-AICmod1
#AIC difference of -210

#Residual Analysis of Reduced Model 1
redmodel1.res = rstudent(redmodel1)

mean(redmodel1.res) #mean is approximately 0

plot(yhat,redmodel1.res, abline(0,0)) #residuals approximately centered around zero

plot(redmodel1, 3) #shows homoscedasticity

plot(redmodel1, 2) #residuals are approx normal
#no violations of assumptions

#second proposed model
redmodel2= lm(newdata$Y~newdata$E3 + newdata$E5)
summary(redmodel2)
#P value < 2.2*10^-16
#Bonferroni Correction: 0.05/2 = 0.025

#perform F test - reduced model 1 vs reduced model 2
anova(redmodel2, redmodel1)
#P value < 2.2*10^-16
#small p val - will add E5 to model

#compare AIC values
AICmod2 = AIC(stepAIC(redmodel2,method = "both"))
AICmod2-AICmod1
#AIC difference of -205

#Residual Analysis of Reduced Model 2
redmodel2.res = rstudent(redmodel2)

mean(redmodel2.res) #mean is approximately 0

plot(yhat,redmodel2.res, abline(0,0)) #residuals approximately centered around zero

plot(redmodel2, 3) #shows homoscedasticity

plot(redmodel2, 2) #residuals are approx normal
#no violations of assumptions


cor(newdata$E3, newdata$E5)
#low correlation between E3 and E5

#third proposed model
redmodel3= lm(newdata$Y~newdata$E3*newdata$E5)
summary(redmodel3)
#P value < 2.2*10^-16
#Bonferroni Correction: 0.05/2 = 0.025

#perform F test - reduced model 2 vs reduced model 3
anova(redmodel3, redmodel2)
#P value = 0.056
#Bonferroni Correction: 0.05/3 = 0.017

#compare AIC values
AICmod3 = AIC(stepAIC(redmodel3,method = "both"))
AICmod3 - AICmod2
#AIC difference of -11

#Residual Analysis of Reduced Model 2
redmodel3.res = rstudent(redmodel3)

mean(redmodel3.res) #mean is approximately 0

plot(yhat,redmodel3.res, abline(0,0)) #residuals approximately centered around zero

plot(redmodel3, 3) #shows homoscedasticity

plot(redmodel3, 2) #residuals are approx normal
#no violations of assumptions


#Use proposed model 3 as final model
finalmodel = redmodel3

plot(newdata$E3*newdata$E5, newdata$Y, main= "Relationship between E3, E5, and Depression Risk", xlab="Environmental Variables E3 and E5", ylab="Depression Diagnosis")
#ANOVA table of final model
anova(finalmodel)
