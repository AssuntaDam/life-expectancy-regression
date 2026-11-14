install.packages("BAS")
install.packages("carData")
install.packages("car")
install.packages("corrplot")
install.packages("ellipse")
install.packages("faraway")
install.packages("GGally")
install.packages("leaps")
install.packages("MASS")
install.packages("Matrix")

library(BAS)
library(carData)
library(car)
library(corrplot)
library(ellipse)
library(faraway)
library(GGally)
library(leaps)
library(MASS)
library(Matrix)
#library(rgl)


#-------- Data set preparation --------
lifexp = read.csv('life_expectancy.csv')

#Remove the rows that contain missing values
lifexp=na.omit(lifexp)

#Turn the categorical variable into a factor
lifexp$Status = factor(lifexp$Status, ordered = F)

#Choose the year: 2013, because it is recent and has many observations
lifexp=lifexp[which(lifexp$Year==2013),]

lifexp = lifexp[lifexp$Country != "South Africa", ]

#Use the country names as row names
rownames(lifexp)=lifexp[,'Country']
lifexp=lifexp [, c(-1, -2)]

#Check the dimensions
dim(lifexp) #124  16

#Summary of the columns
summary(lifexp)

#Check the type of the columns
str(lifexp)


#Visualize the data

#pairs(lifexp[ , c('Status','Population', 'Hepatitis.B','Measles','Polio','Diphtheria','HIV.AIDS','infant.deaths','under.five.deaths','Total.expenditure','GDP','BMI','thinness..1.19.years','Alcohol','Schooling','Life.expectancy')], pch = 16)

#better version:
ggpairs(lifexp[, c('Status','Population','Diphtheria','HIV.AIDS','Total.expenditure','GDP','BMI','thinness..1.19.years','Schooling','Life.expectancy')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

#Life.expectancy is the response and the other variables are the predictors




#-------- Considerations on p-values --------


#Multiple linear regression model with all the covariates:
mod0 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Measles+Polio+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling, lifexp)
summary(mod0)
#R^2 and adjusted R^2 are fairly close to 1 => the model explains "Life.expectancy" well


#Polio has a high p-value (Pr(>|t|)), so its significance is low: refit the model without it
mod1 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Measles+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling , lifexp)
summary(mod1)

#Remove Measles as well
mod2 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling , lifexp)
summary(mod2)

#Remove Alcohol as well
mod3 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod3)

#Remove Hepatitis.B as well
mod = lm(Life.expectancy ~ Status+Population+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod)


#Variance-covariance matrix of the coefficients
vcov(mod)

#Design matrix
X = model.matrix(mod)
X


#-------- AIC, BIC and adjusted R^2 --------


#The Akaike information criterion (AIC) is an estimator of out-of-sample prediction error and thereby relative quality of statistical models for a given set of data. Given a set of candidate models for the data, the preferred model is the one with the minimum AIC value.
#Thus, AIC rewards goodness of fit (as assessed by the likelihood function), but it also includes a penalty that is an increasing function of the number of estimated parameters.
#AIC = −2 · log( likelihood ) + 2r where r is the number of parameters in the model.
AIC(mod0)
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(mod)
#Even just removing the covariates with high p-values lowers the AIC, which is good


#BIC
#The formula for the Bayesian information criterion (BIC) is similar to the formula for AIC, but with a different penalty for the number of parameters.
#The BIC generally penalizes free parameters more strongly than the Akaike information criterion, though it depends on the size of n and relative magnitude of n and r. It works well when n >> r.
#BIC = −2 · log( likelihood ) + r · log(n) 8
BIC(mod0)
BIC(mod1)
BIC(mod2)
BIC(mod3)


#Which covariates to remove, starting from mod0, according to the AIC:
step(mod0, direction = "backward" , trace = T)
#Starting from mod0, at each step it removes the variable that lowers the AIC the most, until the model with the lowest possible AIC is reached.
#"trace=TRUE" shows every step
#According to the AIC the best model is like our "mod" (after removing the variables with high p-values)
#but without infant.deaths and with Hepatitis.B (the two are more or less similar)

#The same with the BIC:
n = dim(lifexp)[1]
mod0_BIC_back = step(mod0, direction = "backward", k = log(n), trace = T)
#The BIC removes too many variables => the model is too simple and explains little

#Using the adjusted R^2 instead:
x = model.matrix(mod0) [ , -1 ]
y = lifexp$Life.expectancy
adjr = leaps(x, y, method = "adjr2")
names(adjr)
adjr
bestmodel_adjr2_ind = which.max(adjr$adjr2)
adjr$which[bestmodel_adjr2_ind,]

#The best model for the adjusted R^2 is without Polio, infant.deaths, under.five.deaths and Alcohol, that is:
#Status+Population+Hepatitis.B+Measles+Diphtheria+HIV.AIDS+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling


#-------- Initial choice of the model --------


#Model chosen for now: the one obtained after removing the covariates with high p-values,
#because it still has a lower AIC than the initial model and a fairly high adjusted R^2
#(we look for a good compromise between prediction and interpretation: we want significant covariates)
mod = lm(Life.expectancy ~ Status+Population+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod)



#-------- Collinearity --------


#VIF (Variance Inflation Factor): if vif > 5 => high probability of collinearity (=> the variables are too similar to each other)
vif(mod)

# StatusDeveloping           Population           Diphtheria           HIV.AIDS          infant.deaths
# 1.533178                   1.885529             1.380966               1.311809        231.732048
# under.five.deaths    Total.expenditure          GDP                  BMI               thinness..1.19.years
# 221.038155                 1.264204             1.316815             1.968811          2.464902
# Schooling
# 2.598384

#There is collinearity between infant.deaths and under.five.deaths, so we have to remove one of them
modn=update(mod,.~.-under.five.deaths)
summary(modn)

#infant.deaths has p-value=0.2 => remove it
modn=update(modn,.~.-infant.deaths)
summary(modn)

vif(modn)
# vif(modn)
# StatusDeveloping           Population           Diphtheria
# 1.527980             1.395381             1.315025
# HIV.AIDS    Total.expenditure                  GDP
# 1.275722             1.254280             1.316629
# BMI thinness..1.19.years            Schooling
# 1.944895             2.071147             2.554030


#-------- Homoscedasticity --------
#A sequence of random variables is homoscedastic if all its random variables have the same finite variance.


plot(modn$fit, modn$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = 'red')

#or
plot(modn, which=1)

#The plot of residuals against fitted values shows a random and symmetric scatter of the points around
#the horizontal line at zero, with no particular pattern. This supports the assumptions of linearity,
#homoscedasticity and independence of the residuals. The model therefore appears adequate with respect
#to these fundamental assumptions of linear regression.


#-------- Normality --------


modn_res = modn$residuals/summary(modn)$sigma
plot(modn$fitted, modn_res, xlab = 'Fitted values', ylab = 'Standarzized residuals')
qqnorm(modn$residuals)
qqline(modn$residuals, col = 'blue')

#If the p-value is high we do not reject H0 => the residuals are normal
shapiro.test(modn$res)
#p-value = 0.04227 (< 0.05, it has to be raised with a Box-Cox transformation)

#Histogram of residuals and boxplot are useful tools to look at the shape of the residual distribution and to
#see if the tails of the distributions are souspiciously heavy (i.e., there are many outliers) respectively.
hist(mod$res, 10, probability = TRUE, col = 'lavender', main = 'residuals')

#Residuals
boxplot(mod$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender')


#-------- Box-Cox --------


#Box-Cox transformation
par(mfrow = c( 1, 1 ))
bc <- boxcox(modn, plotit = TRUE)

#Find the value of lambda that maximizes the log-likelihood
best_lambda_ind = which.max(bc$y)
best_lambda = bc$x[best_lambda_ind] #1.88
best_lambda=2

#New model:
modb = lm(((Life.expectancy ^ best_lambda) - 1)/best_lambda ~ Status+Population+Diphtheria+HIV.AIDS+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling ,data = lifexp)
summary(modb)
#Some covariates have p-value > 0.1, but only slightly: evaluate before removing them

#Plot of the standardized residuals
modb_res = modb$residuals/summary(modb)$sigma
plot(modb$fitted, modb_res, xlab = 'Fitted values', ylab = 'Standarzized residuals')
qqnorm(modb$residuals)
qqline(modb$residuals, col = 'blue')

#If the p-value is high we do not reject H0 => the residuals are normal
shapiro.test(modb$res)
#p-value = 0.4867 > 0.05, ok

#The p-value increased from 0.04227 to 0.4867, so the transformation was effective => the residuals are now normal


#-------- Adjusted R^2 --------


#ggpairs(lifexp[ , c('Status','Population','Diphtheria','HIV.AIDS','Total.expenditure','GDP','BMI','thinness..1.19.years','Schooling','Life.expectancy')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

x = model.matrix(modb) [, -1]
y = lifexp$Life.expectancy
adjr = leaps(x, y, method = "adjr2")
names(adjr)
adjr
bestmodel_adjr2_ind = which.max(adjr$adjr2)
adjr$which[bestmodel_adjr2_ind,]

# 1    2    3    4    5    6    7    8    9
# TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

#According to the adjusted R^2 it is a good model

#Design matrix:
X = model.matrix(modb)



#-------- Leverage points --------


lev = hatvalues(modb) #same, using the name

#r = number of covariates
p=modb$rank  #rank of X (covariates plus intercept) = r+1
n = dim(lifexp)[1]

#An observation is a leverage point if hii > 2*p/n

plot(modb$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", pch = 16, col = 'black')
abline(h = 2 * p/n, lty = 2, col = 'red')
watchout_points_lev = lev[which( lev > 2 * p/n )]
watchout_ids_lev = seq_along(lev)[which( lev > 2 * p/n )]
points(modb$fitted.values[watchout_ids_lev], watchout_points_lev, col = 'red', pch = 16)

lev[lev> 2*p/n]

# Australia Luxembourg Bangladesh     Canada      Ghana      India    Lesotho
# 0.1909510  0.5931668  0.2403038  0.1699131  0.2063593  0.2673966  0.2566218
# Mauritania     Mexico   Pakistan     Panama  Swaziland
# 0.1902741  0.2121117  0.3109798  0.2542358  0.2505492

#Remove the leverage points from the model:
modbl = update(modb,.~., subset = (lev < 2*p/n))
summary(modbl)
#some p-values became > 0.1. Let us look instead at the residuals and Cook's distance

abs((modb$coefficients - modbl$coefficients)/modb$coefficients)
#This gives the relative change of the betas


#-------- Standardized residuals --------


plot(modb$res, ylab = "Residuals", main = "Plot of residuals")
sort(modb$res)

sort(modb$res) [c( 1, n )]
countries = row.names(lifexp)

mods = summary(modb)
res_std = modb$res/mods$sigma
watchout_ids_rstd = which(abs(res_std) > 2)
watchout_rstd = res_std[watchout_ids_rstd]
watchout_rstd

# Belgium    Lithuania       Angola     Maldives        Niger Sierra Leone
# 2.527076    -2.131865    -2.658726     2.006065     2.148179    -2.883166

# Standardized residuals
plot(modb$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals")
abline(h = c(-2,2), lty = 2, col = 'orange')
points(modb$fitted.values[watchout_ids_rstd],res_std[watchout_ids_rstd], col = 'red', pch = 16)
points(modb$fitted.values[watchout_ids_lev],res_std[watchout_ids_lev], col = 'orange', pch = 16)
legend('topright', col = c('red','orange'),c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n')


#-------- Studentized residuals --------


mods = summary(modb)
mods$sigma

# 'rstandard' gives studentized residuals automatically
stud = rstandard(modb)
watchout_ids_stud = which(abs(stud) > 2)
watchout_stud = stud[watchout_ids_stud]
watchout_stud

# Australia      Belgium    Lithuania     Portugal       Angola     Maldives
# -2.011698     2.730685    -2.209748     2.101283    -2.698723     2.136347
# Niger       Panama Sierra Leone
# 2.330511     2.016287    -3.024472


plot(modb$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals",pch=16)
points(modb$fitted.values[watchout_ids_stud],stud[watchout_ids_stud], col = 'pink', pch = 16)
points(modb$fitted.values[watchout_ids_lev],stud[watchout_ids_lev], col = 'orange', pch = 16)
#points(mod$fitted.values[watchout_ids_rstd],stud[watchout_ids_rstd], col ='red' , pch = 16)

#The standardized residuals coincide with some of the studentized ones
abline(h = c(-2,2), lty = 2, col = 'orange')
legend('topright', col = c('pink','orange'),c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n')


#-------- Cook's distance --------
#It combines leverage points and standardized residuals


Cdist = cooks.distance(modb)
watchout_ids_Cdist = which(Cdist > 4/(n-p))
watchout_Cdist = Cdist[watchout_ids_Cdist]
watchout_Cdist

# Australia      Belgium    Lithuania   Luxembourg     Portugal     Maldives
# 0.09551524   0.12499885   0.03632994   0.04150824   0.07678827   0.06120533
# Niger       Panama Sierra Leone    Swaziland
# 0.09611145   0.13859261   0.09186164   0.04118438

par(mfrow = c(1, 3))
plot(modb$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', ylab = 'Cooks Distance', main = 'Cooks Distance')
points(modb$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], col = 'magenta', pch = 16)
plot(modb$fitted.values, stud, pch = 16, xlab = 'Fitted values', ylab = 'Studentized Residuals', main = 'Studentized Residuals')
points(modb$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], col = 'blue', pch = 16)
plot(modb$fitted.values, lev, pch = 16, xlab = 'Fitted values', ylab = 'Leverages', main = 'Leverages')
points(modb$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ], col = 'red', pch = 16)


id_to_keep = !(1:n %in% watchout_ids_Cdist)




#-------- FINAL MODEL (based on Cook's distance) --------
#Update the model using the Cook's distance criterion


mod_finale = update(modb, .~., subset = (Cdist < 4/(n-p)))
summary(mod_finale)

#Remove thinness..1.19.years and GDP as well, which have p-value > 0.1
mod_finale = update(mod_finale, .~.-thinness..1.19.years-GDP, subset = (Cdist < 4/(n-p)))
summary(mod_finale)

#Normality
shapiro.test(mod_finale$res)

#Homoscedasticity
plot(mod_finale$fit, mod_finale$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = 'red')
#or
plot(mod_finale, which=1)
#ggpairs(lifexp[ , c('Status','Population','Diphtheria','HIV.AIDS','Total.expenditure', 'BMI', 'Schooling')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))


#-------- Prediction on a different year --------


lifexp14 = read.csv('life_expectancy.csv')

#Remove the rows that contain missing values
lifexp14=na.omit(lifexp14)

#Turn the categorical variable into a factor
lifexp14$Status = factor(lifexp14$Status, ordered = F)

#Choose the year
lifexp14=lifexp14[which(lifexp14$Year==2000),]

#Use the country names as row names
rownames(lifexp14)=lifexp14[,'Country']
lifexp14=lifexp14 [, c(-2)]

lifexp14 = lifexp14[lifexp14$Country != "South Africa", ]

predictions <- predict(mod_finale, newdata = lifexp14)
print(predictions)
y=((2*predictions)+1)^(0.5)
y
#y contains the predicted life expectancy of each country in 2000


#Add the predictions to the original data (visible in the table "lifeexp14" in the environment)
lifexp14$Predicted_Life_Expectancy <- y
#Remove the rows with missing values
lifexp14=na.omit(lifexp14)

#Split the results into two separate data frames: developed and developing countries
developed <- subset(lifexp14, Status == "Developed")
developing <- subset(lifexp14, Status == "Developing")
developing=na.omit(developing)
#See the tables "developed" and "developing" in the environment


#First results for the developed countries
head(developed)

#First results for the developing countries
head(developing)

library(ggplot2)


#Plot for the developed countries
ggplot(developed, aes(x = Country, y = Predicted_Life_Expectancy)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Predicted Life Expectancy for Developed Countries") +
  xlab("Country") +
  ylab("Predicted Life Expectancy")

#Plot for the developing countries
ggplot(developing, aes( x= Country,y = Predicted_Life_Expectancy)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Predicted Life Expectancy for Developing Countries") +
  xlab("Country") +
  ylab("Predicted Life Expectancy")


#Mean of the predicted life expectancy for each group
mean_predicted_developed <- mean(developed$Predicted_Life_Expectancy)
mean_predicted_developing <- mean(developing$Predicted_Life_Expectancy)

cat("Mean Predicted Life Expectancy for Developed Countries: ", mean_predicted_developed, "\n")
cat("Mean Predicted Life Expectancy for Developing Countries: ", mean_predicted_developing, "\n")

#Compare with the actual means

#Compute the errors
actual <- lifexp14$Life.expectancy
predicted <- lifexp14$Predicted_Life_Expectancy


# Scatter plot of predicted against actual values
#Scatter plot:
#Visualizing the predicted values against the actual values in a scatter plot helps to see
#how well the model fits the data. Ideally, the points should line up along the
#line y = x.

library(ggplot2)
ggplot(data = lifexp14, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Predicted vs Actual Life Expectancy") +
  xlab("Actual Life Expectancy") +
  ylab("Predicted Life Expectancy")

#Compute the MAE
#Mean Absolute Error (MAE):
#It is the mean of the absolute differences between predicted and actual values. A lower value indicates a better prediction.


# Compute the MSE
#Mean Squared Error (MSE):
#It is the mean of the squared differences between predicted and actual values. It penalizes larger errors more.
mse <- mean((actual - predicted)^2)
print(paste("Mean Squared Error (MSE):", mse))

#Compute the RMSE
#Root Mean Squared Error (RMSE):
#It is the square root of the MSE and has the same unit of measurement as the data, which is useful for interpreting the error.
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))
#The RMSE says that the average error of the predictions is about 3.7 years => good

#Compute the residuals
residuals <- actual - predicted

#Plot of the residuals
ggplot(data = lifexp14, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Residuals vs Predicted Life Expectancy") +
  xlab("Predicted Life Expectancy") +
  ylab("Residuals")


#Comparison between the predicted values of "life.expectancy" (mean_predicted_developing and mean_predicted_developed)
#and the actual values, for both developing and developed countries:
mean_predicted_developing
mean(lifexp14$Life.expectancy[lifexp14$Status == "Developing"])

mean_predicted_developed
mean(lifexp14$Life.expectancy[lifexp14$Status == "Developed"])
