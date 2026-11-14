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


#--------(Gaia) Organizzazione data set--------
lifexp = read.csv('life_expectancy.csv')

#Togliamo le righe che contengono celle vuote
lifexp=na.omit(lifexp)

#Facciamo diventare la variabile categorica di tipo factor
lifexp$Status = factor(lifexp$Status, ordered = F)

#Scegliamo l'anno: 2013 perché è recente e ha tanti dati
lifexp=lifexp[which(lifexp$Year==2013),]

lifexp = lifexp[lifexp$Country != "South Africa", ]

#Rinominiamo le righe
rownames(lifexp)=lifexp[,'Country']
lifexp=lifexp [, c(-1, -2)]

#Verifichiamo le dimensioni
dim(lifexp) #124  16

#Descrizione delle colonne
summary(lifexp)

#Verifichiamo il tipo delle colonne
str(lifexp)


#Visualizziamo i dati

#pairs(lifexp[ , c('Status','Population', 'Hepatitis.B','Measles','Polio','Diphtheria','HIV.AIDS','infant.deaths','under.five.deaths','Total.expenditure','GDP','BMI','thinness..1.19.years','Alcohol','Schooling','Life.expectancy')], pch = 16)

#meglio così:
ggpairs(lifexp[, c('Status','Population','Diphtheria','HIV.AIDS','Total.expenditure','GDP','BMI','thinness..1.19.years','Schooling','Life.expectancy')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))

#Scegliamo life.expectancy come risposta e le altre variabili come predittori




#--------(Soraya) Considerazioni sul p-value--------


#Creiamo il modello di regressione lineare multipla:
mod0 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Measles+Polio+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling, lifexp)
summary(mod0)
#R^2 e R^2 aggiustato sono abbastanza vicini ad 1 => il modello spiega bene la variabile "Life.expectancy"


#Poliomielite ha un p-value (Pr(>|t|)) alto quindi la sua significatività è bassa, rifacciamo il modello senza
mod1 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Measles+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling , lifexp)
summary(mod1)

#Togliamo anche Measles
mod2 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Alcohol+Schooling , lifexp)
summary(mod2)

#Togliamo anche Alcohol
mod3 = lm(Life.expectancy ~ Status+Population+Hepatitis.B+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod3)

#Togliamo anche Hepatitis.B
mod = lm(Life.expectancy ~ Status+Population+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod)


#Matrice varianza
vcov(mod)

#Matrice disegno
X = model.matrix(mod)
X


#--------AIC, BIC e R^2 aggiustato--------


#The Akaike information criterion (AIC) is an estimator of out-of-sample prediction error and thereby relative quality of statistical models for a given set of data. Given a set of candidate models for the data, the preferred model is the one with the minimum AIC value. 
#Thus, AIC rewards goodness of fit (as assessed by the likelihood function), but it also includes a penalty that is an increasing function of the number of estimated parameters.
#AIC = −2 · log( likelihood ) + 2r where r is the number of parameters in the model.
AIC(mod0)
AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(mod)
#Anche solo togliendo i dati con p-value alto l'AIC è diminuito, il che è buono


#BIC
#The formula for the Bayesian information criterion (BIC) is similar to the formula for AIC, but with a different penalty for the number of parameters. 
#The BIC generally penalizes free parameters more strongly than the Akaike information criterion, though it depends on the size of n and relative magnitude of n and r. It works well when n >> r.
#BIC = −2 · log( likelihood ) + r · log(n) 8
BIC(mod0)
BIC(mod1)
BIC(mod2)
BIC(mod3)


#Con questo scopriamo chi togliere a partire da mod0 tramite AIC:
step(mod0, direction = "backward" , trace = T)
#Partendo da mod0, rimuove ogni volta la variabile che abbassa di più l'AIC in modo da avere alla fine il modello con l'AIC più basso possibile.
#"trace=TRUE" permette di visualizzare ogni step
#Secondo l'AIC il modello migliore è come il nostro "mod" (dopo aver tolto le variabili con p-value alto)
#ma senza infant.deaths e con Hepatitis.B (più o meno sono simili)

#Stessa cosa ma con il BIC:
n = dim(lifexp)[1]
mod0_BIC_back = step(mod0, direction = "backward", k = log(n), trace = T)
#BIC toglie troppe variabili => modello troppo semplice, spiega poco

#Usando invece R^2 adjusted:
x = model.matrix(mod0) [ , -1 ] 
y = lifexp$Life.expectancy
adjr = leaps(x, y, method = "adjr2") 
names(adjr)
adjr
bestmodel_adjr2_ind = which.max(adjr$adjr2)
adjr$which[bestmodel_adjr2_ind,]

#Modello migliore per r^2 adj è senza Polio, infant.deaths, under.five.deaths e alcohol, cioè:
#Status+Population+Hepatitis.B+Measles+Diphtheria+HIV.AIDS+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling


#--------Scelta iniziale del modello--------


#Modello scelto per ora: quello ottenuto dopo aver tolto i valori con P-value alto,
#perché ha comunque un AIC più basso del modello iniziale e un R^2 aggiustato abbastanza alto
#(cerchiamo un buon compromesso tra previsione e interpretazione dei dati, vogliamo dati che siano anche significativi)
mod = lm(Life.expectancy ~ Status+Population+Diphtheria+HIV.AIDS+infant.deaths+under.five.deaths+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling , lifexp)
summary(mod)



#--------(Alice) Collinearità--------


#VIF (Variance Inflation Factor): se vif > 5 => alta probabilità di collinearità (=> le variabili sono troppo simili tra loro)
vif(mod)

# StatusDeveloping           Population           Diphtheria           HIV.AIDS          infant.deaths 
# 1.533178                   1.885529             1.380966               1.311809        231.732048 
# under.five.deaths    Total.expenditure          GDP                  BMI               thinness..1.19.years 
# 221.038155                 1.264204             1.316815             1.968811          2.464902 
# Schooling 
# 2.598384 

#Si nota collinearità tra infant.deaths  e  under.five.deaths quindi dobbiamo toglierne uno
modn=update(mod,.~.-under.five.deaths)
summary(modn)

#infant.deaths ha p-value=0.2 => togliamo
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


#--------Omoschedasticità--------
#Una sequenza di variabili casuali è omoschedastica se tutte le sue variabili casuali hanno la stessa varianza finita.


plot(modn$fit, modn$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = 'red')

#oppure
plot(modn, which=1)

#Dal grafico dei residui rispetto ai valori predetti si osserva una dispersione casuale e simmetrica dei punti attorno
#alla linea orizzontale a zero, senza la presenza di pattern particolari. Questo supporta le ipotesi di linearità,
#omoschedasticità e indipendenza dei residui. Il modello appare quindi adeguato rispetto a queste assunzioni fondamentali
#della regressione lineare.


#--------Normalità--------


modn_res = modn$residuals/summary(modn)$sigma
plot(modn$fitted, modn_res, xlab = 'Fitted values', ylab = 'Standarzized residuals')
qqnorm(modn$residuals)
qqline(modn$residuals, col = 'blue')

#Se p-value è alto non rifiutiamo H0 => dati normali
shapiro.test(modn$res)
#p-value = 0.04227 (< 0.05, bisogna alzarlo con una Box-Cox)

#Histogram of residuals and boxplot are useful tools to look at the shape of the residual distribution and to
#see if the tails of the distributions are souspiciously heavy (i.e., there are many outliers) respectively.
hist(mod$res, 10, probability = TRUE, col = 'lavender', main = 'residuals')

#Residui
boxplot(mod$res, main = "Boxplot of savings residuals", pch = 16, col = 'lavender')


#--------BOXCOX--------


#Eseguiamo la trasformazione Box-Cox
par(mfrow = c( 1, 1 )) 
bc <- boxcox(modn, plotit = TRUE)

#Troviamo il valore di lambda che massimizza la log-verosimiglianza
best_lambda_ind = which.max(bc$y) 
best_lambda = bc$x[best_lambda_ind] #1.88
best_lambda=2

#Ora abbiamo un nuovo modello:
modb = lm(((Life.expectancy ^ best_lambda) - 1)/best_lambda ~ Status+Population+Diphtheria+HIV.AIDS+Total.expenditure+GDP+BMI+thinness..1.19.years+Schooling ,data = lifexp)
summary(modb)
#Ci sono variabili con p-value > 0.1 ma di poco, valutiamo prima di toglierle

#Plot dei residui standardizzati
modb_res = modb$residuals/summary(modb)$sigma
plot(modb$fitted, modb_res, xlab = 'Fitted values', ylab = 'Standarzized residuals')
qqnorm(modb$residuals)
qqline(modb$residuals, col = 'blue')

#Se il p-value è alto non rifiutiamo H0 => dati normali
shapiro.test(modb$res)
#p-value = 0.4867 > 0.05, ok

#Il p-value è aumentato da 0.04227 a 0.4867 quindi la trasformazione è stata efficace => i dati ora sono normali


#--------R^2 aggiustato--------


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

#Secondo l'R^2 aggiustato è un buon modello

#Matrice disegno:
X = model.matrix(modb)



#--------(Assunta) Punti di leva--------


lev = hatvalues(modb) #uguale con il nome

#r numero di covariate
p=modb$rank  #rango di X (covariate più intercetta)=r+1
n = dim(lifexp)[1]

#Un'osservazione è una leva se hii>2*p/n

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

#Rimuoviamo le leve dal modello:
modbl = update(modb,.~., subset = (lev < 2*p/n)) 
summary(modbl)
#alcuni p-value sono diventati > 0.1. valutiamo invece cosa succede con i residui e distanza di Cook

abs((modb$coefficients - modbl$coefficients)/modb$coefficients)
#Questo dice quanto sono cambiati in percentuale i beta


#--------Residui standardizzati--------


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

# Residui standardizzati
plot(modb$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals") 
abline(h = c(-2,2), lty = 2, col = 'orange')
points(modb$fitted.values[watchout_ids_rstd],res_std[watchout_ids_rstd], col = 'red', pch = 16) 
points(modb$fitted.values[watchout_ids_lev],res_std[watchout_ids_lev], col = 'orange', pch = 16) 
legend('topright', col = c('red','orange'),c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n')          


#--------Residui studentizzati--------


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

#Gli std residuals coincidono con alcuni student
abline(h = c(-2,2), lty = 2, col = 'orange') 
legend('topright', col = c('pink','orange'),c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n')


#--------Distanza di Cook--------
#Unisce i punti di leva e i residui standardizzati


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




#--------(Francesca) MODELLO FINALE (basato sulla distanza di Cook)--------
#Aggiorniamo quindi il modello col criterio della distanza di Cook


mod_finale = update(modb, .~., subset = (Cdist < 4/(n-p))) 
summary(mod_finale)

#Togliamo anche thinness..1.19.years e GDP che hanno p-value > 0.1
mod_finale = update(mod_finale, .~.-thinness..1.19.years-GDP, subset = (Cdist < 4/(n-p))) 
summary(mod_finale)

#Normalità
shapiro.test(mod_finale$res)

#Omoschedasticità
plot(mod_finale$fit, mod_finale$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = 'red')
#oppure
plot(mod_finale, which=1)
#ggpairs(lifexp[ , c('Status','Population','Diphtheria','HIV.AIDS','Total.expenditure', 'BMI', 'Schooling')], title ="Relationships between predictors & response", lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))


#--------Predizione su un altro anno--------


lifexp14 = read.csv('life_expectancy.csv')

#Togliamo le righe che contengono celle vuote
lifexp14=na.omit(lifexp14)

#Rendiamo la categorica una factor
lifexp14$Status = factor(lifexp14$Status, ordered = F)

#Scegliamo l'anno
lifexp14=lifexp14[which(lifexp14$Year==2000),] 

#Rinominiamo le righe
rownames(lifexp14)=lifexp14[,'Country']
lifexp14=lifexp14 [, c(-2)]

lifexp14 = lifexp14[lifexp14$Country != "South Africa", ]

predictions <- predict(mod_finale, newdata = lifexp14)
print(predictions)
y=((2*predictions)+1)^(0.5)
y
#y stampa le previsioni di life.expectancy per ogni paese nel 2000


#Aggiungiamo le previsioni ai dati originali (visualizzabile nella tabella "lifeexp14 nell'environment)
lifexp14$Predicted_Life_Expectancy <- y
#Rimuoviamo le righe con celle vuote
lifexp14=na.omit(lifexp14)

#Dividiamo i risultati in due data frame separati: uno per i paesi sviluppati e uno per quelli in via di sviluppo
developed <- subset(lifexp14, Status == "Developed")
developing <- subset(lifexp14, Status == "Developing")
developing=na.omit(developing)
#Vedi le tabelle "developed" e "developing" nell'environment


#Verifichiamo i primi risultati per i paesi sviluppati
head(developed)

#Verifichiamo i primi risultati per i paesi in via di sviluppo
head(developing)

library(ggplot2)


#Plot per i paesi sviluppati
ggplot(developed, aes(x = Country, y = Predicted_Life_Expectancy)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Predicted Life Expectancy for Developed Countries") +
  xlab("Country") +
  ylab("Predicted Life Expectancy")

#Plot per i paesi in via di sviluppo
ggplot(developing, aes( x= Country,y = Predicted_Life_Expectancy)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Predicted Life Expectancy for Developing Countries") +
  xlab("Country") +
  ylab("Predicted Life Expectancy")


#Calcoliamo la media delle aspettative di vita previste per ciascun gruppo
mean_predicted_developed <- mean(developed$Predicted_Life_Expectancy)
mean_predicted_developing <- mean(developing$Predicted_Life_Expectancy)

cat("Mean Predicted Life Expectancy for Developed Countries: ", mean_predicted_developed, "\n")
cat("Mean Predicted Life Expectancy for Developing Countries: ", mean_predicted_developing, "\n")

#Confrontiamo con le medie effettive

#Calcoliamo gli errori
actual <- lifexp14$Life.expectancy
predicted <- lifexp14$Predicted_Life_Expectancy


# Scatter plot dei valori previsti contro quelli effettivi
#Grafico di dispersione (Scatter Plot):
#Visualizzare i valori previsti contro i valori effettivi in un grafico di dispersione può aiutarti 
#a vedere quanto bene il modello si adatta ai dati. Idealmente, i punti dovrebbero allinearsi lungo la 
#linea y = x.

library(ggplot2)
ggplot(data = lifexp14, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Predicted vs Actual Life Expectancy") +
  xlab("Actual Life Expectancy") +
  ylab("Predicted Life Expectancy")

#Calcoliamo l'MAE
#Errore assoluto medio (Mean Absolute Error, MAE):
#È la media delle differenze assolute tra i valori previsti e quelli effettivi. Un valore più basso indica una migliore previsione.


# Calcoliamo l'MSE
#Errore quadratico medio (Mean Squared Error, MSE):
#È la media dei quadrati delle differenze tra i valori previsti e quelli effettivi. Penalizza maggiormente gli errori più grandi.
mse <- mean((actual - predicted)^2)
print(paste("Mean Squared Error (MSE):", mse))

#Calcoliamo l'RMSE
#Radice dell'errore quadratico medio (Root Mean Squared Error, RMSE):
#È la radice quadrata dell'MSE e ha la stessa unità di misura dei dati, il che può essere utile per interpretare l'errore.
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE):", rmse))
#l'RMSE ci dice che l'errore medio sulle previsioni è di circa 3.7 anni => buono

#Calcoliamo i residui
residuals <- actual - predicted

#Grafico dei residui
ggplot(data = lifexp14, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Residuals vs Predicted Life Expectancy") +
  xlab("Predicted Life Expectancy") +
  ylab("Residuals")


#Confronto tra valori predetti di "life.expectancy" (mean_predicted_developing e mean_predicted_developed)
#e valori effettivi, sia per i paesi in via di sviluppo che per quelli sviluppati:
mean_predicted_developing
mean(lifexp14$Life.expectancy[lifexp14$Status == "Developing"])

mean_predicted_developed
mean(lifexp14$Life.expectancy[lifexp14$Status == "Developed"])

