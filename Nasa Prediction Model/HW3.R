library(car)
library(graphics)
library (sm)
library(BSDA)
library(tseries)
library(lmtest)
library(MASS)
library(nortest)
library(e1071)
library(SiZer)
library(exactRankTests)
library(lmPerm)
library(effsize)
library(gvlma)
library(caret)
library(DAAG)

## carico il dataset
dati <- read.table("/Users/Alessandra/Desktop/dataset6Nasa.csv", header = TRUE, sep = ";", dec = ",")
dati

## le variabili che utilizzo: Dep = variabile dipendente e Ind2,Ind3,Ind4,Ind5,Ind6,Ind7 le var. indipendenti
Dep<-dati$EffortOreUomo				
Ind2<-dati$Methodology
Ind3<-dati$Complexity
Ind4<-dati$Experience
Ind5<-dati$TotalLines
Ind6<-dati$NewLines
Ind7<-dati$DevelopedLines

## statistiche descrittive dati non normalizzati
summary(Dep)
summary(Ind2)
summary(Ind3)
summary(Ind4)
summary(Ind5)
summary(Ind6)
summary(Ind7)
# deviazione standard dati non normalizzati
sd(Dep)
sd(Ind2)
sd(Ind3)
sd(Ind4)
sd(Ind5)
sd(Ind6)
sd(Ind7)

## plot dei dati non normalizzati
boxplot(dati$EffortOreUomo,dati$Methodology,dati$Complexity,dati$Experience,dati$TotalLines,dati$NewLines,dati$DevelopedLines,
        names=c("Dep","Ind2","Ind3","Ind4","Ind5","Ind6","In7"),
        outline = TRUE, col = "pink")

boxplot(Dep)        
boxplot(Ind2)
boxplot(Ind3)
boxplot(Ind4)
boxplot(Ind5)
boxplot(Ind6)
boxplot(Ind7)


## test per la normalità della distribuzione
## Ipotesi nulla (H0): i dati seguono una distribuzione normale.
## p-value > 0.05: accetto H0
shapiro.test(Dep)      
shapiro.test(Ind2)
shapiro.test(Ind3)
shapiro.test(Ind4)
shapiro.test(Ind5)
shapiro.test(Ind6)
shapiro.test(Ind7)

hist(Dep, main = "Nasa", xlab = "EffortOreUomo")
qqnorm(Dep)
qqline(Dep)

hist(Ind2, main = "Nasa", xlab = "Methodology")
qqnorm(Ind2)
qqline(Ind2)

hist(Ind3, main = "Nasa", xlab = "Complexity")
qqnorm(Ind3)
qqline(Ind3)

hist(Ind4, main = "Nasa", xlab = "Experience")
qqnorm(Ind4)
qqline(Ind4)

hist(Ind5, main = "Nasa", xlab = "TotalLines")
qqnorm(Ind5)
qqline(Ind5)

hist(Ind6, main = "Nasa", xlab = "NewLines")
qqnorm(Ind6)
qqline(Ind6)

hist(Ind7, main = "Nasa", xlab = "DevelopedLines")
qqnorm(Ind7)
qqline(Ind7)


## cerco di avvicinarmi ad una distribuzione normale applicando il log ai dati
dati = log(dati)
dati

## ricarico le variabili
Dep<-dati$EffortOreUomo				
Ind2<-dati$Methodology
Ind3<-dati$Complexity
Ind4<-dati$Experience
Ind5<-dati$TotalLines
Ind6<-dati$NewLines
Ind7<-dati$DevelopedLines

## rifaccio lo shapiro test sui valori normalizzati
shapiro.test(Dep)
shapiro.test(Ind2)
shapiro.test(Ind3)
shapiro.test(Ind4)
shapiro.test(Ind5)
shapiro.test(Ind6)
shapiro.test(Ind7)


hist(Dep, main = "Nasa", xlab = "EffortOreUomo")
qqnorm(Dep)
qqline(Dep)

hist(Ind2, main = "Nasa", xlab = "Methodology")
qqnorm(Ind2)
qqline(Ind2)

hist(Ind3, main = "Nasa", xlab = "Complexity")
qqnorm(Ind3)
qqline(Ind3)

hist(Ind4, main = "Nasa", xlab = "Experience")
qqnorm(Ind4)
qqline(Ind4)

hist(Ind5, main = "Nasa", xlab = "TotalLines")
qqnorm(Ind5)
qqline(Ind5)

hist(Ind6, main = "Nasa", xlab = "NewLines")
qqnorm(Ind6)
qqline(Ind6)

hist(Ind7, main = "Nasa", xlab = "DevelopedLines")
qqnorm(Ind7)
qqline(Ind7)


## statistiche descrittive dati normalizzati
summary(Dep)
summary(Ind2)
summary(Ind3)
summary(Ind4)
summary(Ind5)
summary(Ind6)
summary(Ind7)
# deviazione standard dati normalizzati
sd(Dep)
sd(Ind2)
sd(Ind3)
sd(Ind4)
sd(Ind5)
sd(Ind6)
sd(Ind7)


## plot dei dati normalizzati
boxplot(dati$EffortOreUomo,dati$Methodology,dati$Complexity,dati$Experience,dati$TotalLines,dati$NewLines,dati$DevelopedLines,
        names=c("Dep","Ind2","Ind3","Ind4","Ind5","Ind6","In7"),
        outline = TRUE, col = "pink")

boxplot(Dep)        
boxplot(Ind2)
boxplot(Ind3)
boxplot(Ind4)
boxplot(Ind5)
boxplot(Ind6)
boxplot(Ind7)


# Linear regression assumptions, i.e., 
# - the existence of a linear relationship between the independent variable and the dependent variable (linearity), 
# - the constant variance of the error terms for all the values of the independent variable (homoscedasticity), 
#- the normal distribution of the error terms (normality), 
# - and the statistical independence of the errors, in particular, no correlation between consecutive errors (independence).

## pearson correlation
corre <- cor.test(Ind2, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre
corre <- cor.test(Ind3, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre
corre <- cor.test(Ind4, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre
corre <- cor.test(Ind5, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre
corre <- cor.test(Ind6, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre
corre <- cor.test(Ind7, Dep, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
corre

##verifico se c'è correlazione tra le variabili indipendenti
cor(dati[,2:7],method = "pearson") # ci sta correlazione tra Ind5,Ind6 e Ind7

# test di linearità
sm.regression(Ind2, Dep, h=300,model="linear",test=TRUE, xlab="Methodology", ylab="EffortOreUomo")
text(Ind2, Dep)
sm.regression(Ind3, Dep, h=300,model="linear",test=TRUE, xlab="Complexity", ylab="EffortOreUomo")
text(Ind3, Dep)
sm.regression(Ind4, Dep, h=300,model="linear",test=TRUE, xlab="Experience", ylab="EffortOreUomo")
text(Ind4, Dep)
sm.regression(Ind5, Dep, h=300,model="linear",test=TRUE, xlab="TotalLines", ylab="EffortOreUomo")
text(Ind5, Dep)
sm.regression(Ind6, Dep, h=300,model="linear",test=TRUE, xlab="NewLines", ylab="EffortOreUomo")
text(Ind6, Dep)
sm.regression(Ind7, Dep, h=300,model="linear",test=TRUE, xlab="DevelopedLines", ylab="EffortOreUomo")
text(Ind7, Dep)

# perform Breusch-Pagan test per l'omoschedasticità
## A p-Value > 0.05 indicates that the null hypothesis(the variance is unchanging in the residual) can be rejected and therefore heterscedasticity exists. This can be confirmed by running a global validation of linear model assumptions (gvlma) on the lm object.
bptest(Dep ~ Ind2 + Ind3 + Ind4 + Ind5 + Ind6 + Ind7)


## stepwise linear regression
fm <- lm(data=dati, Dep   ~ Ind2 + Ind3  + Ind4 + Ind5 + Ind6 + Ind7)
summary(fm)

## con gvlma sul modello ho la conferma dell'eteroschedatiscità del mio modello
gvlma(fm)

stepsel <-step(fm, direction="both")
### restituisce la formula con le variabili selezionate
formulas <-  stepsel$call$formula   
formu <- toString(formulas)
cat("Formula: ", formu, "\n")

# verifico la normalità dei residui ottenuti con il modello 
shapiro.test(resid(fm)) ##sono normalmente distribuiti perchè p-value > 0.05

# The Durbin-Watson statistic is used to test for the presence of serial correlation 
# among the residuals. The value of the Durbin-Watson statistic ranges from 0 to 4. 
# As a general rule of thumb, the residuals are not correlated if the Durbin-Watson 
# statistic is approximately 2, and an acceptable range is 1.50 - 2.50. 

## test di Durbin-Watson
dw<-dwtest(fm,data=dati) ##sono correlati
dw

#distanza di cook
cooks.distance(fm)
plot(cooks.distance(fm))
plot(density(cooks.distance(fm)))


## plot dei residui
plot(fitted(fm), resid(fm),
     xlab="Fitted values",
     ylab="Residuals", 
     main="Scatter plot of Residuals",
     type = "n"
)
text(fitted(fm), resid(fm))

############################

# VALIDAZIONE

## k-fold cross validation
# define training control
#k=8 in base a MAE e l'altro valore
train_control <- trainControl(method="cv", number=8)
# train the model
model <- train(EffortOreUomo ~ Methodology + Complexity + Experience + NewLines, 
               data=dati, trControl=train_control, method="lm", na.action = na.pass)
print(model)



## hold out cross validation
set.seed(1)
in_train <- createDataPartition(dati$EffortOreUomo, p = 4/5, list = FALSE)
training <- dati[ in_train,]
testing  <- dati[-in_train,]
nrow(dati)
nrow(training)
nrow(testing)
lda_fit <- train(EffortOreUomo ~ Methodology + Complexity + Experience + NewLines, data = dati, method = "lm")
lda_fit
lda_fit$resample


