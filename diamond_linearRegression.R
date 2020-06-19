
#import the data
library(ggplot2)
ddata=diamonds
str(ddata)
# The carata is used for the predition of price 
# prediction is based on the strength of carat
summary(ddata$carat)
# the mean is greater than median so it is a right skewed distribution as confirmed by histogram 
hist(ddata$price)
# matrice de correlation des variables entre elles 
# correlation matrix
cor(ddata[c("depth","table","carat")])
# la plus forte correlation est entre depth et price 
# the strongest correlation is between depth and price
pairs(ddata[c("depth","table","carat")])
# install package psych
library(psych)
pairs.panels(ddata[c("depth", "table", "carat")])
# building the model (linear regression)
diamond_model<-lm(price~x+y+z+depth+table+carat,ddata)
# or 
dat_model<-lm(price~ .,ddata)
summary(dat_model)
# chaque coeff sous le nom de la variable indique l'increment de charge pour une augmentation
# de 1 de la variable ex pour age +1 = 256$ de charges supp 
# under each coefficient you get an indication how much increment of predicted variable correspond increment of 1 of the 
# of the variable influencing prediction
# now we seperate in two part the dataset we take 75% of lines for train, remaining for test
dat_model2<-ddata[1:1100,]
dat_test2<-ddata[1101:1338,]
dat_train2<-dat_model2
dat_model2<-lm(price~x+y+z+depth+table,dat_train2)
dat_pred<-predict(dat_model2,dat_test2)
# we check model performance prediction and charges in test should be highly correlated if model is good
cor(dat_test2$price,dat_pred)
head(dat_pred)
head(ins_test2$charges)
# to compare side by side prediction and real data in test
comparaison<-cbind(dat_test2$price,dat_pred)
dat_model2
