# CS112-Assignment2-20181020
#Question1
#Part A & B
#Model1: Positive
x<-c(1:99)
y<-0.08*x+8+rnorm(99,0,0.2)
plot(x,y,
     xlab ="x",ylab="y",pch=20)
model1<-lm(y~x)
summary(model1)

"
Summary result
Call:
  lm(formula = y ~ x)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.97806 -0.16115  0.00739  0.19767  0.80653 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 7.977372   0.063150  126.32   <2e-16 ***
  x           0.060120   0.001097   54.83   <2e-16 ***
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3118 on 97 degrees of freedom
Multiple R-squared:  0.9687,	Adjusted R-squared:  0.9684 
F-statistic:  3006 on 1 and 97 DF,  p-value: < 2.2e-16
"

#Part C
#Model2: Negative
y<-0.08*x+8+rnorm(99,0,0.2)
y2<-c(y,10000)
x2<-c(x,2)
plot(x2,y2,
     xlab="x",ylab="y",pch=15)
model2<-lm(y2~x2)
summary(model2)

"
Call:
lm(formula = y2 ~ x2)

Residuals:
Min     1Q Median     3Q    Max 
-377.0 -235.5  -94.3   47.0 9620.5 

Coefficients:
Estimate Std. Error t value Pr(>|t|)  
(Intercept)  390.808    196.832   1.985   0.0499 *
x2            -5.632      3.435  -1.640   0.1043  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 990.4 on 98 degrees of freedom
Multiple R-squared:  0.0267,	Adjusted R-squared:  0.01677 
F-statistic: 2.688 on 1 and 98 DF,  p-value: 0.1043
"

#Part D:
#Model 1 in red line shows the regression line based on the original 99 points with a positive slope. 
#Model 2 in blue line shows the regression line based on 100 points with a negative slope.
abline(model1,lty=1,col="red")
abline(model2,lty=1,col="blue")

#Part E:
"Question: in less than 3 sentences illustrate the dangers of extrapolation.
Extrapoliation is making estimation beyond the original data range (observation range).
However, through adding a single outlier, the slope and entire regression model may change completely, 
distruppting the accuracy and reliability of the model. 
The danger of extrapolation lies once the data is out of the original data range." 


#Question 2
library(arm)
library(Matching)
data("lalonde")
set.seed(141)
ctrlgroup<-lalonde[which(lalonde$treat==0),]
#The regression model
lm_lalonde<-lm(re78~age+educ+re74+re75+educ*re74+educ*re75+age*re74+age*re75+re74*re75,
               data=ctrlgroup)
summary(lm_lalonde)

#Simulation
simulations<-10000 #times of simulation
sim_lalonde<-sim(lm_lalonde,n.sims=simulations)

#Medians
med_edu <- median(ctrlgroup$educ)
med_re74 <- median(ctrlgroup$re74)
med_re75 <- median(ctrlgroup$re75)

#Building a matrix and store the data
mat1 <- matrix(, nrow = 39, ncol = 3)
prediction_store <- vector()
for (age in c(17:55)){
  Xs <- c(1, age, med_edu, med_re74, med_re75, med_edu*med_re74, med_edu*med_re75, age*med_re74, age*med_re75, med_re74*med_re75) 
  for (i in 1:reps){
    prediction <- sum((simulation@coef[i,])*Xs) + rnorm(1, 0, simulation@sigma[i])
    prediction_store <- c(prediction_store, prediction )
}
  
  mat1[age-16,] <- c(age,  quantile(store_prediction, probs = c(0.025,0.975))) 
  #16:Because the age starts with 17, we need 17 to be the first.
}

#Plot the scatter plot
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(16,56), ylim = c(-15000,-15000),  #smoothing the data
     xlab = "Age", main = "Prediction intervals of estimated re78 holding predictors at their medians",
     ylab = "re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = mat1[age-16, 2],
    x1 = age,
    y1 = mat1[age-16, 3],
    lwd = 3,
    col="black")
}

#90% Quantile
q90_edu <- quantile(ctrlgroup$educ,probs = 0.9)
q90_re74 <- quantile(ctrlgroup$re74,probs = 0.9)
q90_re75 <- quantile(ctrlgroup$re75,probs = 0.9)

#Building a matrix and store the data
mat1 <- matrix(, nrow = 39, ncol = 3)
prediction_store <- vector()
for (age in c(17:55)){
  Xs <- c(1, age, q90_edu, q90_re74, q90_re75, q90_edu*q90_re74, q90_edu*q90_re75, age*q90_re74, age*q90_re75, q90_re74*q90_re75) 
  
for (i in 1:reps){
    prediction <- sum((simulation@coef[i,])*Xs) + rnorm(1, 0, simulation@sigma[i])
    prediction_store <- c(prediction_store, prediction )
  }
  mat1[age-16,] <- c(age,  quantile(store_prediction, probs = c(0.025,0.975))) 
  #16:Because the age starts with 17, we need 17 to be the first.
}

#Plot the scatter plot
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(16,56), ylim = c(-15000,-15000),  #smoothing the data
     xlab = "Age", main = "Prediction intervals of estimated re78 holding predictors at their 90% Quantiles",
     ylab = "re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = mat1[age-16, 2],
    x1 = age,
    y1 = mat1[age-16, 3],
    lwd = 3,
    col="red")
}


#Question 3:
library(foreign)
library(boot)
nsw<-read.dta("http://www.nber.org/~rdehejia/data/nsw.dta")
nsw.ok <- nsw[, -1]
summary(nsw.ok)
#Usually setting working directory setwd("~/Downloads"), but here to make sure external validity
lm1=lm(re78~treat,data=nsw.ok)
lm1$coefficients[2]
a=c()
re78<-nsw.ok$re78
treat<-nsw.ok$treat
index<-c(1:772)
for(i in 1:10000){
  y<-sample(index,size=772, replace=TRUE)
  lm_coef<-lm(re78[y]~treat[y])$coefficients[2]
  a<-append(a,lm_coef)
}
hist(a)
quantile(a,0.025)
#Result
#2.5% 
#-44.77057 
quantile(a,0.975)
#97.5% 
#1887.497 
#Interesting Foundings:When bootstrapped the 200 out of original 772 data set, it yields similar results
#comparing to the analytical method.
#This suggests that bootstrapping is a simple, effective, also relatively reliable method 
#to estimate the population parameters by only using a limited number of data set. 
#200 is a relatively small sample size.


#Question 4:
R2<-function(predict,actual){
  tss<-sum((actual-mean(actual))^2)
  rss<-sum((actual-predict)^2)
  #total sum and residual sum of squares
  rsq<-1-rss/tss
  #r square
}
bootstrap_re78<- sample(nsw.ok$re78,772,replacement<-T)
r_re78<-nsw$re78
example<-R2(predict<-bootstrap_re78,actual=r_re78)


#Question 5:
library(ISLR)
glm.fit<-glm(nsw.ok$treat~nsw.ok$age+nsw.ok$education+nsw.ok$hispanic+nsw.ok$married+nsw.ok$nodegree+nsw.ok$re75)
summary(glm.fit)
coef_fit<-coef(glm.fit)

treatgroup<-nsw[nsw.ok$treat==1,]
ctrlgroup<-nsw[nsw.ok$treat==0,]
#297 data sets in treatment group
vector_treat <- vector()
for(i in 1:297){
  predict_treat<-coef_fit[1]+coef_fit[2]*treatgroup[i,3]+coef_fit[3]*treatgroup[i,4]+
    coef_fit[4]*treatgroup[i,5]+coef_fit[5]*treatgroup[i,6]+coef_fit[6]*treatgroup[i,7]+
    coef_fit[7]*treatgroup[i,8]+coef_fit[8]*treatgroup[i,9]
  vector_treat[i]<-predict_treat
}

ctrl_vector<-vector()
for(i in 1:425){
  ctrl_predict<-coef_fit[1]+coef_fit[2]*ctrlgroup[i,3]+coef_fit[3]*ctrlgroup[i,4]+coef_fit[4]*ctrlgroup[i,5]+
    coef_fit[5]*ctrlgroup[i,6]+coef_fit[6]*ctrlgroup[i,7]+coef_fit[7]*ctrlgroup[i,8]+coef_fit[8]*ctrlgroup[i,9]
  ctrl_vector[i]<-ctrl_predict
}

hist(vector_treat,col="red",xlab = "Treatment Group's Estimated Probabilities",ylab = "Frequency",main = "For Treatment Group")
hist(ctrl_vector,col="blue",xlab = "Control Group's Estimated Probabilities",ylab = "Frequency",main = "For Control Group")



