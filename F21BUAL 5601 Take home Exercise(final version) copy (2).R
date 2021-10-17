##################
### QUESTION 1 ###
##################
# If it is the first time to use "gparis", please install the packages before you start.
install.packages("gpairs")
install.packages("gvlma")
install.packages("car")

library(gpairs)

# Open the data file
# Part 1
summary(Rent_Data)
summary(Rent_Data[,-6])
Rent_Data<-Rent_Data[,-6] # Updating data by removing 6th variable, X.
summary(Rent_Data) # Chcek the updated dataset.
gpairs(Rent_Data) 
cor(Rent_Data[-1,-1])

# Part 2
## 1)
q1.m1<-lm(Rent~Distance.from.Airport, data=Rent_Data)
summary(q1.m1)
# Modify the codes in line 17-18 using two other predictors
# Model for Distance.to...Downtown

# Model for Distance.to.University


## 2)
confint(q1.m1) # Copy this code and paste it. Then change model name for model 2 (Distance.to...Downtown) and model 3 (Distance.to.University)

# Part 4
par(mfrow=c(3,4))
# Copy code below and paste it. Then change model name for model 2 (Distance.to...Downtown) and model 3 (Distance.to.University)
plot(q1.m1)

library(car)
par(mfrow=c(1,3))
influencePlot(q1.m1) # Use this code for creasing plot for model 2 and 3


library(gvlma)
gvlma(q1.m1)
plot(gvlma(q1.m1))
# Use codes for creasing plot for model 2 and 3



##################
### QUESTION 2 ###
##################
install.packages("faraway")
library (faraway)

# Part 1
dev.off() # Clear plots and reset the plot setting
gpairs(teengamb)
cor(teengamb)

# Part 2
teengamb$sex<-factor(teengamb$sex, 
                     level=c(0,1),    
                     labels=c("Male","Female")) 
Q2_1<-lm(gamble~.,data=teengamb)
summary(Q2_1)


# Part 3
plot(gamble~sex, data=teengamb)

# Part 4
Q2_2<-lm(gamble~sex+income+sex*income,data=teengamb)
summary(Q2_2)



##################
### QUESTION 3 ###
##################
library(ISLR)
library(gpairs)

# Part 1
dim(Wage)
str(Wage)
summary(Wage)
gpairs(Wage)

# Part 2
attach(Wage)
plot(age, wage)

# Part 3
Q3_3<-lm(wage~age, data=Wage)
summary(Q3_3)

#Part 4
par(mfrow=c(2,2))
plot(Q3_3)

library(gvlma)
gvlma(Q3_3)

#Part 6
Q3_6_1<-lm(wage~age+I(age^2), data=Wage)
summary(Q3_6_1)
plot(Q3_6_1)
gvlma(Q3_6_1)

Q3_6_2<-lm(wage~age+I(age^2)+I(age^3), data=Wage)
summary(Q3_6_2)
plot(Q3_6_2)
gvlma(Q3_6_2)

Q3_6_3<-lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
summary(Q3_6_3)
plot(Q3_6_3)
gvlma(Q3_6_3)

Q3_6_4<-lm(wage~age+I(age^2)+I(age^3)+I(age^4)+I(age^5), data=Wage)
summary(Q3_6_4)
plot(Q3_6_4)
gvlma(Q3_6_4)

anova(Q3_3,Q3_6_1,Q3_6_2,Q3_6_3,Q3_6_4)
