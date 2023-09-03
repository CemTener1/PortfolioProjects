#####Data Wrangling
#Introduce Variables
library(knitr)

my_table <- data.frame(
  Name = c("Age", 
           "MaritalStatus", 
           "HHIncome", 
           "Poverty", 
           "HomeOwn", 
           "Work", 
           "Weight", 
           "Height", 
           "BMI", 
           "Pulse",
           "BPSysAve", 
           "Testosterone", 
           "Diabetes", 
           "HealthGen", 
           "Depressed" 
           
  ),
  Type = c("Numerical", "Categorical", "Categorical", "Numerical", "Categorical", "Categorical", "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", "Numerical", "Categorical", "Categorical", "Categorical"),
  Description = c("Age in years at screening of study participant",
                  "Marital status of study participant reported for participants aged 20 years or older", 
                  "Total annual gross income for the household", "A ratio of family income to poverty guidelines Smaller numbers -> more poverty",
                  "Home of study participant or someone in their family is owned, rented or occupied by some other arrangement",
                  "Type of work done by study participant last week Reported for participants aged 16 years or older", 
                  "Weight in kg",
                  "Standing height in cm", 
                  "Body mass index (weight/height2 in kg/m2)", 
                  "60 second pulse rate",
                  "Combined systolic blood pressure reading",
                  "Testerone total (ng/dL)", "Study participant told by a doctor or health professional that they have diabetes",
                  "Self-reported rating of participant’s health in general", 
                  "Self-reported number of days where participant felt down, depressed or hopeless"
  )
)
kable(my_table, align = "c", padding = 5) 

library(dplyr)
library(tidyverse)
library(purrr)
mydata = (read.csv("NHANES.csv"))
#View(mydata)

mydata <-mydata%>%
  mutate(Non_HDL = TotChol - DirectChol,
         Marriage = case_when(MaritalStatus %in% c("Married", "LivePartner")~1, TRUE ~0),
         #Married or Living w/ someone 1, divorced, widowed, or separeted 0
         HighIncome = case_when(HHIncomeMid > 87000~1, TRUE~0),
         MidIncome = case_when(HHIncomeMid< 87000 & HHIncomeMid >55000~1, TRUE~0),
         LowIncome = case_when(HHIncomeMid < 55000~1, TRUE~0),
         #split it into income classes, we can do high or low income(maybe recode after talking to erin)
         HomeOwn = case_when(HomeOwn %in% c("Own")~1, TRUE~0),
         #homeowner 1, rent or other 0
         Employed = case_when(Work %in% c("Working")~1, TRUE~0),
         #If have a job 1, looking or unemployed 0
         Depressed = case_when(Depressed %in% c("None")~0, TRUE~1),
         #if 0 days depressed 0, even several days is counted as depressed(maybe recode after talking to erin),
         Gender = case_when(Gender %in% c("male")~1, TRUE~0),
         #male is 1, female is 0
         Diabetes = case_when(Diabetes %in% c("Yes")~1, TRUE~0),
         Alcohol12PlusYr = case_when(Alcohol12PlusYr %in% c("Yes")~1, TRUE~0),
         SmokeNow = case_when(SmokeNow %in% c("Yes")~1, TRUE~0),
         RegularMarij = case_when(RegularMarij %in% c("Yes")~1, TRUE~0),
         PhysActive = case_when(PhysActive %in% c("Yes")~1, TRUE~0)
  )%>%
  filter(BPDiaAve != 0)%>%
  summarise(Non_HDL,DirectChol, Pulse, Weight, Height, BMI,  BPSysAve, BPDiaAve, Age, Gender, Marriage, HighIncome,MidIncome,LowIncome, HomeOwn, Employed,  Diabetes, Depressed, PhysActive, Alcohol12PlusYr, SmokeNow, RegularMarij)

mydata <- na.omit(mydata)

#####EDA
#Summarize the correlation plot
#install.packages("corrplot")
library(corrplot)
corrplot::corrplot(cor(mydata))

#Sample size was too big, so I got a smaller sample for my plots
df <-sample(1:nrow(mydata), 50)
mydata2 <-mydata[df,]

plot(mydata2[,c(1,2:9)])
#no variable with strong correlation with Non_HDL cholesterol

#####
#Model Selection

#Evaluate the criteria(adj rsquare, cp , aic)
library(leaps)
library(HH)
all = regsubsets(Non_HDL~DirectChol+Pulse+Weight+Height+BMI+BPDiaAve+BPSysAve+Age+factor(Gender)+ factor(Marriage)+ factor(HighIncome)+factor(MidIncome)+factor(LowIncome)+ factor(HomeOwn)+factor(Employed)+ factor(Diabetes)+ factor(Depressed)+ factor(PhysActive)+ factor(Alcohol12PlusYr)+ factor(SmokeNow)+ factor(RegularMarij), nbest = 3 ,data = mydata)
#summaryHH(all)

#evaluate the plot accordingly
#install.packages("olsrr")
library(olsrr)
model = lm(Non_HDL~DirectChol+Pulse+Weight+Height+BMI+BPSysAve+BPDiaAve+Age+factor(Employed)+ factor(Diabetes)+ factor(Alcohol12PlusYr), data = mydata)
k <-ols_step_all_possible(model)
plot(k)

#Best two models are selected as potential models
#562 = DirectChol BPDiaAve Age Diabetes Alcohol12PlusYr	(Model 1)
#1816 = DC-W-Hg-BPD-Ag-E-Db-A1 (Model 2)

#####
#Model Diagnostics

#build model and check conditions-they do not hold
library(MASS)
model1 = lm((Non_HDL)~(DirectChol) + (BPDiaAve)+ (Age) + as.factor(Diabetes)+  as.factor(Alcohol12PlusYr), data = mydata)

plot(model1,1:2)#Condition do not hold-lets try Box-Cox to find a suitable transformation

#Box-Cox Optimization
#do boxcox for model 1
#Box-Cox result = .2222, suggest log transformation
bc = boxcox(model1)
lambda = bc$x[which.max((bc$y))]

#build new model and check conditions-they hold

new_model1 = lm(log(Non_HDL)~(DirectChol) + (BPDiaAve)+ (Age) + as.factor(Diabetes)+  as.factor(Alcohol12PlusYr), data = mydata)
plot(new_model1,1:2)

#repeaet the same for model2- same Box-Cox result, log transformation 
model2 = lm(Non_HDL~DirectChol+Weight+Height+BPDiaAve+ Age+ as.factor(Employed)+ as.factor(Diabetes)+as.factor(Alcohol12PlusYr) , data = mydata)
plot(model2,1:2)

# Diagnostics for Transformed 9 Variable Model- 
new_model2 = model2 = lm(log(Non_HDL)~(DirectChol)+Weight+Height+BPDiaAve+ Age+ as.factor(Employed)+ as.factor(Diabetes)+as.factor(Alcohol12PlusYr) , data = mydata)
plot(new_model2,1:2)#Conditions are satisfied

#Model Selection Criteria for Log(Non-HDL Cholesterol)
#install.packages("olsrr")
library(olsrr)
plot(k)

#summarize the summary output for both models
summary(new_model1)
summary(new_model2)


#####
#Cross-Validation
library(Metrics)
#cross validation on both models, choose the better one
K = 3
CVInd = sample(1:K,dim(mydata)[1],replace = TRUE)

MSPR.k = rep(NA,K)
for(k in 1:K){
  mod = lm(log(Non_HDL)~(DirectChol)+Weight+Height+BPDiaAve+ Age+ as.factor(Employed)+ as.factor(Diabetes)+as.factor(Alcohol12PlusYr), data = mydata[CVInd !=k,])
  preds = predict.lm(mod,mydata[CVInd ==k,],interval = "none")
  MSPR.k[k] = sum((mydata[CVInd==k,]$Non_HDL-preds)^2)/sum(CVInd==k)
}
MSPR = mean(MSPR.k)
#MSPR


#For model 2:
MSPR.k2 = rep(NA,K)
for(k in 1:K){
  mod = lm(log(Non_HDL)~(DirectChol) + (BPDiaAve)+ (Age) + as.factor(Diabetes)+  as.factor(Alcohol12PlusYr), data = mydata[CVInd !=k,])
  preds = predict.lm(mod,mydata[CVInd ==k,],interval = "none")
  MSPR.k2[k] = sum((mydata[CVInd==k,]$Non_HDL-preds)^2)/sum(CVInd==k)
}
cem = mean(MSPR.k2)
#MSPR2

my_table <- data.frame(
  Model = c("5 Variable", "9 Variable"),
  MSPR = c("6.411273","6.422842"),
  Type = c("k-fold","k-fold"),
  K = c(3,3)
  
)
kable(my_table, align = "c", padding = 5) 

#####Use the model 1- lower MSPR and fewer terms(simpler)
#Predictions
#make predictions with the new model-evaluate each and make comparisons 

#Predict the LDL cholesterol for average person
newdata <- data.frame(DirectChol= c(1.365) , BPDiaAve=c(68.29), 
                      Age=c(41.56), Diabetes=c(0),  Alcohol12PlusYr=c(1))

# generate a prediction interval
pred <- predict(new_model1, newdata, interval = "prediction", level = 0.95)
pred

#Confidence interval for an average person(averaged all the variable values)
newdata <- data.frame(DirectChol= c(1.365) , BPDiaAve=c(68.29), 
                      Age=c(41.56), Diabetes=c(0),  Alcohol12PlusYr=c(1))

# generate a prediction interval
pred <- predict(new_model1, newdata, interval = "confidence", level = 0.95)
pred

#How does LDL chol change when BPDiaAve is in Q1 and Q3
newdata <- data.frame(DirectChol= c(1.365) , BPDiaAve=c(61.00,77), 
                      Age=c(41.56), Diabetes=c(0),  Alcohol12PlusYr=c(1))

# generate a confidence interval
pred2 <- predict(new_model1, newdata, interval = "confidence", level = 0.95)
pred2














