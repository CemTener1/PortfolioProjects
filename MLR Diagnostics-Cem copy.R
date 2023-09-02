#Create parameters
beta0 = 1.5
beta1 = 3
beta2 = 5
beta3 = 1.5
sigma2 = 5
N = 100 #reps
x1min = 0  # lower endpoint of X1 range
x1max = 20  # upper endpoint of X1 range
x2min = 0 # lower endpoint of X2 range
x2max = 10 # upper endpoint of X2 range

# set seed for reproducibility
set.seed(2000)

## ------- Create a MLR model with interaction ----------------------

X1 = runif(N, x1min, x1max)
X2 = runif(N, x2min, x2max)
Y = beta0 + beta1 * X1 + beta2 * X2 + beta3*X1*X2 + rnorm(N,0,sqrt(sigma2))

#Plot our data-realize the presence of interaction
plot(Y~X1+X2+X1*X2, col="lightgrey")

#How to add variables based on residual plots?
resids1 = resid(lm(Y~X1))
plot(resids1~X2, main = "Residuals vs X2") 
#plot indicates we need X2 in our model because there is a linear relationship
#between model residuals and X2

resids2 = resid(lm(Y~X1+X2))
interaction = X1*X2
plot(resids2~interaction, main = "Residuals vs X1*X2") 

#Still we do not see "random noise", so we should add the interaction

resids_full_model =resid(lm(Y~X1+X2+X1*X2))
plot(resids_full_model~fitted.values(lm(Y~X1+X2+X1*X2)), main = "Residuals vs X1*X2")

## ------- Diving into Multicollinearity ----------------------
#Definition: one predictor variable in a multiple regression model can be linearly predicted 
#from the others with a substantial degree of accuracy.
#Why is it a problem? Because it undermines the statistical 
#significance of an independent variable.

library(GGally)
library(MASS)
library(car)
library(carData)

#Create correlated data
sigma<-rbind(c(1,-0.8,-0.7), c(-0.8,1, 0.9), c(-0.7,0.9,1))
sigma
#The correlation of V1 vs V2 is around -0.8, the correlation of V1 vs V3
#is around -0.7 and the correlation of V2 vs V3 is around 0.9
# create the mean vector
mu<-c(10, 5, 2) 
# generate the multivariate normal distribution
df<-as.data.frame(mvrnorm(n=1000, mu=mu, Sigma=sigma))
#Make a matrix of plots with a given data set
ggpairs(df)

#VIF Formula: 1/(1-R^2)

Y_new = beta0 + beta1*df$V1+ beta2*df$V2+ beta3*df$V3 + rnorm(N,0,sqrt(sigma2))
mod = lm(Y_new~df$V1+df$V2+df$V3)
summary(mod)
vif(mod) #values greater than 5 addresses multicollinearity 

#Solution 1: Remove the highly correlated variables - not preferred because you are losing data
model = lm(Y_new~df$V1+df$V3)
summary(model)
vif(model)

#Solution 2: Create a new variable by adding the two correlated variables
V4 = df$V2+df$V3
model2 = lm(Y_new~df$V1 + V4)
summary(model2)
vif(model2)

#####
#How does the regression plane affected with the presence of an interaction term

#3d scatterplots
library(plotly)

#create the 3d space and place the observations
mod = lm(Y~X1+X2)

#Create a 3d space and place observations
plot_ly(z = ~Y, x = ~X1, y = ~X2, opacity = 0.5) %>%
  add_markers()

#define the ranges of the parameters
x <- seq(0, 20)
y <- seq(0, 20)

#define the plane
plane <- outer(x, y, function(a, b){mod$coef[1] + 
    mod$coef[2]*a + mod$coef[3]*b})


#place the plane on the 3d space
plot_ly(z = ~Y, x = ~X1, y = ~X2, opacity = 0.5) %>%
  add_markers() %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE)

#Regression plane get curved with the addition of the interaction term. Lets demonstrate
library(reshape2)

mod_interaction = lm(Y~X1*X2)

x = seq(0, 20)  # generates a sequence of numbers in the ranges of X1
y = seq(0, 20) # generates a sequence of numbers in the ranges of X2

zsurface = expand.grid(X1 = x, X2 = y, KEEP.OUT.ATTRS = F)  #generates a surface map of predicted 
zsurface$z = predict.lm(mod_interaction, newdata = zsurface) # Y values for each X1 and X2 for their ranges
zsurface = acast(zsurface, X2 ~ X1)                       

plot_ly(z = ~Y, x = ~X1, y = ~X2, opacity = 0.5) %>%
  add_markers() %>%
  add_trace(x = ~x, y = ~y, z = ~zsurface, type = "surface")  # plots 3d space with all data and surface map

#The interaction term bring non-linearity to the model. That's why the regression plane gets curved.



