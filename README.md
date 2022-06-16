# Bicycle-crash-analysis-code
Code for analysing crashes in the city of Munich
#####################################################################################
#Crash PRediction and Data Analysis
#-----------------------------------------------------------------------------------#
#####################################################################################

#-----------------------------------------------------------------------------------#
# Install and load packages
-----------------------------------------------------------------------------------#
  
library(AER)
library("parameters")
library("lm.beta")
library(lmtest)
library(mlogit)
library(questionr)
library(reghelper)
library(corrplot)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyverse)
library(pscl)
library(boot)
library(Hmisc)

#-----------------------------------------------------------------------------------#
# Read data
#-----------------------------------------------------------------------------------#


data <- read.csv("data.csv") #.csv file with all the parameters and crash data
summary(data)


#-----------------------------------------------------------------------------------#
# Check for variable correlations
#-----------------------------------------------------------------------------------#


a <- cor(data, method = c("pearson"))
write.csv((a), "myformattedcortable.csv") #save the correlation table

#Make dataframe for the dataset
df<-data.frame(data)


#specific pair plots for all variables
pairs(~ crashpoint+signal+tram+busstop+railstop+crossing+junction
      +education+biergarten+supermarkt+bicycle+primary+secondary
      +resident+footway+path+Weighted_Average.Speed, data = data)


##--------------------------Histogram of data-------------------------##


#Histogram of individual variables
hist(data$Weighted_Average.Speed, main = paste("Histogram of Weighted Average Speed"), ylim = c(0, 3000), xlab = "Weighted Average Speed")
hist(data$crashpoint, main = paste("Histogram of attribute Crashpoint"),ylim = c(0, 7000), xlab = "Number of Crashes")
hist(data$signal, main = paste("Histogram of Signal count"), xlab = "Signal count")
hist(data$tram, main = paste("Histogram of Tram count"), xlab = "Tram count")
hist(data$busstop, main = paste("Histogram of Busstop Count"), xlab = "Busstop Count")
hist(data$railstop, main = paste("Histogram of Railstop count"), xlab = "Railstop count")
hist(data$crossing, main = paste("Histogram of Crossing count"), xlab = "Crossing coun")
hist(data$junction, main = paste("Histogram of Junction count"), xlab = "Junction count")
hist(data$education, main = paste("Histogram of Education institute count"), xlab = "Education institute count")
hist(data$biergarten, main = paste("Histogram of Biergarden count"), xlab = "Biergarden count")
hist(data$supermarkt, main = paste("Histogram of Supermarket count"), xlab = "Supermarket count")
hist(data$bicycle, main = paste("Histogram of Bicycle street length"), xlab = "Bicycle street length")
hist(data$primary, main = paste("Histogram of Primary street length"), xlab = "Primary street length")
hist(data$secondary, main = paste("Histogram of Secondary street length"), xlab = "Secondary street length")
hist(data$tertiary, main = paste("Histogram of Tertiary street length"), xlab = "Tertiary street length")
hist(data$resident, main = paste("Histogram of Residential street length"), xlab = "Residential street length")
hist(data$footway, main = paste("Histogram of Footpath length"), xlab = "Footpath length")
hist(data$path, main = paste("Histogram of Unspecified street length"), xlab = "Unspecified street length")


#####---------Find max values-------###
df[which.max(df$Weighted_Average.Speed),]
df[which.max(df$crashpoint),]
df[which.max(df1$signal),]
df[which.max(df1$educatio),]


#-----------------------------------------------------------------------------------#
# Multiple Linear Regression
#-----------------------------------------------------------------------------------#
#Regular
data1.lm <-lm(crashpoint ~ signal+tram+busstop+railstop+crossing+junction
              +educatio+biergart+supermar+bicycle+primary+secondar+tertiary+resident+footway+path+
                Weighted_Average.Speed, data = data2)
summary(data1.lm)

dwtest(data1.lm)
#2nd iteration
data1.lm <-lm(crashpoint ~ signal+tram+busstop+railstop+crossing
              +educatio+biergart+supermar+primary+secondar+resident+
                Weighted_Average.Speed, data = data2)
summary(data1.lm)


####
lm.beta(data1.lm) 
summary(data1.lm)$coefficient
confint(data1.lm)
alias(data1.lm)

##LogLM
data2.lm <-glm(crashpoint ~ signal+tram+busstop+railstop+crossing+junction
               +educatio+biergart+supermar+bicycle+primary+secondar+resident+footway+path+
                 X20_Weighted_Average.Speed, family = gaussian(link = "log"), data = data1)
summary(data2.lm)


#predict
pred <- read.csv("prediction attributes.csv")

write.csv(predict(data1.lm, newdata = pred), file = "predictions_lm.csv")

#Plot
#lmplot <- read.csv("predictions_lm.csv")
data_mod <- data.frame(Predicted = predict(data1.lm),  # Create data for ggplot2
                       Observed = data2$crashpoint)

ggplot(data_mod,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)


########################################################################################
# Poisson Model: The condition assumes that mean = Variance to avoid any dispersion in data, so lets check that
########################################################################################


library(arm)
mean(data2$crashpoint)
var(data2$crashpoint)

m1<- glm(formula = crashpoint ~ signal+tram+busstop+railstop+crossing+junction
         +educatio+biergart+supermar+bicycle+primary+secondar+tertiary
         +resident+footway+path+Weighted_Average.Speed, data = data, family = poisson)
summary(m1)

exp(coef(m1))



#R^2 = 1 - (Residual Deviance/Null Deviance)

m2<- glm(formula = crashpoint ~ signal+tram++railstop
         +educatio+biergart+supermar+bicycle+primary+secondar+tertiary
         +resident+footway+path+Weighted_Average.Speed, data = data2, family = poisson)
summary(m2)
lm.beta(m2)
#R^2 = 0.60
dispersiontest(m2)
with(summary(m2), 1 - deviance/null.deviance)

#Plot
#lmplot <- read.csv("predictions_lm.csv")
data_mod <- data.frame(Predicted = predict(m2),  # Create data for ggplot2
                       Observed = data2$crashpoint)

ggplot(data_mod,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)



###### If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists. 
###########This means that the estimates are correct, but the standard errors (standard deviation) 
################are wrong and unaccounted for by the model

c1 = coef(m2)
s1 = se.coef(m2)
c2 = coef(m3)
s2 = se.coef(m3)
expo = exp(c1)
cbind(c1,s1,c2,s2,expo)



#predict
predm2 <- read.csv("prediction attributes.csv")
write.csv(predict(m2, newdata = predm2), file = "predictions_Poisson.csv")


###########Model for ZINFL Comparison############

m361 <- glm(crashpoint ~ signal+tram+busstop+railstop+crossing+junction
            +educatio+biergart+supermar+bicycle+primary+secondar+tertiary+resident+footway+path+
              Weighted_Average.Speed 
            , data = data2, family = poisson)
summary(m361)



########################################################################################
#######################     Negative Binomial Model
########################################################################################

m6 <- glm.nb(crashpoint ~ signal+tram+busstop+railstop+crossing+junction
             +educatio+biergart+supermar+bicycle+primary+secondar+tertiary+resident+footway+path+
               Weighted_Average.Speed, data = data)
summary(m7)

##Final mode
m7 <- glm.nb(crashpoint ~ signal+tram+railstop+
               +educatio+biergart+supermar+bicycle+primary+secondar+tertiary+resident+footway+
               Weighted_Average.Speed, data = data)
summary(m7)

check_overdispersion(m7)

#0.6196

m7 <- glm.nb(crashpoint ~ signal+tram+busstop+railstop+crossing+junction
             +educatio+biergart+supermar+bicycle+primary+secondar+resident+footway+path+
               Weighted_Average.Speed, data = data)
summary(m7)
#0.6192

##DIspersion Statistic
E2 <- resid(m7, type = "pearson")
N  <- nrow(data1)
p  <- length(coef(m7)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p)##Closer this value is to 1, the better


#predict
#pred <- read.csv("2020_Attributes - Copy.csv")
pred <- read.csv("2020_Attributes - pred.csv")
write.csv(predict(m7, newdata = pred), file = "predictions_negbin.csv")




##########################################################################################
############################NewZIPNegative Binomial
##########################################################################################

#ZeroInflated NegBin models are good for models if Vairance is bigger than Mean
mean(data$crashpoint)
var(data1$crashpoint)

m61 <- zeroinfl(crashpoint ~ signal+tram+railstop
                +educatio+biergart+supermar+bicycle+primary+secondar
                +Weighted_Average.Speed, data = data2, dist = "negbin") #Works best
summary(m61)



##Adding Weighted_Average.Speed to the zero inflation component, adds negative sign to the coefficient
summary(m62)
summary(m62)$coefficient
listcoef(m62)


m63 <- zeroinfl(crashpoint ~ signal+tram+railstop
                +educatio+biergart+supermar+bicycle+primary+secondar+resident+
                  Weighted_Average.Speed|signal+tram+railstop
                +educatio+biergart+supermar+primary+secondar, data = data2, dist = "negbin")
summary(m63)

lmtest::lrtest(m62, m63)


expCoef <- exp(coef((m63)))
expCoef


##DIspersion Statistic
E2 <- resid(m63, type = "pearson")
N  <- nrow(data2)
p  <- length(coef(m63)) + 1 # '+1' is due to theta
sum(E2^2) / (N - p)##Closer this value is to 1, the better
#0.968



#predict

pred1 <- read.csv("2020_Attributes - pred.csv")
a <- data.frame((predict(m63, newdata = pred1)))
write.csv(predict(m63, newdata = pred1), file = "predictions_zinfl_v1.csv")
summary(a)



#Chhose the model with lower value 
AIC(data1.lm, m2, m7, m63)
BIC(data1.lm, m2, m7, m63)

###Vuong test
vuong(m7, m63)

summary(m7)
summary(m63)
#Likelihoodratio test NB and ZINB

2*(-7242- -7274.09)
pchisq(64.18, 9, lower.tail = FALSE)



#Coefficient Comparison

c1 = coef(data1.lm) #LinearRegression
c2 = coef(m361)#Poisson
c3 = coef(m7)#NegativeBinomial
c4 = coef(m62)#ZeroInflated
cbind(c1,c2,c3)


#Plot
data_mod <- data.frame(Predicted = predict(m62),  # Create data for ggplot2
                       Observed = data2$crashpoint)

ggplot(data_mod,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
citation(package = "MASS")


####MODELLING THE SOLUTIONS$#######################

#Carefreezone in the inner districts
sol <- read.csv("2020_Attributes - solutions.csv")
write.csv(predict(m63, newdata = sol), file = "solutions_carfreezone.csv")


##########################################--------------------------SPATIAL REGRESSION----------------------------------########################################


library(texreg)
library(spdep)
library(rgdal)
library(rgeos)
library(sf)
library(tmap)
library(spatialreg)


shp_name <- "SpReg_final.shp" #Load shape file with all attributes
spdata <- st_read(shp_name)
summary(spdata)



summary(spdata)

hist(spdata$Wt_Speed, main = paste("Histogram of Crashpoint"),ylim = c(0, 1000), xlab = "Crashpoints")


###Plot the shp file####

tm_shape(spdata) + 
  tm_fill("Avg_crashp", title = "CrashGrid", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Average CrashPoints for the years 2016-2019", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8)

#make weightmatrix
queen.nb = poly2nb(spdata)
rook.nb = poly2nb(spdata, queen=FALSE)

queen.listw = nb2listw(queen.nb)
rook.listw = nb2listw(rook.nb)
listw1 = queen.listw
#listw2 = rook.listw

#define Regression

reg.eq1 = Avg_crashp ~ Wt_Speed+Signal+Tram+Busstop+Railstop+Cossings+Junction+Education+Biergarten+Supermarkt+Bicycle+Primary+Secondary+Tertiary+Resident+Footway+Path

#Run OLS
reg1 = lm(reg.eq1, data=spdata)
summary(reg1)


lm.morantest(reg1, listw1)
#lm.morantest(reg1, listw2)


##Test to see which model gives better output
lm.LMtests(reg1, listw1, test="all") #LMerr = spatial error, LMlag=Spatial Lagmodel, RLMerr Robust eror model, 

options(scipen=15)

lm.LMtests(reg1, listw1, test="LMerr", zero.policy = T) # spatial error
lm.LMtests(reg1, listw1, test="LMlag", zero.policy = T) # spatial lag model

#Pick models with lower P values

#Run Spatial Model

####Model1 SLX
#p=rho, T=theta, and L=lambda
#SLX Spatially Lagged X model X y=XB+WXT+e
reg2 = lmSLX(reg.eq, data=spdata, listw1)
summary(reg2)

##Export regression summary
#htmlreg(reg3, file="spreg3.docx")


#Compare Direct and Indirect effect(lag variables)
impacts(reg2, listw=listw1)

summary(impacts(reg2, listw=listw1, R=500), zstats = TRUE)
#R is repitions, usually ignored in SLX model
#Done to understand the p values of total effects, since some of direct and indirect effects might have different significance

##TRY MANUAL MODEL TOO###



##SAR SPATIAL LAG
reg3eq = Avg_crashp ~ Wt_Speed+Signal+Railstop+Cossings+Education+Biergarten+Supermarkt+Primary+Secondary+Footway


reg3 = lagsarlm(reg3eq, data = spdata, listw1)
summary(reg3)

impacts(reg3, listw=listw1)
summary(impacts(reg3, listw=listw1, R=500), zstats = TRUE)

#predict
pred <- read.csv("2020_att_sol.csv")
preds <- predict(reg3, newdata=pred, listw=listw1)
write.csv(preds, file = "predictions_Splag.csv")

###SPATIAL EORROR MODEL
reg4eq = Avg_crashp ~ Signal+Railstop+Cossings+Education+Biergarten+Supermarkt+Primary+Secondary+Resident+Wt_Speed

#reg4 = errorsarlm(reg.eq1, data=spdata, listw1)
reg4 = errorsarlm(reg4eq, data=spdata, listw1)
summary(reg4)

#predict
pred <- read.csv("2020_Attributes - sperrpred.csv")
preds <- predict(reg4, newdata=pred, listw=listw1)
write.csv(predict(reg4, newdata = pred), file = "predictions_SpError.csv")


##Spatial HAusmann Test
Hausman.test(reg4)

AIC(reg3, reg4)
