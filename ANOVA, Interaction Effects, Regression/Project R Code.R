# Load all the required packages
library(moments)
library(stats)
library(fdrtool)
library(DoE.base)
library(car)
library(dae)
library(effects)
library(corrplot)

# Read the data
diabetes_data<-read.csv("C:/Users/subha/Desktop/ANLY510/County Level Diabetes Data.csv")

# Identify median
quantile(diabetes_data$income, c(0.25, 0.50, 0.75))

quantile(diabetes_data$povpct, c(0.25, 0.50, 0.75))

quantile(diabetes_data$unemploy, c(0.25, 0.50, 0.75))

quantile(diabetes_data$perc_65up, c(0.25, 0.50, 0.75))

quantile(diabetes_data$ob_prev_adj, c(0.25, 0.50, 0.75))

quantile(diabetes_data$ltpia_prev_adj, c(0.25, 0.50, 0.75))


# Convert continuous variables to factor variables
diabetes_data$inc_level[diabetes_data$income<46.4]<-1

diabetes_data$inc_level[diabetes_data$income>=46.4]<-2

diabetes_data$inc_level<-as.factor(diabetes_data$inc_level)


diabetes_data$pov_level[diabetes_data$povpct<15]<-1

diabetes_data$pov_level[diabetes_data$povpct>=15]<-2

diabetes_data$pov_level<-as.factor(diabetes_data$pov_level)



diabetes_data$unemp_level[diabetes_data$unemploy<4.1]<-1

diabetes_data$unemp_level[diabetes_data$unemploy>=4.1]<-2

diabetes_data$unemp_level<-as.factor(diabetes_data$unemp_level)


diabetes_data$perc65up_level[diabetes_data$perc_65up<17.2]<-1

diabetes_data$perc65up_level[diabetes_data$perc_65up>=17.2]<-2

diabetes_data$perc65up_level<-as.factor(diabetes_data$perc65up_level)


diabetes_data$ob_level[diabetes_data$ob_prev_adj<31.4]<-1

diabetes_data$ob_level[diabetes_data$ob_prev_adj>=31.4]<-2

diabetes_data$ob_level<-as.factor(diabetes_data$ob_level)


diabetes_data$inact_perc_level[diabetes_data$ltpia_prev_adj<25.5]<-1

diabetes_data$inact_perc_level[diabetes_data$ltpia_prev_adj>=25.5]<-2

diabetes_data$inact_perc_level<-as.factor(diabetes_data$inact_perc_level)


# Transform the dependent variable to meet the assumptions for ANOVA/Regression
diabetes_data<-subset(diabetes_data, dm_prev_adj!=-1)

plot(density(diabetes_data$dm_prev_adj))
agostino.test(diabetes_data$dm_prev_adj)
diabetes_data$logdm<-log(diabetes_data$dm_prev_adj+1)

plot(density(diabetes_data$logdm))
agostino.test(diabetes_data$logdm)

bartlett.test(diabetes_data$logdm, diabetes_data$inc_level)
bartlett.test(diabetes_data$logdm, diabetes_data$pov_level)
bartlett.test(diabetes_data$logdm, diabetes_data$unemp_level)
bartlett.test(diabetes_data$logdm, diabetes_data$perc65up_level)
bartlett.test(diabetes_data$logdm, diabetes_data$ob_level)
bartlett.test(diabetes_data$logdm, diabetes_data$inact_perc_level)

# Check whether heteroskedasticity is a serious problem
tapply(diabetes_data$logdm, diabetes_data$inc_level, var)
tapply(diabetes_data$logdm, diabetes_data$pov_level, var)
tapply(diabetes_data$logdm, diabetes_data$unemp_level, var)
tapply(diabetes_data$logdm, diabetes_data$perc65up_level, var)
tapply(diabetes_data$logdm, diabetes_data$ob_level, var)
tapply(diabetes_data$logdm, diabetes_data$inact_perc_level, var)

#Full ANOVA model
model<-aov(logdm~inc_level*pov_level*unemp_level*perc65up_level*ob_level*inact_perc_level, data=diabetes_data)
halfnormal(model, ME.partial = TRUE)

#Full ANOVA model in reverse
model_rev<-aov(logdm~inact_perc_level*ob_level*perc65up_level*unemp_level*pov_level*inc_level, data=diabetes_data)
halfnormal(model_rev, ME.partial = TRUE)


#Reduced ANOVA model with significant factors
model2<-aov(logdm~inact_perc_level*ob_level*unemp_level*inc_level, data=diabetes_data)
summary(model2)
Anova(model2, type = 'III')

qqnorm(model2$residuals)
qqline(model2$residuals)
shapiro.test(model2$residuals)
plot(density(model2$residuals))
plot(model2$residuals)
hist(model2$residuals)

# Mean effect
tapply(diabetes_data$logdm, diabetes_data$inc_level, mean)
tapply(diabetes_data$logdm, diabetes_data$ob_level, mean)
tapply(diabetes_data$logdm, diabetes_data$unemp_level, mean)
tapply(diabetes_data$logdm, diabetes_data$inact_perc_level, mean)

#Interaction Effects
par(mfrow=c(1,1))
interaction.ABC.plot(logdm, inc_level, ob_level, unemp_level, data=diabetes_data)
interaction.ABC.plot(logdm, inc_level, ob_level, inact_perc_level, data=diabetes_data)
interaction.ABC.plot(logdm, inc_level, unemp_level, inact_perc_level, data=diabetes_data)
interaction.ABC.plot(logdm, unemp_level, ob_level, inact_perc_level, data=diabetes_data)

#4 way interaction
sub1<-subset(diabetes_data, inact_perc_level==1)
sub2<-subset(diabetes_data, inact_perc_level==2)

interaction.ABC.plot(logdm, inc_level, ob_level, unemp_level, data=sub1)
interaction.ABC.plot(logdm, inc_level, ob_level, unemp_level, data=sub2)


## Linear Regression Model
diabetes_data1<-subset(diabetes_data, income!=-1)
diabetes_data1<-subset(diabetes_data1, diabetes_data1$ob_prev_adj!=-1)
diabetes_data1<-subset(diabetes_data1, diabetes_data1$unemploy!=-1)
diabetes_data1<-subset(diabetes_data1, diabetes_data1$ltpia_prev_adj!=-1)
diabetes_data2<-data.frame(diabetes_data1$logdm, diabetes_data1$ltpia_prev_adj, 
                            diabetes_data1$income, diabetes_data1$ob_prev_adj, diabetes_data1$unemploy)

#Correlation Plot
m<-cor(diabetes_data2)
corrplot(m, method = "circle")

#Scale the data for subsequent analysis
diabetes_data1$income<-scale(diabetes_data1$income)
diabetes_data1$ob_prev_adj<-scale(diabetes_data1$ob_prev_adj)
diabetes_data1$ltpia_prev_adj<-scale(diabetes_data1$ltpia_prev_adj)
diabetes_data1$unemploy<-scale(diabetes_data1$unemploy)

plot(density(diabetes_data1$logdm))

par(mfrow = c(2,2))
plot(density(diabetes_data1$ltpia_prev_adj))
plot(density(diabetes_data1$income))
plot(density(diabetes_data1$ob_prev_adj))
plot(density(diabetes_data1$unemploy))

#Check for non-linear relationships
scatterplot(diabetes_data1$income, diabetes_data1$logdm, grid=FALSE, boxplots=FALSE)
scatterplot(diabetes_data1$ob_prev_adj, diabetes_data1$logdm, grid=FALSE, boxplots=FALSE)
scatterplot(diabetes_data1$unemploy, diabetes_data1$logdm, grid=FALSE, boxplots=FALSE)
scatterplot(diabetes_data1$ltpia_prev_adj, diabetes_data1$logdm, grid=FALSE, boxplots=FALSE)
diabetes_data1$income2<-diabetes_data1$income^2
diabetes_data1$unemploy2<-diabetes_data1$unemploy^2


#Full regression model  
model3<-lm(logdm~income*ob_prev_adj*unemploy*ltpia_prev_adj, data=diabetes_data1)
summary(model3)

vif(model3)
qqnorm(model3$residuals)
qqline(model3$residuals)
shapiro.test(model3$residuals)

#Full model with non-linear unemployment effect
model4<-lm(logdm~income*ob_prev_adj*unemploy*ltpia_prev_adj+unemploy2, data=diabetes_data1)
summary(model4)

vif(model4)
anova(model3, model4)
qqnorm(model4$residuals)
qqline(model4$residuals)
shapiro.test(model4$residuals)

#Fulll model with non-linear income effect
model5<-lm(logdm~income*ob_prev_adj*unemploy*ltpia_prev_adj+unemploy2+income2, data=diabetes_data1)
summary(model5)
vif(model5)
anova(model4, model5)

