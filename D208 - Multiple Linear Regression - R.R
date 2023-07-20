#setting up environment
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(naniar)
library(visdat)
library(readr)

#import dataset
medical_clean <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D208 - Predictive Modeling/medical_clean.csv")
View(medical_clean)

#assessing missingness and duplicates
vis_miss(medical_clean)
str(medical_clean)
sum(duplicated(medical_clean))

#general glimpse of dataset
summary(medical_clean)

#copy data set for outliers if needed
medical_clean2 <- medical_clean

#z-score columns
medical_clean2$children_z <- scale(x=medical_clean2$Children)
medical_clean2$age_z <- scale(x=medical_clean2$Age)
medical_clean2$income_z <- scale(x=medical_clean2$Income)
medical_clean2$vitd_levels_z <- scale(x=medical_clean2$VitD_levels)
medical_clean2$doc_visits_z <- scale(x=medical_clean2$Doc_visits)
medical_clean2$full_meals_eaten_z <- scale(x=medical_clean2$Full_meals_eaten)
medical_clean2$vitd_supp_z <- scale(x=medical_clean2$vitD_supp)
medical_clean2$initial_days_z <- scale(x=medical_clean2$Initial_days)
medical_clean2$totalcharge_z <- scale(x=medical_clean2$TotalCharge)
medical_clean2$additional_charges_z <- scale(x=medical_clean2$Additional_charges)

#outlier vectors
children_outliers <- which(medical_clean2$children_z >3 | medical_clean2$children_z < -3)
age_outliers <- which(medical_clean2$age_z >3 | medical_clean2$age_z < -3)
income_outliers <- which(medical_clean2$income_z >3 | medical_clean2$income_z < -3)
vitd_levels_outliers <- which(medical_clean2$vitd_levels_z >3 | medical_clean2$vitd_levels_z < -3)
doc_visits_outliers <- which(medical_clean2$doc_visits_z >3 | medical_clean2$doc_visits_z < -3)
full_meals_eaten_outliers <- which(medical_clean2$full_meals_eaten_z >3 | medical_clean2$full_meals_eaten_z < -3)
vitd_supp_outliers <- which(medical_clean2$vitd_supp_z >3 | medical_clean2$vitd_supp_z < -3)
initial_days_outliers <- which(medical_clean2$initial_days_z >3 | medical_clean2$initial_days_z < -3)
total_charge_outliers <- which(medical_clean2$totalcharge_z >3 | medical_clean2$totalcharge_z < -3)
additional_charges_outliers <- which(medical_clean2$additional_charges_z >3 | medical_clean2$additional_charges_z < -3)

#treating outliers
unique_outliers <- unique(c(children_outliers, doc_visits_outliers, full_meals_eaten_outliers, income_outliers,
                            vitd_levels_outliers, vitd_supp_outliers))
medical_clean3 <- medical_clean2[-unique_outliers, ]
medical_clean3 <- subset(medical_clean3, select = -c(age_z, income_z, vitd_levels_z, doc_visits_z, full_meals_eaten_z,
                                    vitd_supp_z, initial_days_z, totalcharge_z, additional_charges_z,
                                    children_z))

#univariate visualizations
ggplot(medical_clean3, aes(x=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Initial_days)) + geom_histogram(bins=60)
ggplot(medical_clean3, aes(x=Children)) + geom_histogram()
ggplot(medical_clean3, aes(x=Age)) + geom_histogram()
ggplot(medical_clean3, aes(x=Income)) + geom_histogram(bins=60)
ggplot(medical_clean3, aes(x=Marital)) + geom_bar()
ggplot(medical_clean3, aes(x=Gender)) + geom_bar()
ggplot(medical_clean3, aes(x=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=VitD_levels)) + geom_histogram()
ggplot(medical_clean3, aes(x=Doc_visits)) + geom_histogram(bins=10)
ggplot(medical_clean3, aes(x=Full_meals_eaten)) + geom_histogram(bins=6)
ggplot(medical_clean3, aes(x=vitD_supp)) + geom_histogram(bins=3)
ggplot(medical_clean3, aes(x=Soft_drink)) + geom_bar()
ggplot(medical_clean3, aes(x=Initial_admin)) + geom_bar()
ggplot(medical_clean3, aes(x=HighBlood)) + geom_bar()
ggplot(medical_clean3, aes(x=Stroke)) + geom_bar()
ggplot(medical_clean3, aes(x=Complication_risk)) + geom_bar()
ggplot(medical_clean3, aes(x=Overweight)) + geom_bar()
ggplot(medical_clean3, aes(x=Arthritis)) + geom_bar()
ggplot(medical_clean3, aes(x=Diabetes)) + geom_bar()
ggplot(medical_clean3, aes(x=Hyperlipidemia)) + geom_bar()
ggplot(medical_clean3, aes(x=BackPain)) + geom_bar()
ggplot(medical_clean3, aes(x=Anxiety)) + geom_bar()
ggplot(medical_clean3, aes(x=Allergic_rhinitis)) + geom_bar()
ggplot(medical_clean3, aes(x=Reflux_esophagitis)) + geom_bar()
ggplot(medical_clean3, aes(x=Asthma)) + geom_bar()
ggplot(medical_clean3, aes(x=Services)) + geom_bar()
ggplot(medical_clean3, aes(x=TotalCharge)) + geom_histogram(bins=60)
ggplot(medical_clean3, aes(x=Additional_charges)) + geom_histogram(bins=60)

#bivariate visualizations
ggplot(medical_clean3, aes(x=Children, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=Age, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=Income, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=Gender, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Marital, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=ReAdmis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=VitD_levels, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=Full_meals_eaten, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=vitD_supp, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
ggplot(medical_clean3, aes(x=Soft_drink, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Initial_admin, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=HighBlood, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Stroke, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Complication_risk, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Overweight, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Arthritis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Diabetes, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Hyperlipidemia, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=BackPain, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Anxiety, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Allergic_rhinitis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Reflux_esophagitis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=Services, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean3, aes(x=TotalCharge, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)
cor(medical_clean3$Initial_days, medical_clean3$TotalCharge)
#TotalCharge needs to be withheld as an explanatory variable, nearly perfectly correlated with Initial_days
ggplot(medical_clean3, aes(x=Additional_charges, y=Initial_days)) + geom_point() + geom_smooth(method='lm', se=FALSE)

#writing cleaned csv to folder
write.csv(medical_clean3, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D208 - Predictive Modeling\\medical_clean3.csv')

#model with intercept only
model_initial_only <- lm(Initial_days ~ 1, data=medical_clean3)

#simple example model
simple_model <- lm(Initial_days ~ HighBlood + Income + Gender, data=medical_clean3)
summary(simple_model)
rmse_simple <- sqrt(mean((simple_model$fitted.values - medical_clean3$Initial_days)^2))
hist(residuals(simple_model))
ggplot(simple_model, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0)

#model with all explanatory variables
model_all <- lm(Initial_days ~ Children + Age + Income + Marital + Gender + ReAdmis + VitD_levels +
                  Doc_visits + Full_meals_eaten + vitD_supp + Soft_drink + Initial_admin + HighBlood +
                  Stroke + Complication_risk + Overweight + Arthritis + Diabetes + Hyperlipidemia +
                  BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma +
                  Services + Additional_charges, data=medical_clean3)
summary(model_all)
plot(model_all)
rmse_all <- sqrt(mean((model_all$fitted.values - medical_clean3$Initial_days)^2))
AIC(model_all)

#significant variables: Intercept, ReAdmis(Yes) Initial_admin(Observation), Complication_risk(Low),
#Arthritis(Yes), Anxiety(Yes), Allergic_rhinitis(Yes), Services(CT Scan) 

#checking model_all assumptions
hist(residuals(model_all))
ggplot(model_all, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0)
#it's an 'okay' fit; all .fitted remain bimodal


#stepwise selection for model
better_fit <- step(model_initial_only, direction = 'forward', scope=formula(model_all), trace=0)
summary(better_fit)
better_fit$anova
better_fit$call
better_fit$coefficients
AIC(better_fit)

hist(better_fit$residuals)
ggplot(better_fit, aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0)
rmse_better <- sqrt(mean((better_fit$fitted.values - medical_clean3$Initial_days)^2))

#predicting
explanatory_data <- expand_grid(
  ReAdmis = unique(medical_clean3$ReAdmis),
  Initial_admin = unique(medical_clean3$Initial_admin),
  Complication_risk = unique(medical_clean3$Complication_risk),
  Arthritis = unique(medical_clean3$Arthritis),
  Anxiety = unique(medical_clean3$Anxiety),
  Allergic_rhinitis = unique(medical_clean3$Allergic_rhinitis),
  HighBlood = unique(medical_clean3$HighBlood),
  Full_meals_eaten = 0:8
)

predicted_data <- explanatory_data %>%
  mutate(Initial_days = predict(better_fit, explanatory_data))
summary(predicted_data)
ggplot(predicted_data, aes(x=Initial_days)) + geom_histogram(binwidth = 1)
