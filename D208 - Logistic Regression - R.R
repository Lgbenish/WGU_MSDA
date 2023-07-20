#setting up environment
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(naniar)
library(visdat)
library(readr)
library(caret)

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

#creating numeric response variable
medical_clean3$ReAdmisNumeric <- as.numeric(as.factor(medical_clean3$ReAdmis))
medical_clean3$ReAdmisNumeric <- medical_clean3$ReAdmisNumeric-1
hist(medical_clean3$ReAdmisNumeric)


#univariate visualizations
ggplot(medical_clean3, aes(x=ReAdmisNumeric)) + geom_histogram()
ggplot(medical_clean3, aes(x=Children)) + geom_histogram(binwidth=1)
ggplot(medical_clean3, aes(x=Age)) + geom_histogram()
ggplot(medical_clean3, aes(x=Income)) + geom_histogram(bins=60)
ggplot(medical_clean3, aes(x=Marital)) + geom_bar()
ggplot(medical_clean3, aes(x=Gender)) + geom_bar()
ggplot(medical_clean3, aes(x=VitD_levels)) + geom_histogram()
ggplot(medical_clean3, aes(x=Doc_visits)) + geom_histogram(binwidth=1)
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
ggplot(medical_clean3, aes(x=Initial_days)) + geom_histogram(binwidth=1)
ggplot(medical_clean3, aes(x=TotalCharge)) + geom_histogram(bins=60)
ggplot(medical_clean3, aes(x=Additional_charges)) + geom_histogram(bins=60)

#trying different bivariate viz
ggpairs(data=medical_clean3, columns = c('ReAdmis', 'Children', 'Age', 'Income', 'Marital', 'Gender', 'VitD_levels', 
                       'Doc_visits', 'Full_meals_eaten', 'vitD_supp', 'Soft_drink', 'Initial_admin',
                       'HighBlood', 'Stroke', 'Complication_risk', 'Overweight', 'Arthritis', 'Diabetes',
                       'Hyperlipidemia', 'BackPain', 'Anxiety', 'Allergic_rhinitis', 'Reflux_esophagitis',
                       'Asthma', 'Services', 'Initial_days', 'TotalCharge', 'Additional_charges'))

#bivariate visualizations
ggplot(medical_clean3, aes(x=Children, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Age, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Income, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Marital, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Gender, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=VitD_levels, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Doc_visits, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Full_meals_eaten, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=vitD_supp, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Soft_drink, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Initial_admin, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=HighBlood, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Stroke, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Complication_risk, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Overweight, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Arthritis, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Diabetes, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Hyperlipidemia, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=BackPain, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Anxiety, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Allergic_rhinitis, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Reflux_esophagitis, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Asthma, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Services, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean3, aes(x=Initial_days, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=TotalCharge, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean3, aes(x=Additional_charges, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)



