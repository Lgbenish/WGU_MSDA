#setting up environment
library(dplyr)
library(ggplot2)
library(naniar)
library(visdat)
library(readr)
library(ranger)
library(caret)


#import dataset
medical_clean <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D209 - Data Mining I/Task 2 - Predictive Analysis/medical_clean.csv")
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

#removing outlier vectors/objects from environment
remove(additional_charges_outliers, age_outliers, children_outliers, doc_visits_outliers, full_meals_eaten_outliers,
       income_outliers, initial_days_outliers, total_charge_outliers, unique_outliers, vitd_levels_outliers, vitd_supp_outliers)

#removing unnecessary explanatory variables
medical_clean4 <- subset(medical_clean3, select = c(ReAdmis, Initial_admin, Complication_risk, Arthritis, 
                                                    Anxiety, Allergic_rhinitis, HighBlood, Full_meals_eaten, 
                                                    Initial_days))


#visualizing only those pertinent variables from prior linear regression
#univariate viz
ggplot(medical_clean4, aes(x=Initial_days)) + geom_histogram(binwidth = 1)
ggplot(medical_clean4, aes(x=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Initial_admin)) + geom_bar()
ggplot(medical_clean4, aes(x=Complication_risk)) + geom_bar()
ggplot(medical_clean4, aes(x=Arthritis)) + geom_bar()
ggplot(medical_clean4, aes(x=Anxiety)) + geom_bar()
ggplot(medical_clean4, aes(x=Allergic_rhinitis)) + geom_bar()
ggplot(medical_clean4, aes(x=HighBlood)) + geom_bar()
ggplot(medical_clean4, aes(x=Full_meals_eaten)) + geom_histogram(binwidth=1)

#bivariate viz
ggplot(medical_clean4, aes(x=ReAdmis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=Initial_admin, y=Initial_days)) +geom_boxplot()
ggplot(medical_clean4, aes(x=Complication_risk, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=Arthritis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=Anxiety, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=Allergic_rhinitis, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=HighBlood, y=Initial_days)) + geom_boxplot()
ggplot(medical_clean4, aes(x=Full_meals_eaten, y=Initial_days)) + geom_point()

#writing cleaned/pre-processed medical_clean4 to folder
write.csv(medical_clean4, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 2 - Predictive Analysis\\medical_clean4.csv')

#set seed for future needs
set.seed(42)

#train-test-split
sample_rows <- sample(nrow(medical_clean4), nrow(medical_clean4)*0.8)
n_80_20_split <- round(nrow(medical_clean4) * 0.8, 0)
medical_clean_train <- medical_clean4[sample_rows, ]
medical_clean_test <- medical_clean4[-sample_rows, ]

#writing split data sets to folder
write.csv(medical_clean_train, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 2 - Predictive Analysis\\medical_clean_train.csv')
write.csv(medical_clean_test, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 2 - Predictive Analysis\\medical_clean_test.csv')

#modeling
rf_model <- train(
  Initial_days ~ .,
  medical_clean_train,
  method = 'ranger',
  trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = TRUE
  )
)

plot(rf_model)

#hyperparameter tuning
#introducing tuning grid and retrain
tuneGrid <- expand.grid(
  .mtry = 6,
  .splitrule = c('variance', 'extratrees'),
  .min.node.size = c(5, 10, 20)
)

rf_model <- train(
  Initial_days ~ .,
  medical_clean_train,
  method = 'ranger',
  tunelength = 3,
  tuneGrid = tuneGrid, 
  trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = TRUE
  )
)

print(rf_model)
plot(rf_model)

#modify tuning grid to optimal RMSE paramaters and retrain

tuneGrid <- data.frame(
  .mtry = 6,
  .splitrule = 'extratrees',
  .min.node.size = 20
)

rf_model <- train(
  Initial_days ~ .,
  medical_clean_train,
  method = 'ranger',
  #tunelength removed due to error messages
  tuneGrid = tuneGrid, 
  trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = TRUE
  )
)


#prediction on training data
medical_clean_train$pred <- predict(rf_model, medical_clean_train)
residual_train <- medical_clean_train$Initial_days - medical_clean_train$pred
hist(residual_train)
summary(residual_train)
rmse_train <- sqrt(mean(residual_train^2))

#predicting on test data
medical_clean_test$pred <- predict(rf_model, medical_clean_test)

#calculating accuracy/RMSE of test data
#residual = actual - pred
residual_test <- medical_clean_test$Initial_days - medical_clean_test$pred
hist(residual_test)
summary(residual_test)
rmse_test <- sqrt(mean(residual_test^2))
