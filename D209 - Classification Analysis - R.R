#setting up environment
library(dplyr)
library(ggplot2)
library(naniar)
library(visdat)
library(readr)
library(class)
library(caret)
library(fastDummies)
library(pROC)

#import dataset
medical_clean <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D209 - Data Mining I/Task 1 - Classification Analysis/medical_clean.csv")
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

#creating numeric response variable
medical_clean3$ReAdmisNumeric <- as.numeric(as.factor(medical_clean3$ReAdmis))
medical_clean3$ReAdmisNumeric <- medical_clean3$ReAdmisNumeric-1
hist(medical_clean3$ReAdmisNumeric)

#reducing dataset to only previously noted significant explanatory variables
medical_clean4 <- subset(medical_clean3, select = c(Children, Age, Initial_admin, HighBlood, Stroke, Complication_risk, 
                                                    Arthritis, Diabetes, Anxiety, Reflux_esophagitis, Asthma, Services, 
                                                    Initial_days, ReAdmisNumeric, ReAdmis))

#visualizing only those pertinent variables from prior logreg
#univariate viz
ggplot(medical_clean4, aes(x=ReAdmisNumeric)) + geom_histogram(bins=3)
ggplot(medical_clean4, aes(x=Children)) + geom_histogram(binwidth=1)
ggplot(medical_clean4, aes(x=Age)) + geom_histogram(binwidth = 2)
ggplot(medical_clean4, aes(x=Initial_admin)) + geom_bar()
ggplot(medical_clean4, aes(x=HighBlood)) + geom_bar()
ggplot(medical_clean4, aes(x=Stroke)) + geom_bar()
ggplot(medical_clean4, aes(x=Complication_risk)) + geom_bar()
ggplot(medical_clean4, aes(x=Arthritis)) + geom_bar()
ggplot(medical_clean4, aes(x=Diabetes)) + geom_bar()
ggplot(medical_clean4, aes(x=Anxiety)) + geom_bar()
ggplot(medical_clean4, aes(x=Reflux_esophagitis)) + geom_bar()
ggplot(medical_clean4, aes(x=Asthma)) + geom_bar()
ggplot(medical_clean4, aes(x=Services)) + geom_bar()
ggplot(medical_clean4, aes(x=Initial_days)) + geom_histogram()


#bivariate viz
ggplot(medical_clean4, aes(x=Children, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean4, aes(x=Age, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)
ggplot(medical_clean4, aes(x=Initial_admin, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=HighBlood, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Stroke, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Complication_risk, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Arthritis, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Diabetes, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Anxiety, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Reflux_esophagitis, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Asthma, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Services, fill=ReAdmis)) + geom_bar()
ggplot(medical_clean4, aes(x=Initial_days, y=ReAdmisNumeric)) + geom_point() + geom_smooth(method='glm', se=FALSE)


#creating dummy explanatory variable columns
medical_clean5 <- dummy_cols(medical_clean4, select_column = c('Initial_admin', 'HighBlood', 'Stroke', 'Complication_risk',
                                                               'Arthritis', 'Diabetes', 'Anxiety', 'Reflux_esophagitis', 'Asthma',
                                                               'Services'), remove_selected_columns = TRUE)
medical_clean5 <- subset(medical_clean5, select = -c(ReAdmis))

#showing all numeric
summary(medical_clean5)

#normalizing numeric variables that need it (not dummy coded)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
medical_clean5$children_normal <- normalize(medical_clean5$Children)
medical_clean5$age_normal <- normalize(medical_clean5$Age)
medical_clean5$initial_days_normal <- normalize(medical_clean5$Initial_days)
medical_clean5 <- subset(medical_clean5, select = -c(Children, Age, Initial_days))

#writing cleaned/pre-processed set to folder
write.csv(medical_clean5, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 1 - Classification Analysis\\medical_clean5.csv')

#set seed for future needs
set.seed(42)

#train-test-split
sample_rows <- sample(nrow(medical_clean5), nrow(medical_clean5)*0.8)
n_80_20_split <- round(nrow(medical_clean5) * 0.8, 0)

medical_clean5_train <- medical_clean5[sample_rows, ]
medical_clean5_test <- medical_clean5[-sample_rows, ]

#writing split data sets to folder
write.csv(medical_clean5_train, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 1 - Classification Analysis\\medical_clean5_train.csv')
write.csv(medical_clean5_test, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D209 - Data Mining I\\Task 1 - Classification Analysis\\medical_clean5_test.csv')

#creating response vectors, new dataframes without responses
train_actual_readmis_numeric <- medical_clean5_train$ReAdmisNumeric
medical_clean5_train2 <- subset(medical_clean5_train, select = -c(ReAdmisNumeric))
test_actual_readmis_numeric <- medical_clean5_test$ReAdmisNumeric
medical_clean5_test2 <- subset(medical_clean5_test, select = -c(ReAdmisNumeric))

#creating different knn models
k_3 <- knn(train=medical_clean5_train2, test=medical_clean5_test2, cl=train_actual_readmis_numeric, k=3)
mean(test_actual_readmis_numeric == k_3)
#0.9737

k_5 <- knn(train=medical_clean5_train2, test=medical_clean5_test2, cl=train_actual_readmis_numeric, k=5)
mean(test_actual_readmis_numeric == k_5)
#0.9775

k_7 <- knn(train=medical_clean5_train2, test=medical_clean5_test2, cl=train_actual_readmis_numeric, k=7)
mean(test_actual_readmis_numeric == k_7)
#0.9775
#see no difference in average accuracy of k_5 and k_7

#creating new dataframes with ReAdmis as factor
medical_clean5_train3 <- medical_clean5_train
medical_clean5_train3$ReAdmisFactor <- factor(medical_clean5_train3$ReAdmisNumeric)
medical_clean5_train3 <- subset(medical_clean5_train3, select = -c(ReAdmisNumeric))
medical_clean5_test3 <- medical_clean5_test
medical_clean5_test3$ReAdmisFactor <- factor(medical_clean5_test3$ReAdmisNumeric)
medical_clean5_test3 <- subset(medical_clean5_test3, select = -c(ReAdmisNumeric))


#using caret instead of class to allow for cross-val
trcontrol <- trainControl(method = 'cv', number = 5)
knn_caret <- train(
  ReAdmisFactor ~.,
  data = medical_clean5_train3,
  method = 'knn',
  trControl=trcontrol, 
)

knn_caret_test_readmis_predictions <- predict(knn_caret, medical_clean5_test3)
mean(knn_caret_test_readmis_predictions == test_actual_readmis_numeric)
#0.9780

#accuracy
conf_mat <- table(medical_clean5_test3$ReAdmisFactor, knn_caret_test_readmis_predictions)
true_neg <- conf_mat[1, 1]
true_pos <- conf_mat[2, 2]
false_neg <- conf_mat[2, 1]
false_pos <- conf_mat[1, 2]
accuracy <- (true_neg + true_pos)/(true_neg + true_pos + false_neg + false_pos)

#creating ROC curves
knn_caret_prob_pred <- predict(knn_caret, medical_clean5_test3, type='prob')
ROC <- roc(medical_clean5_test3$ReAdmisFactor, knn_caret_prob_pred$`1`)
plot(ROC)
AUC <- auc(ROC)
