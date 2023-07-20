#setting up environment
library(readr)
library(naniar)
library(visdat)
library(tidyverse)
library(plyr)
library(dplyr)
library(factoextra)

#importing and copying dataset
medical_clean <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D212 - Data Mining II/medical_clean.csv")
medical_clean_copy <- medical_clean

#general view/exploration
str(medical_clean)
vis_miss(medical_clean)
sum(is.na(medical_clean))
sum(duplicated((medical_clean)))

#plan on utilizing continuous, patient demographic variables that are present prior to admission to hospital
#age, income, children, latitude, longitude, population

#further exploring/cleaning - outliers?
#z-score columns
medical_clean$population_z <- scale(x=medical_clean$Population)
medical_clean$children_z <- scale(x=medical_clean$Children)
medical_clean$age_z <- scale(x=medical_clean$Age)
medical_clean$income_z <- scale(x=medical_clean$Income)
medical_clean$lat_z <- scale(x=medical_clean$Lat)
medical_clean$long_z <- scale(x=medical_clean$Lng)

#outlier vectors
population_outliers <- which(medical_clean$population_z >3 | medical_clean$population_z < -3)
children_outliers <- which(medical_clean$children_z >3 | medical_clean$children_z < -3)
age_outliers <- which(medical_clean$age_z >3 | medical_clean$age_z < -3)
income_outliers <- which(medical_clean$income_z >3 | medical_clean$income_z < -3)
#not bringing in outliers for Lat/Long, as these are geographic and slightly different from the continuous nature of income/age, etc.

#treating outliers
unique_outliers <- unique(c(children_outliers, income_outliers, population_outliers))
medical_clean <- medical_clean[-unique_outliers, ]
medical_clean <- subset(medical_clean, select = c(Customer_id, age_z, income_z, 
                                        population_z, children_z
                                        , lat_z, long_z))
str(medical_clean)
summary(medical_clean)
sd(medical_clean$age_z)

#removing outlier vectors/objects from environment
remove(age_outliers, children_outliers, income_outliers 
       , unique_outliers, population_outliers)

#writing prepped data to folder
write.csv(medical_clean, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D212 - Data Mining II\\Task 2\\medical_clean.csv')

#performing PCA
set.seed(42)
medical_pca <- prcomp(medical_clean[, 2:7])
medical_pca$rotation
biplot(medical_pca, scale=0)

#continued calculations by each PC
fviz_eig(medical_pca, choice = "eigenvalue", addlabels = TRUE)
medical_pca$rotation[, 1:4]

#explained variance
medical_var_explained <- round(medical_pca$sdev^2 / sum(medical_pca$sdev^2)*100, 2)
print(medical_var_explained[1:4])
medical_var_explained_4pcs <- sum(medical_var_explained[1:4])
print(medical_var_explained_4pcs)
