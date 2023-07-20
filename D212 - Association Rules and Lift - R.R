#setting up environment
library(readr)
library(tidyr)
library(Matrix) #brought in due to being a pre-req for arules
library(arules)

#importing and copying dataset
medical_mba <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D212 - Data Mining II/Task 3/medical_market_basket.csv")
medical_mba_copy <- medical_mba

#viewing structure of dataset
dim(medical_mba)

#removing rows where they are all NAs, followed by removing columns that are all NAs
medical_mba <- medical_mba[rowSums(is.na(medical_mba)) != ncol(medical_mba), ]
medical_mba <- medical_mba[, colSums(is.na(medical_mba)) != nrow(medical_mba)]

#next is from Dr. Kamara's code; adding transaction ID and factorizing dataframe
medical_mba$TransactionID <- factor(seq.int(nrow(medical_mba)))
medical_mba <- as.data.frame(unclass(medical_mba), stringsAsFactors = TRUE)

#pivoting into two columned dataframe
medical_mba_pre_transactions <- pivot_longer(medical_mba, cols = 1:15, names_to = "PrescriptionNBR", values_to = "Medication")
head(medical_mba_pre_transactions)
medical_mba_pre_transactions <- medical_mba_pre_transactions[, c(1, 3)]
medical_mba_pre_transactions <- medical_mba_pre_transactions[!(medical_mba_pre_transactions$Medication == ""), ]
list_medical_mba <- as.data.frame(medical_mba_pre_transactions)
list_medical_mba <- split(list_medical_mba$Medication, list_medical_mba$TransactionID)
str(list_medical_mba)

#creating "basket"
medical_mba_trans <- as(list_medical_mba, "transactions")
medical_mba_basket <- as(medical_mba_trans, "matrix")

str(medical_mba_basket)
dim(medical_mba_basket)

#writing csv to folder
write.csv(medical_mba_basket, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D212 - Data Mining II\\Task 3\\medical_mba_basket.csv')

#running apriori
medical_mba_arules <- apriori(medical_mba_basket, control = list(verbose = F), parameter = list(supp = 0.008, conf=0.4, minlen=2))

#removing redundancy
medical_redundant <- is.redundant(medical_mba_arules)
non_redundant_medical_mba <- medical_mba_arules[!medical_redundant]

inspect(head(sort(non_redundant_medical_mba, by = c("lift"), decreasing =  T), 5))
inspect(non_redundant_medical_mba)
summary(non_redundant_medical_mba)

#top 3 rules
inspect(head(sort(non_redundant_medical_mba, by = c("lift", "confidence"), decreasing =  T), 3))
