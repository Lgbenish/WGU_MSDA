#setting up environment
library(readr)
library(naniar)
library(visdat)

#importing and copying dataset
Hospital_General_Information_Kaggle <- read_csv("C:/Users/lgben/OneDrive/Desktop/MSDA/D210 - Representation and Reporting/Hospital General Information - Kaggle.csv")
Hosp_gen_info <- Hospital_General_Information_Kaggle

#general view/exploration
str(Hosp_gen_info)
vis_miss(Hosp_gen_info)
sum(is.na(Hosp_gen_info))
sum(duplicated((Hosp_gen_info)))

#keeping only pertinent variables
hosp_gen_info_2 <- Hosp_gen_info[, c('Provider ID', 'Hospital Name', 'Address', 'City', 'State', 'ZIP Code',
                                     'County Name', 'Hospital Type', 'Hospital Ownership', 'Hospital overall rating')]

#further exploring/cleaning
str(hosp_gen_info_2)
summary(hosp_gen_info_2)
sum(is.na(hosp_gen_info_2))
removable_rows <- where_na(hosp_gen_info_2)
removable_rows <- removable_rows[, 'row']
hosp_gen_info_3 <- hosp_gen_info_2[-removable_rows, ]
hosp_gen_info_3$'Hospital overall rating' <- as.integer(hosp_gen_info_3$`Hospital overall rating`)
removable_rows2 <- where_na(hosp_gen_info_3$'Hospital overall rating')
hosp_gen_info_4 <- hosp_gen_info_3[-removable_rows2,]
ggplot(hosp_gen_info_4, aes(x=State)) + geom_bar()
hist(hosp_gen_info_4$'Hospital overall rating')

write.csv(hosp_gen_info_4, 'C:\\Users\\lgben\\OneDrive\\Desktop\\MSDA\\D210 - Representation and Reporting\\hosp_gen_info_4.csv')
