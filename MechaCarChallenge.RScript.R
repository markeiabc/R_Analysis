setwd("~/OneDrive/Data Analytics Bootcamp/R_Analysis")
library(tidyverse)

#Read CSV Files for Challenge
MechaCar_table <- read.csv(file = 'MechaCar_mpg.csv',check.names = T,stringsAsFactors = F)
Suspension_Coil_table <- read.csv(file='Suspension_Coil.csv',check.names = T,stringsAsFactors = F)

View(MechaCar_table)

#Predict mpg of MechaCar prototypes - use qualitative test for normality
#Visualize distribution using density plot
ggplot(MechaCar_table,aes(x=mpg)) + geom_density()
 
#Predict mpg of MechaCar prototypes - use quantitative test for normality
shapiro.test(MechaCar_table$mpg)

#Normal distribution bell curve - mean and median should be close in value.

#Generate multiple linear regression model

lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) 
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=MechaCar_table) 
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=MechaCar_table))
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table))

#Confirmed that intercept, vehicle length, and ground clearance have significant impact on mpg

#Create summary statistics table for the suspension coil's pounds-per-inch continuous variable
View(Suspension_Coil_table)
summary(Suspension_Coil_table)

#Reference-https://stackoverflow.com/questions/23163863/mean-of-a-column-in-a-data-frame-given-the-columns-name
mean(Suspension_Coil_table$PSI)
median(Suspension_Coil_table$PSI)
var(Suspension_Coil_table$PSI)
sd(Suspension_Coil_table$PSI)

#Create a dataframe with the summary statistics (https://www.dummies.com/programming/r/how-to-create-a-data-frame-from-scratch-in-r/)
table_columns <- c("Mean", "Median", "Variance", "Standard_Deviation")
values <- c(1499.531, 1499.747, 76.23459, 8.731242)
summary_statistics_table <- data.frame(table_columns, values)
View(summary_statistics_table)

#Change column name of the statistics table
colnames(summary_statistics_table)[which(names(summary_statistics_table) == "table_columns")] <- "stat_name"

#Run t-test for Suspension_Coil
t.test(Suspension_Coil_table$PSI,mu=1500)
