library(jsonlite)
library(tidyverse)

car_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=car_table) #generate multiple linear regression model for MechaCar MPG

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=car_table)) #generate summary statistics for MechaCar MPG

summarize_suspension <- suspension_table %>% summarize(Mean=mean(PSI),Median=median(PSI),SD=sd(PSI),Variance=var(PSI)) #generate a table with mean, median, SD, and variance for the suspension coil data

sample_suspension <- suspension_table %>% sample_n(50) #randomly sample 50 PSI measures

t.test(log10(sample_suspension$PSI),mu=mean(log10(suspension_table$PSI))) #compare sample versus population means
