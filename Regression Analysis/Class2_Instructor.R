
### set working directory ###

setwd("Address of Your Working Directory")

### View the dataset (crime_data) ###
View(crime_data)
### View the first n rows of your dataset ###
head(crime_data,n=10)
### View the last n rows of your dataset ###
tail(crime_data,n=10)
### View the structure (variable names and types) of the dataset ###
str(crime_data)

### Count the observations in the data set ###
length(crime_data$pcnt)
length(crime_data$rate)

### Print the unique values of a variable ###
unique(crime_data$pcnt)

### Print the occurrence of each unique value ###
table(crime_data$pcnt)

### Sort the variable in ascending order ###
sort(crime_data$pcnt)

#### Descriptive Statistics ###

### mean, median, min, max, var, range, sd, var, summary ###
### 3-digit variance? ###

round(var(crime_data$pcnt),digits=3)

### Graphical Tools ###

### Histogram ###

hist(crime_data$pcnt, main = "Histogram of Percentage of Individuals", 
     xlab = "Percentage", ylab = "Frequency", las = 1)

### Boxplot ###

boxplot(crime_data$pcnt, main = "Boxplot of Percentage of Individuals",
        ylab = "Percentage", las = 1 )

### Scatter plot ###

plot(crime_data$pcnt, crime_data$rate, 
     main = "Scatter Plot of crime rate \n By percentage", 
     ylab = "Crime Rate (per 100,000 residents)", xlab = "Percentage", las = 1) 

### multiple plots in one pane ###

par(mfrow=c(1,3)) ### Show 3 plots on the pane altogether ###

hist(crime_data$pcnt, main = "Histogram of Percentage of Individuals", 
     xlab = "Percentage", ylab = "Frequency", las = 1)

boxplot(crime_data$pcnt, main = "Boxplot of Percentage of Individuals",
        ylab = "Percentage", las = 1 )

plot(crime_data$pcnt, crime_data$rate, 
     main = "Scatter Plot of crime rate \n By percentage", 
     ylab = "Crime Rate (per 100,000 residents)", xlab = "Percentage", las = 1) 

### Adding Line of Best Fit to Scatter Plots ###

abline(lm(crime_data$rate~crime_data$pcnt), col=c("red"))




