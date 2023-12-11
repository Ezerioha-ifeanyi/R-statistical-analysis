#loading required library
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)


#import the dataset
data <- read.csv("Dataset_3652.csv")



#create a new column that houses the body mass index
#since the unit of weight and height are in pounds and inches respectively
#we divide the weight by the square of the value of the height and multiply by 703
data["BMI"] = (data["weight"] / ((data["height"] * 0.0254))^2) 



#check the maximum value of BMI
print(min(data$BMI))

#check the structure of the dataset
str(data)

#convert exercise, healthplan and smoke100 column to strings

data$exercise <- as.character(data$exercise)
data$healthplan <- as.character(data$healthplan)
data$smoke100 <- as.character(data$smoke100)

#recheck the structure of the dataset
str(data)

# Grouping the BMI into just underweight and healthy weight since maximum value of BMI is 24.86888
data["BMI_categories"] = cut(data$BMI, breaks = c(-Inf, 18.4, 24.9,29.9,Inf), 
                             labels = c("Underweight", "Healthy Weight", "Overweight", "Obesity"))




# Grouping the age into categories
data["age_categories"] = cut(data$age, breaks = c(17, 44, 64, Inf), 
                             labels = c("18-44 years", "45-64 years",  "Over 64 years"))


#check the first 10 values
print(head(data, n=10))

#creating frequency plot of general health status
df_health = data.frame((values=data$genhealth))

ggplot(df_health, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of General Health Status", x="Health Status", y="Frequency")



#creating frequency plot of respondent level of exercise
df_exercise = data.frame((values=data$exercise))

ggplot(df_exercise, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of Respondent Level of Exercise", x="Values", y="Frequency")


#creating frequency plot of respondent health plan
df_health_plan = data.frame((values=data$healthplan))

ggplot(df_health_plan, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of Respondent Health Plan", x="Values", y="Frequency")


#creating frequency plot of respondent smoking level
df_smoke = data.frame((values=data$smoke100))

ggplot(df_smoke, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of Respondent Smoking Level", x="Values", y="Frequency")


#creating frequency plot of respondent gender
df_gender = data.frame((values=data$gender))

ggplot(df_gender, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of Respondent Gender", x="Values", y="Frequency")


#creating frequency plot of respondent age categories
df_age_cat = data.frame((values=data$age_categories))

ggplot(df_age_cat, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of Respondent Age Category", x="Values", y="Frequency")

#creating frequency plot of BMI categories
df_BMI_cat = data.frame((values=data$BMI_categories))

ggplot(df_BMI_cat, aes(x=values)) +
  geom_bar(stat="count", fill="skyblue", color="black") +
  geom_text(stat="count", aes(label= after_stat(count)), vjust=-0.5, size=3) +
  labs(title="Frequency Plot of BMI Categories", x="BMI Categories", y="Frequency")

# Group the data by gender and calculate the average body mass index
Gender_BMI <- data[,c("gender", "BMI")]

average_gBMI <- Gender_BMI %>% 
  group_by(gender) %>% 
  summarise(mean_value = mean(BMI))


# Create a bar chart of the BMI mean value by gender
barplot(average_gBMI$mean_value, names.arg = average_gBMI$gender, 
        xlab = "Gender", ylab = "Mean_value", main = "Bar Chart of Mean BMI by Gender",
        col="steelblue")



# Group the data by smoke100 and calculate the average body mass index
smoke_BMI <- data[,c("smoke100", "BMI")]

average_sBMI <- smoke_BMI %>% 
  group_by(smoke100) %>% 
  summarise(mean_value = mean(BMI))


# Create a bar chart of the BMI mean value by smoke
barplot(average_sBMI$mean_value, names.arg = average_sBMI$smoke100, 
        xlab = "Smoke100", ylab = "Mean_value", main = "Bar Chart of Mean BMI by Smoke100",
        col="steelblue")



#slicing the data to have genhealth, gender and BMI
ggBMI <- data[,c("genhealth","gender", "BMI")]

#Grouping the sliced data by genhealth and gender
average_ggBMI <- ggBMI %>% 
  group_by(genhealth,gender) %>% 
  summarise(mean_value = mean(BMI))


# creating a bar plots of Mean_BMI by general health and gender
ggplot(average_ggBMI, aes(genhealth, mean_value, fill = gender)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(title="Plot of Mean_BMI by general health and gender")



#Creating a stacked barchart of Genhealth and Healthplan
gen_healthplan = data.frame(data$genhealth, data$healthplan)

ggplot(gen_healthplan, aes(x = data.genhealth, fill = data.healthplan)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of general health and healthplan", y="count")


#Creating a stacked barchart of age category and exercise
age_exercise = data.frame(data$age_categories, data$exercise)

ggplot(age_exercise, aes(x = data.age_categories, fill = data.exercise)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of age categories and exercise level", y="count")



#Creating a stacked barchart of BMI category and Smoke100
BMI_smoke = data.frame(data$BMI_categories, data$smoke100)

ggplot(BMI_smoke, aes(x = data.BMI_categories, fill = data.smoke100)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of BMI_categories and smoke100", y="count")


#Creating a stacked barchart of Healthstatus and exercise
health_exer = data.frame(data$genhealth, data$exercise)

ggplot(health_exer, aes(x = data.genhealth, fill = data.exercise)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of Health status and Exercise level", y="count")


#Creating a stacked barchart of Age categories and smoking level
age_smoke = data.frame(data$age_categories, data$smoke100)

ggplot(age_smoke, aes(x = data.age_categories, fill = data.smoke100)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of Age Categories and Smoking", y="count")


#Creating a stacked barchart of Gender and smoking level
gender_smoke = data.frame(data$gender, data$smoke100)

ggplot(gender_smoke, aes(x = data.smoke100, fill = data.gender)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of Gender and Smoking", y="count")


#Creating a stacked barchart of Gender and Exercise
gender_exer = data.frame(data$gender, data$exercise)

ggplot(gender_exer, aes(x = data.exercise, fill = data.gender)) +
  geom_bar(position = "stack") + 
  labs(title="Stacked barchart of Gender and Exercise", y="count")

#sTATISTICAL ANALYSIS

#is there any correlation between current weight and desired weight
correlation <- cor(data$weightdsr, data$weight)
print(correlation)

#scatterplot to ascertain our result
ggplot(data,
       aes(x=weight, y=weightdsr)) +
  geom_point()

#CHISQUARE ANALYSIS TO CHECK ASSOCIATION BETWEEN GENDER AND SMOKING
gender_smoke = data.frame(data$gender,data$smoke100)		 
gender_smoke = table(data$gender,data$smoke100)				 
print(gender_smoke) 

print(chisq.test(gender_smoke)) 


#CHISQUARE ANALYSIS TO CHECK ASSOCIATION BETWEED general health AND body mass index
health_BMI = data.frame(data$genhealth,data$BMI_categories)		 
health_BMI = table(data$genhealth,data$BMI_categories)				 
print(health_BMI) 

print(chisq.test(health_BMI)) 



#CHISQUARE ANALYSIS TO CHECK IF SMOKING IS RELATED TO RESPONDENT HEALTH
g_BMI = data.frame(data$smoke100,data$genhealth)		 
g_BMI = table(data$smoke100,data$genhealth)				 
print(g_BMI) 

print(chisq.test(g_BMI)) 



#CHISQUARE ANALYSIS TO CHECK IF GENHEALTH IS RELATED TO EXERCISE
g_exer = data.frame(data$genhealth,data$exercise)		 
g_exer = table(data$genhealth,data$exercise)				 
print(g_exer) 

print(chisq.test(g_exer)) 




#CHISQUARE ANALYSIS TO CHECK IF GENDER IS RELATED TO RESPONDENT HEALTH
g_health = data.frame(data$gender,data$genhealth)		 
g_health = table(data$gender,data$genhealth)				 
print(g_health) 

print(chisq.test(g_health)) 



#CHISQUARE ANALYSIS TO CHECK IF AGE CATEGORIES IS RELATED TO SMOKE
a_smoke = data.frame(data$age_categories,data$smoke100)		 
a_smoke = table(data$age_categories,data$smoke100)				 
print(a_smoke) 

print(chisq.test(a_smoke)) 



#CHISQUARE ANALYSIS TO CHECK IF GENDER IS RELATED TO RESPONDENT HEALTH PLAN
g_plan = data.frame(data$gender,data$healthplan)		 
g_plan = table(data$gender,data$healthplan)				 
print(g_plan) 

print(chisq.test(g_plan)) 




#CHISQUARE ANALYSIS TO CHECK ASSOCIATION BETWEED gender AND body mass index category
gen_BMI = data.frame(data$gender,data$BMI_categories)		 
gen_BMI = table(data$gender,data$BMI_categories)				 
print(gen_BMI) 

print(chisq.test(gen_BMI))




#CHISQUARE ANALYSIS TO CHECK IF GENDER IS RELATED TO EXERCISE
gen_exer = data.frame(data$gender,data$exercise)		 
gen_exer = table(data$gender,data$exercise)				 
print(gen_exer) 

print(chisq.test(gen_exer)) 



# R program to find the confidence interval for body mass index

# Calculate the mean of the sample data
mean_value <- mean(data$BMI)

# Compute the size
n <- length(data$BMI)

# Find the standard deviation
standard_deviation <- sd(data$BMI)

# Find the standard error
standard_error <- standard_deviation / sqrt(n)
alpha = 0.05
degrees_of_freedom = n - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound <- mean_value - margin_error
upper_bound <- mean_value + margin_error

# Print the confidence interval
print(c(lower_bound,upper_bound))



