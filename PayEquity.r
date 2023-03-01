# Load R libraries.
library(stargazer)
library(broom)
library(dplyr)
library(ggplot2)
library(arsenal)
library(sqldf)


# Loading the data. 

data <- read.csv("/Users/spboi/Desktop/IDS/Final/dataset1.csv")

# Data cleanup and processing, arranging data into bins



# Department - Hierarchy of departments are as follows
# R & D, Technology, Legal, Finance, HR, Analytics, Sales, Operations, Procurement
# Assigning scores accordingly

data$dept_bin <- 0
data$dept_bin <- ifelse(data$department == "R&D", 9, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Technology", 8, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Legal", 7, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Finance", 6, data$dept_bin)
data$dept_bin <- ifelse(data$department == "HR", 5, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Analytics", 4, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Sales & Marketing", 3, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Operations", 2, data$dept_bin)
data$dept_bin <- ifelse(data$department == "Procurement", 1, data$dept_bin)

# Grouping 5 consecutive regions and assigning them ranks respectively. 
# Regions with lower number have higher weight

data$reg_bin <- 0
data$reg_bin <- ifelse(data$region >= 1 & data$region <= 5, 7, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 6 & data$region <= 10, 6, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 11 & data$region <= 15, 5, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 16 & data$region <= 20, 4, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 21 & data$region <= 25, 3, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 26 & data$region <= 30, 2, data$reg_bin)
data$reg_bin <- ifelse(data$region >= 31 & data$region <= 35, 1, data$reg_bin)

# Assigning ranks to employee education
# Master's & above get 3, Bachelor's get 2 and Below Secondary get 1.

data$edu_bin <- 0
data$edu_bin <- ifelse(data$education == "Master's & above", 3, data$edu_bin)
data$edu_bin <- ifelse(data$education == "Bachelor's", 2, data$edu_bin)
data$edu_bin <- ifelse(data$education == "Below Secondary", 1, data$edu_bin)

# Calculating percentiles of all employee demographic variables. These will help us calculate ranks for each employee.

data$dept_perc <- rank(data$dept_bin)/length(data$dept_bin)
data$reg_perc <- rank(data$reg_bin)/length(data$reg_bin)
data$edu_perc <- rank(data$edu_bin)/length(data$edu_bin)
data$age_perc <- rank(data$age_bin)/length(data$age_bin)
data$perf_perc <- rank(data$previous_year_rating)/length(data$previous_year_rating)
data$serv_perc <- rank(data$length_of_service)/length(data$length_of_service)
data$award_perc <- rank(data$awards_won)/length(data$awards_won)
data$trscr_perc <- rank(data$avg_training_score)/length(data$avg_training_score)

# Calculating natural logarithm of Annual salary to interpret pay gap.
data$total_salary_log <- log(data$ann_salary, base = exp(1))

# Binning the two genders
data$male <- ifelse(data$gender == "m", 1, 0)
data$female <- ifelse(data$gender == "f", 1, 0)

# Checking the structure of the imported data.
str(data)

# Converting categorical variable to factors. 

data$gender <- as.factor(data$gender)
data$edu <- as.factor(data$education)
data$dept <- as.factor(data$department)


# Create statistical summary of the data.
stargazer(data, type = "html", out = "statistics.htm", digits = 2)


# Displaying Total Pay summary grouped by gender
summarization <- group_by(data, gender)
summarization <- summarise(summarization, mean_pay = mean(ann_salary, na.rm = TRUE), medTotalPay = median(ann_salary, na.rm = TRUE), cnt = sum(!(is.na(ann_salary))) )
View(summarization)


# Performance evaluations summary stats grouped by gender. 
performance_summarization <- group_by(data, gender)
performance_summarization <- summarise(performance_summarization, meanPerf = mean(previous_year_rating, na.rm = TRUE), cnt = sum(!(is.na(previous_year_rating))))
View(performance_summarization)

# Distribution of employees by department, grouped by gender.
department_summarization <- group_by(data, dept, gender)
department_summarization <- summarise(department_summarization, mean_pay = mean(ann_salary, na.rm = TRUE), cnt = sum(!(is.na(dept))) ) %>% arrange(desc(dept, gender))
View(department_summarization)

# Distribution of employees by region, grouped by gender 
region_summarization <- group_by(data, region, gender)
region_summarization <- summarise(region_summarization, mean_pay = mean(ann_salary, na.rm = TRUE), cnt = sum(!(is.na(region))) ) %>% arrange(desc(region, gender))
View(region_summarization) 


# Running Regression Models
# Here, we are using Ordinary least squares regression model with and without controls.

# Using No controls. 
reg1 <- lm(total_salary_log ~ male, data = data)
summary(reg1)

# Using performance rating, age and highest education as controls
reg2 <- lm(total_salary_log ~ male + previous_year_rating + education, data = data)
summary(reg2)

# Using employees' whole demographic data as control. 
reg3 <- lm(total_salary_log ~ male + previous_year_rating + education + department + length_of_service + avg_training_score, data = data)
summary(reg3)

# Printing the "adjusted" gender pay gap value.
male_pay_gap <- coef(reg3)["male"] 
# This value multiplied by 100 gives us the percentage pay gap between males and females. Our goal is to reduce this value
print(male_pay_gap)


# Creating a HTML report of regression results.
stargazer(reg1, reg2, reg3, type = "html", out = "results.htm",
          add.lines = list(
            c("Controls:"),
            c("Education","No","Yes", "Yes"),
            c("Department","No", "No", "Yes"),
            c("length_of_service", "No", "No", "Yes"),
            c("-----") )
)


# Department wise results of the linear model. 
dept_wise_results <- lm(total_salary_log ~ male*department + previous_year_rating + age_bin + education + length_of_service + avg_training_score, data = data)
summary(dept_wise_results)
dept_wise_results_clean <- tidy(dept_wise_results)
stargazer(dept_wise_results, type = "html", out = "dept.htm", omit = c("jobTitle", "edu"))
write.csv(dept_wise_results_clean, file = "dept_clean.csv")

#Calculating averages of employees' demographic data

data$average1 <- (((data$dept_perc + data$reg_perc + data$edu_perc + data$age_perc + data$perf_perc + data$serv_perc + data$award_perc + data$trscr_perc)/8)*100)
data$average_perc <- rank(data$average1)/length(data$average1)

# Using ggplot to plot a linear model against demographic data averages and log of annual salary
ggplot(data, aes(x = average1, y = total_salary_log, col = gender)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# Using ggplot to plot a linear model against demographic data averages and department and region.
ggplot(data, aes(x = dept_bin, y = total_salary_log, col = gender)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(data, aes(x = reg_bin, y = total_salary_log, col = gender)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# Creating subsets of the main dataset, for each gender.
data_male <- subset(data, male == 1)
data_male <- data.frame (data_male)

data_female <- subset(data, female == 1)
data_female <- data.frame (data_female)

# This function displays details of pairs of employees (male, female) who are in the same department and in the same location, and have average score ratio between 0.99 and 1 and have annual salary ratio greater than or equal to 1.
# This means that as average score ratio is very close to the male and female employee being compared, they have very similar demographic variables, and hence, should be paid equally.
# When annual salary ratio is greater than or equal to 1, it means that there is a pay gap.
# when all 4 of these conditions satisfy for a pair of employees, it means that the male employee and the female employee have similar demographic variables, but have gender pay gap.
# We also display a variable Sggst_Amt that suggests wage increase to the female employee so that the pay gap is minimized.
sugg <- sqldf ("SELECT data_male.employee_id AS m_emp_id, data_female.employee_id AS f_emp_id, data_male.department AS m_dept, data_female.department AS f_dept, data_male.region AS m_region, data_female.region AS f_region, data_male.average1 AS m_average, data_female.average1 AS f_average, data_male.ann_salary AS m_ann_sal, data_female.ann_salary AS f_ann_sal, (data_male.ann_salary - data_female.ann_salary) AS Suggst_Amt
                     FROM data_male INNER JOIN data_female
                     WHERE data_male.department == data_female.department AND data_male.region == data_female.region AND (data_male.average1 / data_female.average1 <= 1) AND (data_male.average1 / data_female.average1 >= 0.99) AND (data_male.ann_salary / data_female.ann_salary >= 1) 
                     ")

write.csv(sugg,"Suggestions.csv", row.names = TRUE)



