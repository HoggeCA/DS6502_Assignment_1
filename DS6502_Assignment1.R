#Read the data set
salarydata <- read.csv("ds_salaries.csv")

#Install and run necessary packages for out graphs.
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
setwd("")
getwd()

#Cleaning the data into more refined versions
cleaned_data <- subset(salarydata, company_location %in% c("US", "CA", "DE", "IN", "GB", "FR", "ES", "GR"))
read(cleaned_data)

top_jobs <- subset(salarydata, job_title %in% c ("Data Analyst", "Data Engineer", "Data Scientist", "Machine Learning Engineer", "Research Scientist", 
"Data Architect", "Data Science Manager", "Data Analytics Manager"))

us_jobs <- subset(salarydata, company_location == "US")
us_avgSalary <- mean(us_jobs$salary_in_usd)
avgsalaryyear <- aggregate(salary_in_usd ~ work_year ,data = cleaned_data, FUN = mean)

count <- nrow(cleaned_data)

#Computing basic descriptive statistics for salary data
summary(salarydata$salary_in_usd)
sd(salarydata$salary_in_usd)
range(salarydata$salary_in_usd)

#Computing basic descriptive statistics for cleaned data
summary(cleaned_data$salary_in_usd)
sd(cleaned_data$salary_in_usd)
range(cleaned_data$salary_in_usd)

#These functions count the number of people inside a data set.
count_data <- cleaned_data %>%
group_by(company_location) %>%
summarise(count = n())

count_jobs <- top_jobs %>%
group_by(job_title) %>%
summarise(number_of_jobs = n())

#Bar chart that displays the eight most popular company locations and their average salary.  
ggplot(cleaned_data, aes(x = company_location, y = salary_in_usd, fill = company_location)) +
geom_bar(stat = "summary", fun = "mean") +
labs(x = "Company Location", y = "Average Salary (USD)", title = "Average Salary by Company Location") +
scale_y_continuous(labels = scales::comma) +
scale_fill_manual(values = rainbow(length(unique(cleaned_data$company_location)))) 

#Line graph that displays average salary for each year documented in the data set.
ggplot(data = avgsalaryyear, aes(x = work_year, y = salary_in_usd, color = "red")) + geom_line() +
stat_summary( color = "red") +
labs(x = "Work Year", y = "Average Salary (USD)", title = "Average Salary by Work Year") 

#Pie Chart that shows us which countries the majority of these IT companies are from.
ggplot(data = count_data, aes(x = "", y = count, fill = company_location)) + 
geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +  theme_minimal() +
labs(y = "Company Location by Job count", title = "Number of Jobs Listed by country") +
geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +  
theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid  = element_blank())

#Donut graph that displays the eight most common job titles. 
ggplot(count_jobs, aes(x = "", y = number_of_jobs, fill = job_title)) +
geom_bar(width = 1, stat = "identity") +
geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5), fill = "white", color = "white") +
coord_polar("y", start = 0) + theme_void() + theme(legend.position = "right") +
geom_text(aes(label = number_of_jobs), position = position_stack(vjust = 0.5)) +
labs(title = "Most popular roles and how many positions there are")

#Box and whisker graph that determines if salary is based off company size.
ggplot(data = salarydata, aes(x = company_size, y = salary_in_usd, fill = company_size)) + geom_boxplot() +
scale_y_continuous(labels = scales::comma) + labs(title = "Salary by company size")
