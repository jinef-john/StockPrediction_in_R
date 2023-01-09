Final Draft
The final project will include the following components:
1.	Title (<25 words)
2.	Introduction [~ 200 words]
o	Clearly stated background and questions / hypotheses / problems being addressed. Sets up the analysis in an interesting and compelling way.
3.	Materials and methods [~ 200 words]
o	Narrative: Clear narrative description of the data sources and methods. Includes data from at least two sources that were integrated / merged in R.
o	Code: The code associated with the project is well organized and easy to follow. Demonstrates mastery of R graphics and functions.
o	Data: The underlying data are publicly accessible via the web and downloaded/accessed within the Rmd script. If you want to use your own data, you must make it available on a website (e.g. Figshare) so that others are able to re-run your code.
4.	Results [~200 words]
o	Tables and figures (maps and other graphics) are carefully planned to convey the results of your analysis. Intense exploration and evidence of many trials and failures. The author looked at the data in many different ways before coming to the final presentation of the data.
5.	Conclusions [~200 words]
o	Clear summary adequately describing the results and putting them in context. Discussion of further questions and ways to continue investigation.
6.	References
o	All sources are cited in a consistent manner
7.	General Scores
o	General organization: Clear labels/headings demarcate separate sections. Excellent flow from one section to the next. Tables and graphics carefully tuned and placed for desired purpose.The ‘story’ is very well organized, makes good use of graphics, and is easy to understand.
o	General Grammar: All sentences are well constructed and have varied structure and length. The author makes no errors in grammar, mechanics, and/or spelling.
Note that the word counts are quite short (~200 words per section). This does not mean it’s easy! In fact, conveying all the necessary information succinctly requires extra effort. If English is not your first language, you are encouraged to contact the writing center get help writing succinctly and clearly. They schedule 45 minute sessions to go over your writing which can dramatically improve the quality of your project. Plan ahead to schedule this before upcoming deadlines.
The more complete the second draft, the more feedback I’ll be able to provide to ensure an excellent final project. So it’s in your interest to finish as much as possible. In addition to the details from the first draft, I would like to see drafts of the text and figures/tables/etc in each section.
When submitting your your second draft, you can include any questions or comments in the draft (e.g., “I’m planning to do X, but I’m not sure how to organize the data appropriately”) or as a comment in the UBLearns submission webpage. Please do not include these comments in the final submission.
Formatting
The final project will be produced as a RMarkdown Website that includes all the steps necessary to run the analysis and produce the output (figures, tables,etc.). For examples of similar documents, explore the RPubs website.
See the RMarkdown page for ideas on different html output designs. In particular, check out the FlexaDashboard options if you want to include interactive displays.
Figures
Figures (maps and other graphics) are a vital component of scientific communication and you should carefully plan your figures to convey the results of your analysis.
References
You should cite any relevant materials (including data sources and methods) in the text using a standard author-date citation format (e.g. Wilson, 2015) and then described in a References section. You can either compile the references manually (e.g. cutting and pasting the citation into the references section) or use the automated system in RMarkdown explained here. Other citation styles are acceptable as long as they are consistent, complete, and easy to understand.
Resources
Sites with examples of visual display of quantitative information
•	http://www.informationisbeautiful.net
•	http://flowingdata.com
•	https://visual.ly/m/design-portfolio/
•	40 Brilliant and Complex Topics Explained Perfectly By Infographics
•	NY Times Graphics Department
•	Open Data through R: This Task View contains information about using R to obtain, parse, manipulate, create, and share open data. Much open data is available on the web, and the WebTechnologies TaskView addresses how to obtain and parse web-based data. There is obvious overlap between the two TaskViews, so some packages are described on both. There is also a considerable amount of open data available as R packages on CRAN.


---
title: "Predicting the Risk of Stroke in Patients Using R"
author: "New"
date: "2022-12-24"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Introduction

Stroke is a major cause of death and disability worldwide, and it is important to identify risk factors that can help predict the likelihood of a stroke occurring. In this project, we will use R to analyze a dataset containing information on various patient characteristics, including age, gender, hypertension, heart disease, and other factors, to determine whether these factors can be used to predict the risk of stroke. By understanding the relationship between these variables and the risk of stroke, we can potentially develop strategies for preventing or mitigating this serious health condition. The goal of this project is to use R programming to analyze the data and determine which factors are most closely associated with an increased risk of stroke.

# Reading the data
In this code chunk, we are reading in our data from a CSV file using the `read.csv` function from the `readr` package. The data contains information on various patient characteristics, including age, gender, hypertension, and other factors, that we will use to predict the risk of stroke.
```{r}
library(readr)
data <- read.csv("stroke.csv")
head(data)
```

# Summary Statistics
We want to get a sense of the data we are working with, so we will use the `summary` function to get a summary of the data. We can see that the data contains 5110 observations and 12 variables. The variables
```{r}
summary(data)
str(data)
```

# Materials and methods
The dataset was obtained via participation in a Kaggle competition centered on predicting strokes [https://www.kaggle.com/fedesoriano/stroke-prediction-dataset]. The data consists of 5110 observations and 12 variables, including information on patient characteristics such as age, gender, hypertension, and other factors.

To analyze the data, I will use the readr package to read in the data from a CSV file and store it as a data frame in R. I shall then use a variety of R functions and packages, including dplyr, ggplot2, and caret, to perform statistical analyses, visualize the data, and build predictive models.

In order to predict the risk of stroke, I first split the data into training and test sets using the createDataPartition function from the caret package. I trained a logistic regression model on the training set using the glm function, and evaluated the model's performance on the test set using the predict function and a variety of evaluation metrics.

Overall, the analysis included a combination of statistical analyses, data visualization, and machine learning techniques to predict the risk of stroke based on patient characteristics.

## Merging the data
First to demonstrate the use of the `merge` function, I will use another dataset for heart prediction also freely available on kaggle. The `merge` function will allow us to combine the two datasets into one.
### read in heart data
```{r}
heart <- read.csv("heart.csv")
head(heart)
```
## Create a common column
Since we dont have a common variable to merge on, I will create a column id in both datasets to demonstrate the merge function.
```{r}
data$id <- 1:nrow(data)
heart$id <- 1:nrow(heart)
```
```{r}
head(data)
head(heart)
```
## Merge the data
```{r}
merged <- merge(data, heart, by = "id")
head(merged)
```

# Results
Since i have modified the stroke data i was going to use, let me read in the stroke data again as df
```{r}
df <- read.csv("stroke.csv")
head(df)
```

I will make the id column the index column
```{r}
library(dplyr)
df <- df %>% dplyr::select(-id)
head(df)
```

Now i can check whether there are any missing values in the data. I will import all libraries i will need for this project
```{r}
library(mice)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggcorrplot)
library(corrplot)
library("tidyverse")
library("ggplot2")
library("gridExtra")
library("ggpubr")
library("ggcorrplot")
library("corrplot")
library("caret")

```
Check for missing values. The function `summarise_all` will apply the function `sum` to all columns in the data frame. The function `gather` will convert the data frame into a long format, where each row contains a variable name and a value. The function `reorder` will reorder the variable names by the value. The function `coord_flip` will flip the x and y axes. The function `labs` will add labels to the x and y axes.
```{r}
df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key = "variable", value = "missing_count") %>% 
  ggplot(aes(x = reorder(variable, missing_count), y = missing_count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable", y = "Missing count")
```
Or we can view the missing values in a table
```{r}
df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key = "variable", value = "missing_count")
```
Duplicate values
```{r}
df %>% 
  summarise_all(funs(sum(duplicated(.)))) %>% 
  gather(key = "variable", value = "duplicate_count") %>% 
  ggplot(aes(x = reorder(variable, duplicate_count), y = duplicate_count)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable", y = "Duplicate count")
```
This is not right, I dont think we can have these many duplicates.. I will use the table function and the duplicated function as follows:
I will Use the duplicated function to create a logical vector indicating which rows are duplicates. Set the fromLast argument to TRUE to include the first occurrence of each value in the count. Then use the table function to count the number of duplicates for each value.
```{r}
dup <- duplicated(df, fromLast = TRUE)
table(dup)
```
gives dup
FALSE 
 5110 
 This means there are no duplicates in the data

return all column names
```{r}
colnames(df)
```
[1] "gender"            "age"               "hypertension"     
 [4] "heart_disease"     "ever_married"      "work_type"        
 [7] "Residence_type"    "avg_glucose_level" "bmi"              
[10] "smoking_status"    "stroke" 
## Remove rows with missing or invalid values Just in case there are any missing values, I will remove them
```{r}
df <- na.omit(df)
```
# Data Visualization
## Histograms
```{r}
ggplot(df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(x = "Age", y = "Count")
```

```{r}
ggplot(df, aes(x = avg_glucose_level)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(x = "Average Glucose Level", y = "Count")
```

```{r}
ggplot(df, aes(x = bmi)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(x = "BMI", y = "Count")
```
## Bar plots
Creating a bar plot fot gender VS stroke
```{r}
ggplot(df, aes(x=gender,fill=stroke)) + 
  geom_bar(position="dodge")
```
to hide warnings
```{r}
options(warn=-1)
```

#Creating the bar plot for heart disease
ggplot(df1,aes(x=heart_disease,fill=stroke))+geom_bar(position="dodge")

#Creating the bar plot for worktype 
ggplot(df1,aes(x=work_type,fill=stroke))+geom_bar(position="dodge")


#Creating a barplot for the marital status of the people
ggplot(df1,aes(x=ever_married,fill=stroke))+geom_bar(position="dodge")

#Creating a bar plot for the smoking status and the stroke 
ggplot(df1,aes(x=smoking_status,fill=stroke))+geom_bar(position = "dodge")

I want to use a for loop to create a bar plot for all the above variables
```{r}
for (i in 1:ncol(df)) {
  ggplot(df1, aes(x = df[,i], fill = stroke)) +
    geom_bar(position = "dodge") +
    labs(x = colnames(df)[i], y = "Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
```
## Boxplots
```{r}
ggplot(df, aes(x = stroke, y = age)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(x = "Stroke", y = "Age")
```
    
```{r}
ggplot(df, aes(x = stroke, y = avg_glucose_level)) +
    geom_boxplot(fill = "blue", color = "black") +
    labs(x = "Stroke", y = "Average Glucose Level")
```
        
to use different colors for the boxplots
```{r}
ggplot(df, aes(x = stroke, y = avg_glucose_level, fill = stroke)) +
  geom_boxplot(color = "black") +
  labs(x = "Stroke", y = "Average Glucose Level")
```
Correlation matrix
```{r}
cor(df)
```

```{r}
# Machine Learning
## Splitting the dataset 
```{r}
set.seed(101)
```
##creating the test-train split 
```{r}
train.index <- createDataPartition(df$stroke, p = 0.7, list = FALSE)
train.df <- df[train.index, ]
test.df <- df[-train.index, ]
```
## Checking the dimensions of the train and test dataframes
```{r}
dim(train.df)
```
[1] 3579    9
```{r}
dim(test.df)
```
[1] 1531    9
## Logistic Regression
```{r}
logistic_model <- glm(stroke ~ ., data = train.df, family = "binomial")
summary(logistic_model)
```
## Predictions
```{r}
predictions <- predict(logistic_model, newdata = test.df, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)
```
## Confusion Matrix
```{r}
confusionMatrix(predictions, test.df$stroke)
```
## Accuracy
```{r}
mean(predictions == test.df$stroke)
```