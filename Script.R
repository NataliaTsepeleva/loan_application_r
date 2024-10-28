    #IMPORT DATASET

setwd('C:/DataAnalysis/Project') #set working directory, the path to the folder with dataset file
loan_data = read.table('Tsepeleva_dataset.csv', sep=",", header=TRUE) #import dataset from the file
loan_data #display dataset
attach(loan_data) #set database as default  

    #INSTALL NECESSARY PACKAGES

#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("ggrepel")
#install.packages("reshape2")
#install.packages("tseries")
library(tseries)
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(dplyr)
library(ggrepel)
library(tidyverse)

    #CLEAN DATA

#manipulate data in var 'Gender'
unique(loan_data$Gender) #display unique values for 'Gender' variable 
#there is an empty string so delete all the observations where Gender is empty
loan_data = loan_data[loan_data$Gender != "",]

#manipulate data in var 'Self_Employed'
unique(loan_data$Self_Employed) #display unique values for 'Self_Employed' variable
#the same problem her delete observations with empty values in 'Self_Employed'
loan_data = loan_data[loan_data$Self_Employed != "",]

#create some dummy variables
loan_data$Female = as.numeric(loan_data$Gender == 'Female') #1 - female and 0 - male
loan_data$Graduated = as.numeric(loan_data$Education == 'Graduate') #1 - graduated and 0 - not graduated
loan_data$Self_Employed = as.numeric(loan_data$Self_Employed == 'Yes') #1 - self-employed and 0 - not
loan_data$Married = as.numeric(loan_data$Married == 'Yes') #1 - married and 0 - not
loan_data$Loan_Status = as.numeric(loan_data$Loan_Status == 'Y')

#rename variables
names(loan_data)[names(loan_data)=="Loan_Status"] = "Approv_Loan" #now 'Loan_Status' is named 'Approv_Loan'

head(loan_data,5) #display first 5 rows of the dataset to check all the changes

#manipulate data in var 'Dependents'
unique(loan_data$Dependents) #display unique values of the variable 'Dependents'
#There is value '3+' so we change it to just '3'
loan_data$Dependents = ifelse(loan_data$Dependents == "3+", "3", loan_data$Dependents) #delete +
loan_data = loan_data[loan_data$Dependents != "",] #delete observations with missing values

#change data type of the variable
paste(typeof(loan_data$Dependents)) #display the data type of the variable 'Dependents'
loan_data$Dependents <- as.numeric(loan_data$Dependents) #change data type from chr to numeric

#create new variable Total Income as before it was string type with $ character
loan_data$Total_Income = loan_data$ApplicantIncome + loan_data$CoapplicantIncome
#total income is the sum of applicant and coapplicant incomes

#delete missing values
paste("There used to be", nrow(loan_data),"observations") #display how many observation there is initially
loan_data = na.omit(loan_data) #delete mising values
paste("After deleting missing values there are", nrow(loan_data),"observations") #display how many observations there is now

#delete the variables X and Loan_ID as they don't contain any practical information for the analysis
loan_data$X = NULL 
loan_data$Loan_ID = NULL
names(loan_data) #display the names of the columns

    #DESCRIPTIVE STATISTICS

#draw a histogram of the applicants' income
hist(loan_data$ApplicantIncome, breaks = 100, 
     main = "Distribution of the Applicant Income", 
     xlab = "Income,$", ylab = "Number of the applicants")
#there is only few observations with the extreme values of income
#so it will be reasonable to drop these observations as they have a nig influence on the overall statistics of the variable

paste("The mean of the Applicant's income used to be", round(mean(ApplicantIncome), digits = 2),"dollars")
#delete rows(observations) where applicant's income is higher that 20k
loan_data = loan_data[loan_data$ApplicantIncome<20000,] #new dataset with applicants whose income < 20k
paste("After deleting extremely big values the mean is", round(mean(loan_data$ApplicantIncome), digits = 2),"dollars")

#new histogram of the applicants' income
hist(loan_data$ApplicantIncome,
     breaks = 100, main = "Distribution of the Applicants Income less than 20k", 
     xlab = "Income,$", ylab = "Number of the applicants")

#statistics of the variable 'Applicant's income'
summary(loan_data$ApplicantIncome)

#proportion of women in the dataset
table(loan_data$Gender) #displays the number of men and women in the dataset
mean(loan_data$Female) #proportion of women 

#calculate the means of the income for men and women
#create new table with means for male and female
avrincome <- ddply(loan_data, "Gender", summarise, grp.mean=mean(ApplicantIncome))
avrincome #display this table


loan_data$factor_Gender <- as.factor(loan_data$Gender)#create new variable as a Gender factor for the plot
#Plot with Applicant's income and mean for men and women
p<-ggplot(loan_data, aes(x=ApplicantIncome, color=Gender)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=avrincome, aes(xintercept=grp.mean, color=Gender), #add mean lines on the plot
             linetype="dashed")+
  theme(legend.position="top") +
  labs(x = "Income, $", y = "Number of the Applicants", 
       title = "Applicant's Income", subtitle = "For men and women with mean values", color = NULL)
p #display the plot

#statistical test to affirm that men and women have a different income in the dataset
t.test(ApplicantIncome~Female, data=loan_data)

#average of the loan amount
meanLoan = mean(loan_data$LoanAmount) #create new variable with the average value of the loan
paste("The average loan amount is", round(meanLoan, 0), "dollars") #display this variable

#graph with number of men and women in the dataset
ggplot(data = loan_data, #dataset used for the plot
       mapping = aes(Gender, fill = Gender)) + geom_bar() + #values used for the plot
  scale_fill_manual("Gender", values = c("Female" = "#F48FB1", "Male" = "#81D4FA")) + #the name of the legend and colors 
  labs(x = NULL, y = "Number of the Applicants", 
       title = "Number of men and women in the dataset") #names of the axes and plot's name

#graph with number of people with education and not in the dataset
ggplot(data = loan_data, 
       mapping = aes(Education, fill = Education)) + geom_bar() + 
  scale_fill_brewer("Education",palette="Set2") + #for colors we use palette from ggplot package
  labs(x = NULL, y = "Number of the Applicants", 
       title = "Number of applicants with education and not in the dataset") 

#create variables that show the marital status with words
loan_data$married_text = ifelse(loan_data$Married == 1, "Married", "Single")
#graph with number of married applicants in the dataset
ggplot(data = loan_data, 
       mapping = aes(married_text, fill = married_text)) + geom_bar() + 
  scale_fill_brewer("Marital Status", palette = "Set2") +
  labs(x = NULL, y = "Number of the Applicants", 
       title = "Number of married applicants in the dataset")

#create variables that show the self emplyment status with words
loan_data$employ_text = ifelse(loan_data$Self_Employed == 1, "Self employed", "Work in the company")
#graph with number of self employed applicants in the dataset
ggplot(data = loan_data, 
       mapping = aes(employ_text, fill = employ_text)) + geom_bar() + 
  scale_fill_brewer("Employment Status", palette = "Set2") +
  labs(x = NULL, y = "Number of the Applicants", 
       title = "Number of self-employed applicants in the dataset")

table(loan_data$Gender, loan_data$Education) #show how many (wo)men educated and not

#graph with proportion of educated men and women in the dataset
ggplot(data = loan_data, 
       mapping = aes(Gender, fill = Education)) + geom_bar() +
  scale_fill_brewer("Education",palette="Set2") +
  labs(x = NULL, y = "Number of the Applicants", 
       title = "Proportion of educated men and women") 

#Pie chart of variable 'Property_Area'
#data transformation
#create new table with number of observations and % for each area
pie_data <- loan_data %>% #data from our dataset
  group_by(Property_Area) %>% #grouped by the property area
  count() %>% #number of observations for each area
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% #% of each area
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) #variable of labels with %

pie_data #diaply this table

#now create a chart using table 'pie_data'
ggplot(pie_data, aes(x = "", y = perc, fill = Property_Area)) + 
  geom_col() + geom_text(aes(label = labels), #for labels we use var from the 'pie_data'
                         color = "black",
                         position = position_stack(vjust = 0.5), #position of the labels
                         show.legend = FALSE) +
  guides(fill = guide_legend(title = "Property Area")) +
  scale_fill_brewer(palette="Set2")+
  theme_minimal() +
  ggtitle("Area where applicants have their property") +
  coord_polar(theta = "y") + 
  theme_void() #remove axis tick mark labels

#Pie chart of variable 'Dependents'
#create new table with number of observations and % for each number of the dependents in the dataset
pie_data2 <- loan_data %>% 
  group_by(Dependents) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels2 = scales::percent(perc))

pie_data2 #display this table

#create a table to have labels positioning for the chart
df2 <- pie_data2 %>% 
  mutate(csum = rev(cumsum(rev(perc))), 
         pos = perc/2 + lead(csum, 1),
         pos = if_else(is.na(pos), perc/2, pos))

#now create a chart using the data from 'pie_data2' and labels from 'df2'
ggplot(pie_data2, aes(x = "", y = perc, fill = fct_inorder(factor(Dependents)))) + 
  geom_col(width = 1, color = 1) + 
  guides(fill = guide_legend(title = "Number of dependents")) +
  scale_fill_brewer(palette="Set2")+
  ggtitle("Number of dependents that applicants have") +
  coord_polar(theta = "y") + 
  theme_void() +
  geom_label_repel(data = df2,
                 aes(y = pos, label = paste0(labels2)),
                 size = 4.5, nudge_x = 1, show.legend = FALSE)
 
    #ECONOMETRIC MODELS

#the probability of the loan approval 
#according to total income, credit history, marital status, education, employment status, sex
mylogit <- glm(loan_data$Approv_Loan ~ Total_Income + Credit_History + Married + Graduated + Self_Employed + Female, data = loan_data, family = "binomial")
summary(mylogit)

#estimation of simple econometric model 
#Loan_Amount = B0 + B1*Total income + B2*married + B3*graduated + B4*dependents + B5*female
myreg = lm(LoanAmount ~ Total_Income + Married + Graduated +  Dependents + Female, data = loan_data)
summary(myreg)

#draw a histogram with residuals' distribution
hist(myreg$residuals, 
     breaks = 100, main = "Distribution of the residuals", xlab = "ei")

#test if the residuals are normally distributed
jarque.bera.test(myreg$residuals)
#p-value is very low so we reject H0. The residuals are not normally distributed.


#create dataset with only numeric and dummy variables
test_data=loan_data #copy our existing dataset
#delete variables that are not needed anymore
test_data$Gender = NULL 
test_data$Education = NULL
test_data$Property_Area = NULL
test_data$factor_Gender = NULL
test_data$ApplicantIncome = NULL
test_data$CoapplicantIncome = NULL
test_data$Loan_Amount_Term = NULL
test_data$married_text = NULL
test_data$employ_text = NULL

#create table with correlation of the variables
cormat <- round(cor(test_data),2)
cormat #display this table

#Heatmap of the correlation
#function to get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

#function to get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#function to put rows and columns in the same order
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
#to transform table into a format where one column is identifier and others are measuravle var
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#create a heattmap from the previuos transformed table
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#add correlation values on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
