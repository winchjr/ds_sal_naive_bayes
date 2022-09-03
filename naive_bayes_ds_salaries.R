library(caTools) #used for splitting data into training and testing
library(naivebayes) #naive bayes r package
library(dplyr)    # data transformation
library(caret)    # implementing with caret

#optional seed
set.seed(123)

#import data
ds_sal = read.csv("ds_salaries.csv")

#look at data
head(ds_sal) 

#remove all rows which do not have a Machine Learning Engineer, Data Scientist, Data Engineer, or Data Analyst as the job title.
target_titles = c('Machine Learning Engineer', 'Data Scientist', 'Data Engineer', 'Data Analyst')
ds_sal <- subset(ds_sal, (job_title %in% target_titles))

#remove data we are not interested in: work_year, salary, salary_currency, employment_type and X
ds_sal <- subset(ds_sal, select = -c(1, 2, 4, 6, 7))

#factor data that is char
ds_sal <- ds_sal %>%
  mutate(
    experience_level = factor(experience_level),
    salary_in_usd = factor(salary_in_usd),
    remote_ratio = factor(remote_ratio),
    job_title = factor(job_title),
    employee_residence = factor(employee_residence),
    company_location = factor(company_location),
    company_size = factor(company_size)
  )

#look at updated data set
head(ds_sal)

Y = ds_sal[,2] # extract labels from the job titles
msk = sample.split(Y, SplitRatio=3/4)

#look at the data table
table(Y,msk)

#setting the training and testing data, based on the previous sample split
traindata = subset(ds_sal, msk == TRUE)
testdata = subset(ds_sal, msk == FALSE)

x = traindata[,-8]
y = traindata$job_title

#get model
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#get confusion matrix
Predict <- predict(model,newdata = testdata)

#create confusion matrix for verification
confM = table(testdata[ ,"job_title"], Predict)

#create accuracy test
(confM[1,1]+confM[2,2]+confM[3,3]+confM[4,4])/sum(confM)

#plot the data
X <- varImp(model)
plot(X)