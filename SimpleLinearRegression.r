#Simple Linear Regression

#1
#data of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function
relation <- lm(y~x)
print(relation)

#2
# Find weight of a person with height 189
x_test <- data.frame(x = 189)
result <- predict(relation,x_test)
print(round(result, digit=2))

#3
# Plot the chart
plot(x, y,
     col = "blue",
     main = "Height & Weight Regression",
     pch = 16,
     xlab = "Height in cm",
     ylab = "Weight in Kg")

abline(lm(y ~ x))

#4
#Creating data frame
data1= data.frame(x,y)

#splitting data into training and testing
data1_train<-data1[1:7,]
data1_test<-data1[8:10,]

# Apply the lm() function
relation <- lm(y~x, data1_train)
print(relation)

#Make prediction
x_text <- data.frame(x= data1_test$x)
result <- predict(relation,x_text)
print(result)

#performance measurement1 1
mape <- mean(abs((data1_test$y -result)/data1_test$y)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#performance measurement 2
actuals_preds <- data.frame(cbind(actuals=data1_test$y,predicteds=result))
mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicteds )/actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#example
library(readxl)
df <- read_excel("C:/Users/User/Downloads/income_happiness.xlsx")
View(df)

#Split data into training (80%) and testing (20%) sets
#Randomly select row indices for training
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Apply the lm() function
relation <- lm(happiness~income, data=train_data)
print(relation)

# Prediction
a <- data.frame(income = test_data$income)
result <- predict(relation, a)

# Plot
plot(test_data$income, test_data$happiness,
     col = "red",
     pch = 16,
     xlab = "income",
     ylab = "happiness")

abline(lm(happiness ~ income, data = train_data))


#5
#Class Activity

#a
#data of experience
x <- c(1,2,3,4,5,6,7,8,9,10)

#data of monthly salary
y <- c(2500,2700,3000,3400,3900,4400,5000,5600,6200,6900)

#Creating data frame
data1 <- data.frame(x,y)

#70% training, 30% testing
data1_train <- data1[1:7,]
data1_test <- data1[8:10,]

#Create the regression model
relation <- lm(y~x, data = data1_train)
print(relation)

#Prediction using test data
x_test <- data.frame(x = data1_test$x)
result <- predict(relation,x_test)
print(result)

#b
#Scatter plot with regression line
plot(x, y,
     col = "blue",
     main = "Experience vs Monthly Salary",
     pch = 16,
     xlab = "Experience (Years)",
     ylab = "Monthly Salary (RM)")

abline(relation)
