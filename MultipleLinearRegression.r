#Multiple Linear Regression

#1
#Mtcars dataset

#built in data
data(mtcars)
head(mtcars)
str(mtcars)

#model the MLR
model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
#invesitigate the properties of the model
summary(model)

#split data into train and test sets
data.train<- mtcars[1:22,]
data.test<- mtcars[23:32,]

#modelling
relation <-lm(mpg ~ hp +wt+cyl, data = data.train)
summary(relation)

# Prediction
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2))

mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#2
#Class Activity

#Multiple Linear Regression

#Create dataset
Ozone <- c(11,11,11,12,12,13,13,13,13,14)
Solar.R <- c(290,44,320,149,120,137,112,27,238,274)
Wind <- c(9.2,9.7,16.6,12.6,11.5,10.3,11.5,10.3,12.6,10.9)
Temp <- c(66,62,73,74,73,76,71,76,64,68)

data1 <- data.frame(Ozone, Solar.R, Wind, Temp)

head(data1)
str(data1)

#model the MLR
model <- lm(Ozone ~ Solar.R + Wind + Temp, data = data1)

#investigate the properties of the model
summary(model)

#split data into train and test sets (70% train, 30% test)
data.train <- data1[1:7,]
data.test <- data1[8:10,]

#modelling
relation <- lm(Ozone ~ Solar.R + Wind + Temp, data = data.train)
summary(relation)

#Prediction
a <- data.frame(
  Solar.R = data.test$Solar.R,
  Wind = data.test$Wind,
  Temp = data.test$Temp
)

result <- predict(relation, a)
print(round(result, digits = 2))

#Performance measurement
mape <- mean(abs((data.test$Ozone - result)/data.test$Ozone)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")
