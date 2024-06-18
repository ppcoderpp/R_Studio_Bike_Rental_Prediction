#PROJECT : BIKE RENTALPREDICTION ..
#P : TO PREDICT THE COUNT OF BIKE RENTAL 
#S : TO BUILD RANDOM FOREST Alogrithm

#Imprting Library 
library(randomForest)
library(readxl)

#Importing file & Initial Anlyasis
day1 <- read_excel("day1.xlsx")
day1

head(day1)
tail(day1)
names(day1)
str(day1)
summary(day1)

#Convert categorical variables to factors
day1$season <- as.factor(day1$season)
day1$yr <- as.factor(day1$yr)
day1$mnth <- as.factor(day1$mnth)
day1$holiday <- as.factor(day1$holiday)
day1$weekday <- as.factor(day1$weekday)
day1$weathersit <- as.factor(day1$weathersit)


#Splitting data into training and testing set
set.seed(1)
indexes <- sample(1:nrow(day1), size = 0.7 * nrow(day1))
train_data <- day1[indexes, ]
test_data <- day1[-indexes, ]

#Creating Random Forest Model
rf <- randomForest(cnt ~ ., data = train_data, ntree = 500)
rf

#Making Prediction
predictions <- predict(rf, newdata = test_data)

#Evalutaion
actual <- test_data$cnt
mse <- mean((predictions - actual)^2) # Mean Squared Error
mse

ss_total <- sum((actual - mean(actual))^2)
ss_residual <- sum((actual - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)

r_squared


all_data <- rbind(train_data, test_data) # Combine training and testing data
all_data$predictions <- predict(rf, newdata = all_data)

all_data

write.csv(all_data, file = "dy1_forest.csv", row.names = FALSE)

# Plot actual 'cnt' and predicted 'predictions'
plot(all_data$predictions, all_data$cnt, main = "Actual vs. Predicted Count",
     xlab = "Predicted Count", ylab = "Actual Count", pch = 19, col = "blue")

# Plot histogram of residuals
residuals <- all_data$cnt - all_data$predictions
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "green")


