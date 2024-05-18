library(readr)
pima_data <- read_csv("C:/Users/hp/Downloads/diabetes.csv")

# Check the first few rows
head(pima_data)

#1.Linear Regression
#Theoretical Explanation:
#  Linear regression is used to model the relationship between a dependent variable and one or more independent variables by fitting a linear equation.

#Example:
# Predicting Body Mass Index (BMI) based on Age.

# Linear Regression Model
linear_model <- lm(BMI ~ Age, data = pima_data)
summary(linear_model)

# Plotting
plot(pima_data$Age, pima_data$BMI, main = "Linear Regression\nPredict BMI from Age", xlab = "Age", ylab = "BMI",
     pch = 19, frame = FALSE, col = "blue")
abline(linear_model, col = "red", lwd = 2)
legend("topright", legend=c("Data", "Fit"), col=c("blue", "red"), pch=c(19, NA), lty=c(NA, 1))


#2.Polynomial Regression
#Theoretical Explanation:
#  Polynomial regression extends linear models by adding polynomial terms, which allows the model to fit non-linear relationships.

#Example:
#  Fit a quadratic model to predict BMI from Age.

# Polynomial Regression Model
pima_data$Age2 <- pima_data$Age^2
poly_model <- lm(BMI ~ Age + Age2, data = pima_data)
summary(poly_model)

# Plotting
pima_data$pred_poly <- predict(poly_model, newdata = pima_data)
plot(pima_data$Age, pima_data$BMI, main = "Polynomial Regression\nPredict BMI from Age", xlab = "Age", ylab = "BMI", pch = 19, col = "blue")
points(pima_data$Age, pima_data$pred_poly, type = "l", col = "red", lwd = 2)


#3.Logistic Regression
#Theoretical Explanation:
#  Logistic regression models the probabilities for classification problems with two possible outcomes. It’s used when the response variable is categorical.

#Example:
#  Predicting the presence of diabetes based on BMI.


# Logistic Regression Model
pima_data$diabetes <- ifelse(pima_data$Outcome == 1, 1, 0)

logistic_model <- glm(diabetes ~ BMI, family = binomial, data = pima_data)
summary(logistic_model)

# Plotting
pima_data$prob <- predict(logistic_model, type = "response")
plot(pima_data$BMI, pima_data$diabetes, main = "Logistic Regression\nPredict Diabetes from BMI", xlab = "BMI", ylab = "Diabetes Probability", pch = 19, col = "blue")
points(pima_data$BMI, pima_data$prob, type = "p", pch = 19, col = "red")
