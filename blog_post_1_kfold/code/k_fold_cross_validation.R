# Load packages
library(ISLR2)

# Drop the missing values
Hitters <- na.omit(Hitters)

# Set up the parameters we need
k <- 10
n <- nrow(Hitters)

# Create the folds: we sample from (1,2,3,..., k) as many times as we have rows

folds <- sample(rep(1:k, length = n))
cv.kfold <- matrix(NA, k, 1)

# Generate the MSE (Mean Squared Errors) for the test groups
for (j in 1:k) {
  lm.fit <- lm(Salary ~ ., data = Hitters[folds != j, ])
  pred <- predict(lm.fit, Hitters[folds == j, ])
  cv.kfold[j] <- mean((Hitters$Salary[folds == j] - pred)^2)
}

# Average the errors to have the k-fold CV MSE
mean.kfold.error <- apply(cv.kfold, 2, mean)