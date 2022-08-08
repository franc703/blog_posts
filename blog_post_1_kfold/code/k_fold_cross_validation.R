# Load packages
library(ISLR2)

# Drop the missing values
Hitters <- na.omit(Hitters)

# Set up the parameters we need
k <- 10
n <- nrow(Hitters)

# Create the folds: we sample from (1,2,3,..., k) as many times as we have rows

# Create the folds, we will use this in the loop to extract the folds for training and testing
folds <- sample(rep(1:k, length = n))
# Create a matrix where we will deposit the test MSE
cv.kfold <- matrix(NA, k, 1)

# Generate the MSE (Mean Squared Errors) for the test groups
for (j in 1:k) {
  lm.fit <- lm(Salary ~ ., data = Hitters[folds != j, ]) #    Run the model for the training set of folds
  pred <- predict(lm.fit, Hitters[folds == j, ]) #    Get prediction for the test set
  cv.kfold[j] <- mean((Hitters$Salary[folds == j] - pred)^2) #    Calculate the MSE and deposit it in the vector
}

# Average the errors to have the k-fold CV MSE
mean.kfold.error <- apply(cv.kfold, 2, mean) #    Average the k MSE