library(keras)
library(tensorflow)
library(dplyr)
library(tfdatasets)
library(ggplot2)

set.seed(19284712)
## Generate Data
# binary outcome
n <- 100000
W <- matrix(rnorm(n*10), ncol=10)
colnames(W) <- paste("W",1:10, sep="")
A <- rbinom(n,1, plogis(0.6*W[,1] +0.4*W[,2] + 0.5*W[,3]+0.3*W[,5]+0.1*W[,8]))
Y <- rbinom(n,1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]*0.4*W[,4]+0.3*W[,9] ))
Y1 <- rbinom(n,1, plogis(1 + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]*0.4*W[,4]+0.3*W[,9] ))
Y0 <- rbinom(n,1,plogis(0 + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]*0.4*W[,4]+0.3*W[,9] ))
dat <- data.frame(W,A,Y)
truePsi <- mean(Y1)-mean(Y0)


# try on sample data
n <- 500
W <- matrix(rnorm(n*10), ncol=10)
colnames(W) <- paste("W",1:10, sep="")
A <- rbinom(n,1, plogis(0.6*W[,1] +0.4*W[,2] + 0.5*W[,3]+0.3*W[,5]+0.1*W[,8]))
Y <- rbinom(n,1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3]*0.4*W[,4]+0.3*W[,9] ))
dat <- data.frame(W,A,Y)
index <- sample(n,0.75*n,replace = FALSE)
train_dat <- dat[index,]
test_dat <- dat[-index,]

# normalize
spec <- feature_spec(train_dat, Y ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()
# build layer
layer <- layer_dense_features(
  feature_columns = dense_features(spec), 
  dtype = tf$float32
)

# build model

build_model <- function() {
  input <- layer_input_from_dataset(train_dat %>% select(-Y))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)
model <- build_model()

history <- model %>% fit(
  x = train_dat %>% select(-Y),
  y = train_dat$Y,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history)


test_predictions <- model %>% predict(test_dat %>% select(-Y))
test_predictions[ ,1]


