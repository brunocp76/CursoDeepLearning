# Agora vamos usar bases de dados de verdade!
# Ajuste um MLP com quantas layers e unidades escondidas
# você preferir.

library(keras)

base <- dataset_boston_housing()
str(base)

x <- base$train$x
y <- base$train$y

str(x)
str(y)

# Model definition ---------------------------------------------

input <- layer_input(shape = 13)

output <- input %>% 
   layer_dense(units = 15, activation = "relu") %>% 
   # layer_dense(units = 14, activation = "relu") %>% 
   # layer_dense(units = 13, activation = "relu") %>% 
   layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = output)

summary(model)

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.01)
   )

# Model fitting ------------------------------------------------

model %>%
   fit(
      x = x,
      y = y,
      batch_size = 2,
      epochs = 20
   )

get_weights(model)
predict(model, x)

# Quick Graphical Check ---------------------------------------------------

z <- dplyr::as_tibble(cbind(y, predict(model, x)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(ggplot2::aes(x = y, y = y_hat)) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::ggtitle("Predicted x Realized")
