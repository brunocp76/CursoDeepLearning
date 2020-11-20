# Agora vamos usar bases de dados de verdade!
# Ajuste um MLP com quantas layers e unidades escondidas
# você preferir.

library(keras)

base <- dataset_boston_housing()
str(base)

x <- base$train$x
y <- base$train$y


# Dica do Milhão... Precisa escalar, principalmente os regressores...
x2 <- scale(x)
y2 <- scale(y)

str(x2)
str(y2)

# Model definition ---------------------------------------------

input <- layer_input(shape = 13)

output <- input %>% 
   layer_dense(units = 15, activation = "relu") %>% 
   # layer_dense(units = 11, activation = "relu") %>%
   layer_dense(units = 7, activation = "relu") %>%
   # layer_dense(units = 3, activation = "relu") %>%
   layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = output)

summary(model)

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.0001)
   )

# Model fitting ------------------------------------------------

model %>%
   fit(
      x = x2,
      y = y2,
      batch_size = 2,
      epochs = 150,
      validation_split = 0.25
   )

get_weights(model)
predict(model, x)

# Quick Graphical Check ---------------------------------------------------

z <- dplyr::as_tibble(cbind(y2, predict(model, x2)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(ggplot2::aes(x = y2, y = y_hat)) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::ggtitle("Predicted x Realized") +
   ggplot2::geom_abline(slope = 1, intercept = 0, col = "red")

