# Agora vamos usar bases de dados de verdade!
# Ajuste um MLP com quantas layers e unidades escondidas
# você preferir.


# Loading Keras and data --------------------------------------------------

library(keras)

base <- dataset_boston_housing()
str(base)

x <- base$train$x
y <- base$train$y

# Dica do Milhão: Precisa escalar, principalmente os regressores... -------

summary(x)
summary(y)

x2 <- scale(x)
y2 <- scale(y)

str(x2)
str(y2)

summary(x2)
summary(y2)

# Model definition --------------------------------------------------------

input <- layer_input(shape = 13)

output <- input %>% 
   layer_dense(units = 15, activation = "relu", use_bias = TRUE) %>% 
   layer_dense(units = 8, activation = "relu", use_bias = TRUE) %>% 
   layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = output)

summary(model)

# Round 1 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.01)
   )

# Round 1 - Model Fitting -------------------------------------------------

model %>%
   fit(
      x = x2,
      y = y2,
      batch_size = 64,
      epochs = 350,
      validation_split = 0.25
   )

# Round 1 - Quick Graphical Check -----------------------------------------

get_weights(model)
predict(model, x2)

z <- dplyr::as_tibble(cbind(y2, predict(model, x2)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = y2,
         y = y_hat
      )
   ) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::labs(
      x = "Realized",
      y = "Predicted",
      title = "Predicted x Realized",
      subtitle = "After Round 1",
      caption = paste0(
         "R-square: ",
         scales::percent(
            x = cor(predict(model, x2), y2)^2,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_abline(slope = 1, intercept = 0, col = "red")

# Round 2 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.005)
   )

# Round 2 - Model Fitting -------------------------------------------------

model %>%
   fit(
      x = x2,
      y = y2,
      batch_size = 128,
      epochs = 350,
      validation_split = 0.25
   )

# Round 2 - Quick Graphical Check -----------------------------------------

get_weights(model)
predict(model, x2)

z <- dplyr::as_tibble(cbind(y2, predict(model, x2)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = y2,
         y = y_hat
      )
   ) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::labs(
      x = "Realized",
      y = "Predicted",
      title = "Predicted x Realized",
      subtitle = "After Round 2",
      caption = paste0(
         "R-square: ",
         scales::percent(
            x = cor(predict(model, x2), y2)^2,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_abline(slope = 1, intercept = 0, col = "red")

# Round 3 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.001)
   )

# Round 3 - Model Fitting -------------------------------------------------

model %>%
   fit(
      x = x2,
      y = y2,
      batch_size = 256,
      epochs = 350,
      validation_split = 0.25
   )

# Round 3 - Quick Graphical Check -----------------------------------------

get_weights(model)
predict(model, x2)

z <- dplyr::as_tibble(cbind(y2, predict(model, x2)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = y2,
         y = y_hat
      )
   ) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::labs(
      x = "Realized",
      y = "Predicted",
      title = "Predicted x Realized",
      subtitle = "After Round 3",
      caption = paste0(
         "R-square: ",
         scales::percent(
            x = cor(predict(model, x2), y2)^2,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_abline(slope = 1, intercept = 0, col = "red")

