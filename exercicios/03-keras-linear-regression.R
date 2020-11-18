# Agora, ao invés de termos somente uma variável `x`, temos 2.
# Escreva o código usando keras para estimar os parâmetros do modelo.

library(keras)

# Data generation ---------------------------------------------------------

n <- 1000

x <- matrix(runif(2*n), ncol = 2)
W <- matrix(c(0.2, 0.7), nrow = 2)
B <- 0.1

y <- x %*% W + B

# Model definition --------------------------------------------------------

input <- layer_input(shape = 2)

output <- layer_dense(input, units = 1, use_bias = TRUE)

model <- keras_model(input, output)

summary(model)

model %>%
   compile(
      loss = loss_mean_squared_error,
      optimizer = optimizer_sgd(lr = 0.01)
   )

# Model fitting -----------------------------------------------------------

model %>%
   fit(
      x = x,
      y = y,
      batch_size = 2,
      epochs = 8
   )

get_weights(model)
predict(model, x)

# Quick Graphical Check ---------------------------------------------------

z <- dplyr::as_tibble(cbind(y, predict(model, x)))

colnames(z) <- c("y", "y_hat")

z %>%
   ggplot2::ggplot(ggplot2::aes(x = y, y = y_hat)) +
   ggplot2::geom_point(size = 0.9) +
   ggplot2::ggtitle("Predicted x Realized") +
   ggplot2::geom_abline(slope = 1, intercept = 0, col = "red")
