# Agora vamos usar bases de dados de verdade!
# Ajuste um MLP com quantas layers e unidades escondidas
# você preferir para prever se uma casa será vendida por
# mais de 25k.

# Loading Keras and data --------------------------------------------------

library(keras)

base <- dataset_boston_housing()
x <- base$train$x
y <- base$train$y
y <- as.numeric(y > 25)


# Dica do Milhão: Precisa escalar, principalmente os regressores... -------

summary(x)
summary(y)

x2 <- scale(x)
# y2 <- scale(y)
y2 <- y

str(x2)
str(y2)

summary(x2)
summary(y2)

# Model definition --------------------------------------------------------

input <- layer_input(shape = 13)

output <- input %>%
   layer_dense(units = 8, activation = "softmax", use_bias = TRUE) %>% 
   layer_dense(units = 1)

model <- keras_model(inputs = input, outputs = output)

summary(model)

# Round 1 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_binary_crossentropy,
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

colnames(z) <- c("realized", "predicted")

cdf_under <- ecdf(z$predicted[z$realized == 0])
cdf_over  <- ecdf(z$predicted[z$realized == 1])

# find min and max statistics to draw line between points of greatest distance
minMax <- seq(
   min(z$predicted),
   max(z$predicted),
   length.out = length(z$predicted[z$realized == 0])) 
x0 <- minMax[which(
   abs(cdf_under(minMax) - cdf_over(minMax)) == max(
      abs(cdf_under(minMax) - cdf_over(minMax)))
)] 
y0 <- cdf_under(x0) 
y1 <- cdf_over(x0) 

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = predicted,
         group = realized,
         color = realized
      )
   ) +
   ggplot2::stat_ecdf(
      size = 1,
      show.legend = FALSE
   ) +
   ggplot2::labs(
      y = "CDF",
      title = "Separation",
      subtitle = "After Round 1",
      caption = paste0(
         "KS: ",
         scales::percent(
            x = y0 - y1,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_segment(
      ggplot2::aes(
         x = x0[1],
         y = y0[1],
         xend = x0[1],
         yend = y1[1]
      ),
      linetype = "dashed",
      color = "red",
      show.legend = FALSE
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y0[1]
      ),
      color = "red"
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y1[1]
      ),
      color = "red"
   )


# Round 2 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_binary_crossentropy,
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

colnames(z) <- c("realized", "predicted")

cdf_under <- ecdf(z$predicted[z$realized == 0])
cdf_over  <- ecdf(z$predicted[z$realized == 1])

# find min and max statistics to draw line between points of greatest distance
minMax <- seq(
   min(z$predicted),
   max(z$predicted),
   length.out = length(z$predicted[z$realized == 0])) 
x0 <- minMax[which(
   abs(cdf_under(minMax) - cdf_over(minMax)) == max(
      abs(cdf_under(minMax) - cdf_over(minMax)))
)] 
y0 <- cdf_under(x0) 
y1 <- cdf_over(x0) 

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = predicted,
         group = realized,
         color = realized
      )
   ) +
   ggplot2::stat_ecdf(
      size = 1,
      show.legend = FALSE
   ) +
   ggplot2::labs(
      y = "CDF",
      title = "Separation",
      subtitle = "After Round 2",
      caption = paste0(
         "KS: ",
         scales::percent(
            x = y0 - y1,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_segment(
      ggplot2::aes(
         x = x0[1],
         y = y0[1],
         xend = x0[1],
         yend = y1[1]
      ),
      linetype = "dashed",
      color = "red",
      show.legend = FALSE
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y0[1]
      ),
      color = "red"
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y1[1]
      ),
      color = "red"
   )

# Round 3 - Model Specification -------------------------------------------

model %>%
   compile(
      loss = loss_binary_crossentropy,
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

colnames(z) <- c("realized", "predicted")

cdf_under <- ecdf(z$predicted[z$realized == 0])
cdf_over  <- ecdf(z$predicted[z$realized == 1])

# find min and max statistics to draw line between points of greatest distance
minMax <- seq(
   min(z$predicted),
   max(z$predicted),
   length.out = length(z$predicted[z$realized == 0])) 
x0 <- minMax[which(
   abs(cdf_under(minMax) - cdf_over(minMax)) == max(
      abs(cdf_under(minMax) - cdf_over(minMax)))
)] 
y0 <- cdf_under(x0) 
y1 <- cdf_over(x0) 

z %>%
   ggplot2::ggplot(
      ggplot2::aes(
         x = predicted,
         group = realized,
         color = realized
      )
   ) +
   ggplot2::stat_ecdf(
      size = 1,
      show.legend = FALSE
   ) +
   ggplot2::labs(
      y = "CDF",
      title = "Separation",
      subtitle = "After Round 3",
      caption = paste0(
         "KS: ",
         scales::percent(
            x = y0 - y1,
            accuracy = 0.01),
         sep = ""
      )
   ) +
   ggplot2::geom_segment(
      ggplot2::aes(
         x = x0[1],
         y = y0[1],
         xend = x0[1],
         yend = y1[1]
      ),
      linetype = "dashed",
      color = "red",
      show.legend = FALSE
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y0[1]
      ),
      color = "red"
   ) +
   ggplot2::geom_point(
      ggplot2::aes(
         x = x0[1],
         y = y1[1]
      ),
      color = "red"
   )
