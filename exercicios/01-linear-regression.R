# Esse script tem uma pequena modificação com relação ao exemplo 01.
# Agora, ao invés de termos somente uma variável `x`, temos 2.
# O objetivo aqui é escrever o SGD de forma a estimar os três parâmetros
# do modelo.


# Data generation ---------------------------------------------------------

n <- 10000

x1 <- runif(n)
x2 <- runif(n)

W1 <- 0.9
W2 <- 0.5
B <- 0.1

y <- W1 * x1 + W2 * x2 + B

# Model definition --------------------------------------------------------

model <- function(w1, w2, b, x1, x2) {
  w1 * x1 + w2 * x2 + b
}

loss <- function(y, y_hat) {
  mean((y - y_hat)^2)
}

# Estimating via SGD ------------------------------------------------------

dl_dyhat <- function(y_hat) {
  2 * (y - y_hat) * (-1)
}

dyhat_dw1 <- function(w1) {
  x1
}

dyhat_dw2 <- function(w2) {
  x2
}

dyhat_db <- function(b) {
  1
}

# escreva o laço que faz a estimativa de todos os parametros w1, w2 e b.

# Inicializando os pesos --------------------------------------------------

w1 <- -100
w2 <- 18

b <- 5000

lr <- 0.1

for (step in 1:5000) {
  y_hat <- model(w1, w2, b, x1, x2)

  indice <- step %% 1000 + 1

  w1 <- w1 - lr * dl_dyhat(y_hat)[indice] * dyhat_dw1(w1)[indice]
  w2 <- w2 - lr * dl_dyhat(y_hat)[indice] * dyhat_dw2(w2)[indice]

  b <- b - lr * dl_dyhat(y_hat)[indice] * dyhat_db(b)

  if (step %% 100 == 0) {
    cat("Passo: ", step, "; w1: ", w1, "; w2: ", w2, "; b: ", b, "; Funcao Perda: ", loss(y, y_hat), "\n", sep = "")
  }
}

w1
w2
b
