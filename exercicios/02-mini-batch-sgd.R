# Com base no modelo implementado abaixo, escreva o `for` loop
# necessário p/ implementar o Mini-batch SGD. 
# O tamanho do batch deve ser especificado por meio de uma variável
# chamada batch_size.


# Data generation ----------------------------------------------

n <- 1000

x <- runif(n)
W <- 0.9
B <- 0.1

y <- W * x + B

# Model definition ---------------------------------------------

model <- function(w, b, x) {
  w * x + b
}

loss <- function(y, y_hat) {
  mean((y - y_hat)^2)
}

# Estimating via SGD ------------------------------------------------------

dl_dyhat <- function(y_hat) {
  2 * (y - y_hat) * (-1)
}

dyhat_dw <- function(w) {
  x
}

dyhat_db <- function(b) {
  1
}

# Inicializando os pesos --------------------------------------------------
# Estou invertendo os pesos, quero ver se há convergência neste método...

w <- 0.1
b <- 0.9

lr <- 0.1

batch_size <- 64

for (step in 1:10000) {
  y_hat <- model(w, b, x)
  
  amostrados <- sort(sample(x = 1:1000, size = batch_size, replace = FALSE))
  
  w <- w - lr * mean(dl_dyhat(y_hat)[amostrados] * dyhat_dw(w)[amostrados])
  b <- b - lr * mean(dl_dyhat(y_hat)[amostrados] * dyhat_db(b))
  
  if (((step %% 10 == 0 & step <= 1000) | step %% 100 == 0) & loss(y, y_hat) > 1E-30) {
    cat("Passo: ", step, "; w: ", w, "; b: ", b, "; Funcao Perda: ", loss(y, y_hat), "\n", sep = "")
  }
}

w
b
