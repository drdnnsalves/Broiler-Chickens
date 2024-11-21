# Carregar as bibliotecas necessárias
library(rstan)
library(tidybayes)
library(ggplot2)
library(dplyr)

# Dados de casos e parâmetros
cases <- c(10, 15, 20, 20, 20, 20, 20,
           20, 20, 20, 17, 14, 11, 8)
N <- 20  # população

# Definir o número de dias e o vetor de dias
n_days <- length(cases)
t <- seq(0, n_days, by = 1)
t0 <- 0
t <- t[-1]

# Condições iniciais
i0 <- 10
s0 <- N - i0
r0 <- 0
y0 <- c(S = s0, I = i0, R = r0)

# Dados para o Stan
data_sir <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases)

# Ajuste do modelo Stan
model <- stan_model("sir_estudo.stan")  # Certifique-se de que o arquivo "sir_estudo.stan" esteja no mesmo diretório

# Ajustar o modelo Stan com os dados
fit_sir_negbin <- sampling(model, data = data_sir, iter = 4000, chains = 8, seed = 0)

