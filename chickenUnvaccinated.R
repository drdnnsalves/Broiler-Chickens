# Carregar as bibliotecas necessárias
library(rstan)

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
fit_sir_negbin <- sampling(model, data = data_sir, iter = 400, chains = 2, seed = 0)

# Converter as amostras em um array
fit_sir_negbin_array <- as.array(fit_sir_negbin)

# Extrair as amostras de beta e gamma
beta_samples <- fit_sir_negbin_array[, , "beta"]
gamma_samples <- fit_sir_negbin_array[, , "gamma"]

# Visualizar as distribuições de beta e gamma
stan_dens(fit_sir_negbin, pars = c("beta", "gamma"), separate_chains = TRUE)

# Salvar as amostras de beta e gamma
save(beta_samples, gamma_samples, file = "beta_gamma_samples.RData")

# Exibir valores médios estimados de beta e gamma
beta_est <- mean(beta_samples)
gamma_est <- mean(gamma_samples)
cat("Beta estimado:", beta_est, "\n")
cat("Gamma estimado:", gamma_est, "\n")
