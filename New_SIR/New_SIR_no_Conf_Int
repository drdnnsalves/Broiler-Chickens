# Carregar bibliotecas
library(deSolve)  # Para resolver o sistema SIR
library(tidybayes)  # Para trabalhar com amostras
library(ggplot2)
library(purrr)  # Para usar funções como map2

# Definir a população inicial e as condições iniciais
N <- 25000  # Aqui você coloca o valor da população inicial
i0 <- 5  # Número inicial de infectados
s0 <- N - i0  # Número inicial de suscetíveis
r0 <- 0  # Número inicial de recuperados
y0 <- c(S = s0, I = i0, R = r0)  # Condições iniciais

# Extração das distribuições a posteriori de beta e gamma
posterior_samples <- fit_sir_negbin %>%
  spread_draws(beta, gamma) 

# Amostrar valores de beta e gamma (por exemplo, 100 amostras)
n_samples <- 100
sampled_params <- posterior_samples %>%
  sample_n(n_samples) %>%
  select(beta, gamma)

# Função SIR determinística
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# Simulação para cada par de beta e gamma
sir_results <- sampled_params %>%
  mutate(
    simulation = map2(beta, gamma, ~ {
      # Parâmetros do modelo
      parameters <- c(beta = .x, gamma = .y, N = N)
      # Condições iniciais
      state <- c(S = s0, I = i0, R = r0)
      # Resolução do sistema SIR
      as.data.frame(ode(y = state, times = t, func = sir_model, parms = parameters))
    })
  )

# Consolidar as simulações
simulated_data <- sir_results %>%
  unnest(simulation) %>%
  group_by(time) %>%
  summarize(
    S_mean = mean(S),
    I_mean = mean(I),
    R_mean = mean(R)
  )

# Gráfico das simulações sem intervalo de incerteza (sem geom_ribbon)
ggplot(simulated_data, aes(x = time)) +
  geom_line(aes(y = I_mean), color = "red", size = 1, alpha = 0.7) +
  geom_line(aes(y = S_mean), color = "blue", size = 1, alpha = 0.7) +
  geom_line(aes(y = R_mean), color = "green", size = 1, alpha = 0.7) +
  labs(
    title = "Simulações do Modelo SIR com Distribuições Posteriores",
    x = "Tempo (dias)",
    y = "População",
    color = "Componente"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )
