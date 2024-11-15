# Carregar as bibliotecas necessárias
library(deSolve)
library(ggplot2)

# Carregar os parâmetros estimados de beta e gamma
load("~/params.RData")

# Parâmetros estimados do modelo SIR
params <- list(beta = beta_est, gamma = gamma_est, mu = 0.00067)

# População total e condições iniciais
N <- 25000
I0 <- 7  # Casos iniciais (ajustável conforme necessário)
R0 <- 0  # Recuperados iniciais
S0 <- N - I0 - R0  # Calcula suscetíveis iniciais
y0 <- c(S = S0, I = I0, R = R0)

# Tempo de simulação (30 dias)
times <- seq(0, 30, by = 1)

# Definir o modelo SIR com mortalidade
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I - mu * S  # Equação para suscetíveis
    dI <- beta * S * I - gamma * I - 4 * mu * I  # Equação para infectados
    dR <- gamma * I - mu * R  # Equação para recuperados
    
    # Imprimir o valor de beta a cada dia no console
    cat("Dia:", time, " - Beta:", beta, "\n")
    
    list(c(dS, dI, dR))  # Retornar as derivadas
  })
}

# Executar a simulação com os parâmetros estimados
output <- ode(y = y0, times = times, func = sir_model, parms = params)

# Converter os resultados em um data.frame
output_df <- as.data.frame(output)

# Calcular o número de vivos
output_df$Vivos <- output_df$S + output_df$I + output_df$R

# Plotar os resultados
ggplot(data = output_df, aes(x = time)) +
  geom_line(aes(y = S, color = "Suscetíveis")) +
  geom_line(aes(y = I, color = "Infectados")) +
  geom_line(aes(y = R, color = "Recuperados")) +
  geom_line(aes(y = Vivos, color = "Vivos")) +  # Adiciona a curva de Vivos
  labs(title = "Modelo SIR Simulado (Com Parâmetros Estimados)", x = "Tempo (dias)", y = "População") +
  scale_color_manual(values = c("Suscetíveis" = "blue", "Infectados" = "red", "Recuperados" = "green", "Vivos" = "purple")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16), 
    axis.text = element_text(size = 14),  
    legend.text = element_text(size = 14),  
    legend.title = element_blank()
  )
