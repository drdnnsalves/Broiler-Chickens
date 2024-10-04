library(deSolve)  
library(ggplot2)  

# Definir o modelo SIR sem mortalidade
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I - mu * S  # Equação para suscetíveis
    dI <- beta * S * I - gamma * I - 4 * mu * I  # Equação para infectados
    dR <- gamma * I - mu * R  # Equação para recuperados
    list(c(dS, dI, dR))  # Retornar as derivadas
  })
}

# Parâmetros do modelo
params <- list(beta = params$beta, gamma = params$gamma, mu = 0.00067)  # Adiciona a taxa de mortalidade
N <- 25000  # População total
I0 <- cases_data$cases[1]  # Casos iniciais
R0 <- 0  # Recuperados iniciais
S0 <- N - I0 - R0  # Calcula suscetíveis iniciais
y0 <- c(S = S0, I = I0, R = R0)  # Vetor de condições iniciais

# Tempo de simulação
times <- seq(0, 30, by = 1)  # Simular por 30 dias

# Executar a simulação
output <- ode(y = y0, times = times, func = sir_model, parms = params)

# Converter os resultados em um data.frame
output_df <- as.data.frame(output)

# Calcular o número de vivos
output_df$Vivos <- output_df$S + output_df$I + output_df$R

# Plotar os resultados com nomes maiores
ggplot(data = output_df, aes(x = time)) +
  geom_line(aes(y = S, color = "Suscetíveis")) +
  geom_line(aes(y = I, color = "Infectados")) +
  geom_line(aes(y = R, color = "Recuperados")) +
  geom_line(aes(y = Vivos, color = "Vivos")) +  # Adiciona a curva de Vivos
  labs(title = "Modelo SIR Simulado (Vacinados)", x = "Tempo (dias)", y = "População") +
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
