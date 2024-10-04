library(ggplot2)     
library(tidyverse)    
library(rstan)       
library(tidybayes)     

rstan_options(auto_write = TRUE)          
options(mc.cores = parallel::detectCores()) 
theme_set(theme_bw())                      

# Exemplo de dados fictícios de casos
cases_data <- data.frame(day = 1:14, cases = c(7, 9, 6, 3, 2, 0, 0,
                                               0, 0, 0, 0, 0, 0, 0)) 


ggplot(data = cases_data) + 
  geom_bar(mapping = aes(x = day, y = cases), stat="identity") +  
  labs(x = "Dias", y = "Número de Casos", title = "Casos ao Longo do Tempo") +  # Rótulos do gráfico
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text = element_text(size = 14) 
  )

# Definindo parâmetros iniciais do modelo SIR
N <- 20                                   # População total
I0 <- cases_data$cases[1]                 # Casos iniciais a partir do primeiro dia
R0 <- 0                                   # Recuperados iniciais (começam em zero)
S0 <- N - I0 - R0                         # Calcula suscetíveis iniciais
y0 <- c(S = S0, I = I0, R = R0)           # Vetor de condições iniciais

# Compilar o modelo SIR em Stan 
model <- stan_model("sir_model.stan") 

# Preparar os dados para Stan
n_days <- nrow(cases_data)      # Número total de dias nos dados
t0 <- 0                         # Dia inicial
ts <- seq(1, n_days, by = 1)    # Sequência de dias
cases <- cases_data$cases       # Casos extraídos dos dados


data_list <- list(n_days = n_days, y0 = y0, t0 = t0, ts = ts, N = N, cases = cases)

# Amostragem usando o modelo SIR
fit <- sampling(model, 
                data = data_list, 
                iter = 1000,   # Número total de iterações
                warmup = 500,  # Período de aquecimento
                chains = 4,    # Número de cadeias de amostra
                seed = 1)      # Semente para reprodutibilidade

# Impressão dos parâmetros amostrados
pars <- c("beta", "gamma", "phi_inv", "R0", "recovery_time")  # Parâmetros a serem exibidos
print(fit, pars = pars)  # Exibe os parâmetros amostrados

# Previsão de casos a partir do modelo SIR
pred_cases <- fit %>%
  spread_draws(pred_cases[n_days]) %>%  # Espalha os desenhos para previsões
  rename(day = n_days) %>%
  left_join(tibble(cases = cases, day = 1:n_days), by = "day") %>%  # Junta com os dados observados
  group_by(day, .chain) %>%  # Agrupa por dia e cadeia
  summarise(cases = mean(cases),  # Calcula média de casos observados
            pred_median = median(pred_cases),  # Mediana das previsões
            pred_9 = quantile(pred_cases, 0.95),  # Percentil 95 das previsões
            pred_1 = quantile(pred_cases, 0.05))  # Percentil 5 das previsões

# Calcular a curva de infectados a partir das previsões do modelo SIR
infected_curves <- fit %>%
  spread_draws(y[day, 2]) %>%  # Extrai a coluna de infectados do modelo
  rename(infected = y) %>%
  group_by(day, .chain) %>%  # Agrupa por dia e cadeia
  summarise(mean_infected = mean(infected))  # Calcula a média de infectados

# Juntar os dados de previsões e infectados
combined_data <- left_join(pred_cases, infected_curves, by = "day")

# Plotar os casos observados e as previsões com nomes maiores
ggplot(combined_data, aes(x = day)) +
  geom_line(aes(y = pred_median, color = "Predição"), size = 1, linetype = "dashed") +  # Linha de predição
  geom_point(aes(y = cases, color = "Casos Observados"), size = 2) +  # Pontos dos casos observados
  labs(x = "Dias", y = "Incidência", title = "Casos Observados e Predição de Casos (Vacinados)") +  # Rótulos do gráfico
  theme_minimal() +  # Tema minimalista
  scale_color_manual(
    name = "Legenda",  # Nome da legenda
    values = c(
      "Predição" = "#E69F00",   # Cor para predições
      "Casos Observados" = "#0072B2"  # Cor para casos observados
    )
  ) +
  theme(
    legend.position = "top",  # Posição da legenda
    legend.title = element_blank(),  # Título da legenda em branco
    panel.grid.minor = element_blank(),  # Remove grade menor
    panel.grid.major = element_line(size = 0.5),  # Tamanho da grade maior
    plot.title = element_text(hjust = 0.5, size = 18),  # Centraliza e aumenta o título
    axis.title.x = element_text(size = 16),  # Aumenta o nome do eixo x
    axis.title.y = element_text(size = 16),  # Aumenta o nome do eixo y
    axis.text = element_text(size = 14),  # Aumenta os números dos eixos
    legend.text = element_text(size = 14)  # Aumenta o texto da legenda
  )

# Extração das amostras a posteriori usando tidybayes
posterior_samples <- fit %>%
  spread_draws(beta, gamma, phi_inv, R0, recovery_time)  # Espalha as amostras a posteriori dos parâmetros

# Visualizar as distribuições a posteriori dos parâmetros com nomes maiores
posterior_samples %>%
  gather(key = "parameter", value = "value", beta, gamma, phi_inv, R0, recovery_time) %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "lightblue", alpha = 0.7) +  # Gráfico de densidade das distribuições
  facet_wrap(~ parameter, scales = "free", ncol = 2) +  # Facetas para cada parâmetro
  labs(title = "Distribuições a Posteriori dos Parâmetros (Vacinados)", x = "Valor", y = "Densidade") +  # Rótulos do gráfico
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),  
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.text = element_text(size = 14) 
  )

# Extraindo os parâmetros estimados
posterior_samples <- fit %>%
  spread_draws(beta, gamma, phi_inv, R0, recovery_time)

# Obter os valores médios dos parâmetros
params <- posterior_samples %>%
  summarize(
    beta = mean(beta),
    gamma = mean(gamma),
    phi_inv = mean(phi_inv),
    R0 = mean(R0),
    recovery_time = mean(recovery_time)
  )
