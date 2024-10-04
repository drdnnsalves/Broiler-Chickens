library(ggplot2)    
library(tidyverse)   
library(rstan)        
library(tidybayes)   

# Configurações de rstan e ggplot
rstan_options(auto_write = TRUE)         
options(mc.cores = parallel::detectCores()) 
theme_set(theme_bw())                      

# Exemplo de dados fictícios de casos
cases_data <- data.frame(day = 1:14, cases = c(10, 15, 20, 20, 20, 20, 20,
                                               20, 20, 20, 17, 14, 11, 8)) 

# Visualizar os dados em um gráfico de barras com nomes maiores
ggplot(data = cases_data) + 
  geom_bar(mapping = aes(x = day, y = cases), stat="identity") +  # Cria gráfico de barras
  labs(x = "Dias", y = "Número de Casos", title = "Casos ao Longo do Tempo") +  # Rótulos do gráfico
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),  
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Definindo parâmetros iniciais do modelo SIR
N <- 25  # População total
I0 <- cases_data$cases[1]  # Casos iniciais a partir do primeiro dia
R0 <- 0  # Recuperados iniciais (começam em zero)
S0 <- N - I0 - R0  # Calcula suscetíveis iniciais
y0 <- c(S = S0, I = I0, R = R0)  # Vetor de condições iniciais

# Compilar o modelo SIR em Stan (use seu próprio arquivo .stan)
model <- stan_model("sir_model.stan")  # Carrega o modelo SIR definido em Stan

# Preparar os dados para Stan
n_days <- nrow(cases_data)  # Número total de dias nos dados
t0 <- 0                       # Dia inicial
ts <- seq(1, n_days, by = 1)  # Sequência de dias
cases <- cases_data$cases      # Casos extraídos dos dados

# Cria uma lista de dados para passar para o Stan
data_list <- list(n_days = n_days, y0 = y0, t0 = t0, ts = ts, N = N, cases = cases)

fit <- sampling(model, 
                data = data_list, 
                iter = 1000,  # Aumente o total de iterações
                warmup = 500,  # Mantenha o aquecimento menor que o total
                chains = 4,    
                seed = 1)

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
  labs(x = "Dias", y = "Incidência", title = "Casos Observados e Predição de Casos (Não Vacinados)") +  # Rótulos do gráfico
  theme_minimal() +  
  scale_color_manual(
    name = "Legenda",  
    values = c(
      "Predição" = "#E69F00",  
      "Casos Observados" = "#0072B2"  
    )
  ) +
  theme(
    legend.position = "top", 
    legend.title = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.grid.major = element_line(size = 0.5), 
    plot.title = element_text(hjust = 0.5, size = 18),  
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    legend.text = element_text(size = 14),   
    axis.text = element_text(size = 14)  
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
  labs(title = "Distribuições a Posteriori dos Parâmetros", x = "Valor", y = "Densidade") +  # Rótulos do gráfico
  theme_minimal() +  # Tema minimalista
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
