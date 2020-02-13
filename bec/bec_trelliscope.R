library(plotly)
library(crosstalk)
library(tidyverse)
library(trelliscopejs)

# Importando dados ------------------------------------------------------------
df_lances_completo <- readRDS('bec/bec_lances.rds') %>% 
  rename(data_hora = data) %>% 
  filter(data_hora < lubridate::ymd('2016-01-01'),
         data_hora >= lubridate::ymd('2011-03-01')) %>% 
  mutate(licitante = toupper(licitante))

df_pregoes_bec <- readRDS('bec/bec_cafe.rds') %>%
  select(id_item, dt_inicio, dt_fim, unidade_compradora, municipio,
         ente_federativo, descricao_item, quantidade, kg_por_unid,
         kg_fornecidos, melhor_lance, win_bid_kg,
         cnpj_fornecedor, nome_fornecedor, num_propostas,
         num_forn_lances, avg_bids_per_bidder)

# Data wrangling --------------------------------------------------------------
# Nesting df_lances no nível do leilão
df_plot_data <- df_lances_completo  %>%
  nest(bids = c(-id_item, -first_bid_kg_defl, -last_bid_kg_defl))

# Joining dados do leilão
df_plot_data <- df_plot_data %>% 
  inner_join(df_pregoes_bec, by = 'id_item') %>% 
  mutate(n_bids = map_dbl(.x = bids, .f = nrow),
         ano = lubridate::year(dt_inicio) %>% as.factor(),
         mes = lubridate::month(dt_inicio) %>% as.factor()) %>%
  arrange(desc(n_bids))

# Apenas leiloes com pelo menos 2 participantes
df_plot_data <- df_plot_data %>% 
  filter(map_lgl(.x = bids, .f = ~ length(unique(.x$licitante)) > 1))

# Criando ranking de participantes, segundo número de lances submetidos
df_plot_data <- df_plot_data %>% 
  mutate(bids = map(.x = bids,
                    .f = ~ count(.x, licitante) %>% 
                      mutate(ranking_bids = row_number(-n)) %>% 
                      rename(n_bids_firm = n) %>%
                      right_join(.x, by = 'licitante')))

# Variavel para legenda ordenando fornecedores pelo numero de lances
df_plot_data <- df_plot_data %>% 
  mutate(bids = map(.x = bids,
                    .f = ~ .x %>% 
                      mutate(Fornecedor = 'Fornecedor ' %>% 
                               str_c(LETTERS[ranking_bids]))))

# Colunas com lances dos 2 fornecedores mais ativos
df_plot_data <- df_plot_data %>% 
  mutate(bidder_no1 = map(.x = bids,
                          .f = ~ filter(.x, ranking_bids == 1)),
         bidder_no2 = map(.x = bids, 
                          .f = ~ filter(.x, ranking_bids == 2)))

# Numero de lances e CNPJ dos 2 fornecedores mais ativos
df_plot_data <- df_plot_data %>% 
  mutate(n_bids_firm1 = map_dbl(.x = bidder_no1, .f = nrow),
         n_bids_firm2 = map_dbl(.x = bidder_no2, .f = nrow),
         nome_bidder1 = map_chr(.x = bidder_no1, .f = ~ .x$licitante[1]),
         nome_bidder2 = map_chr(.x = bidder_no2, .f = ~ .x$licitante[1])) %>% 
  select(-bidder_no1, -bidder_no2) # Não serão mais utilizadas

# Variáveis com mediana e média do incremento normalizado na fase de lances
df_plot_data <- df_plot_data %>% 
  mutate(
    median_inc = 
      map_dbl(.x = bids, .f = ~ median(.x$norm_inc_first, na.rm = TRUE)),
    avg_inc = 
      map_dbl(.x = bids, .f = ~ mean(.x$norm_inc_first, na.rm = TRUE))
  )

# Variavel como nome de todos os fornecedores para usar de filtro
df_plot_data <- df_plot_data %>% 
  mutate(fornecedores = map_chr(.x = bids,
                                .f = ~ .x$licitante %>% 
                                  unique() %>% 
                                  str_c(collapse = ' ; ')))

# Montando gráficos plotly ----------------------------------------------------
source('bec/auction_plotly_bec.R')

df_plot <- df_plot_data %>% 
  mutate(panel = map_plot(.x = bids, .f = ~ auction_plotly_bec(.x)))

# Ajustando cognostics --------------------------------------------------------
df_plot$id_item <- 
  cog(df_plot$id_item,
      default_label = TRUE,
      desc = 'Numero de 20 digitos identificador do leilao')

df_plot$dt_inicio <- 
  cog(df_plot$dt_inicio,
      desc = 'Data e horario de abertura da fase de lances')

df_plot$mes <- 
  cog(df_plot$mes,
      desc = 'Mes em que o leilao foi realizado')

df_plot$melhor_lance <- 
  cog(df_plot$melhor_lance, desc = 'Lance vencedor, tal como informado na ata')

df_plot$kg_fornecidos <-
  cog(df_plot$kg_fornecidos,
      desc = 'Quantidade de cafe negociada no leilao, em quilogramas')

df_plot$kg_por_unid <- 
  cog(df_plot$kg_por_unid,
      desc = 'Quilogramas por unidade de fornecimento utilizada no leilao')

df_plot$municipio <- 
  cog(df_plot$municipio,
      desc = 'Municipio do orgao publico comprador (UASG)')

df_plot$unidade_compradora <- 
  cog(df_plot$unidade_compradora, desc = 'Nome do orgao publico comprador')

df_plot$win_bid_kg <-
  cog(df_plot$win_bid_kg, default_label = TRUE,
      desc = 'Lance vencedor em R$/kg, em preços de dez/2015 (IPCA)')

df_plot$ano <- 
  cog(df_plot$ano, desc = 'Ano de realizacao do leilao')

df_plot$avg_bids_per_bidder <- 
  cog(df_plot$avg_bids_per_bidder,
      desc = 'Numero medio de lances por fornecedor')

df_plot$num_propostas <- 
  cog(df_plot$num_propostas, default_label = FALSE,
      desc = 'Numero de propostas apresentadas')

df_plot$num_forn_lances <- 
  cog(df_plot$num_forn_lances, default_label = TRUE,
      desc = 'Numero de fornecedores que participaram da fase de lances')

df_plot$n_bids <- 
  cog(df_plot$n_bids, default_label = TRUE,
      desc = 'Total de lances registrados no leilao')

df_plot$n_bids_firm1 <- 
  cog(df_plot$n_bids_firm1, default_label = FALSE, 
      desc = str_c('Numero de lances submetidos ',
                   'pelo participante que mais deu lances'))

df_plot$n_bids_firm2 <- 
  cog(df_plot$n_bids_firm2, default_label = FALSE,
      desc = str_c('Numero de lances submetidos ',
                   'pelo segundo participante que mais deu lances'))

df_plot$nome_bidder1 <- 
  cog(df_plot$nome_bidder1,
      desc = 'Nome do fornecedor que mais registrou lances')

df_plot$nome_bidder2 <- 
  cog(df_plot$nome_bidder2,
      desc = 'Nome do segundo fornecedor que mais registrou lances')

df_plot$median_inc <- 
  cog(df_plot$median_inc,
      desc = str_c('Mediana do incremento entre lances de cobertura ',
                   '(incremento normalizado pelo primeiro lance)'))

df_plot$avg_inc <- 
  cog(df_plot$avg_inc,
      desc = str_c('Media do incremento entre lances de cobertura ',
                   '(incremento normalizado pelo primeiro lance)'))

df_plot$fornecedores <- 
  cog(df_plot$fornecedores,
      desc = 'Nomes de todos os participantes da fase aleatória')

df_plot$first_bid_kg_defl <- 
  cog(df_plot$first_bid_kg_defl, default_label = TRUE,
      desc = 'Valor do primeiro lance, em R$/kg, em preços de dez/2015 (IPCA)')

df_plot$last_bid_kg_defl <- 
  cog(df_plot$last_bid_kg_defl, 
      desc = 'Valor do último lance, em R$/kg, em preços de dez/2015 (IPCA)')

# Compilando e salvando -------------------------------------------------------
trelliscope(df_plot, nrow = 1, ncol = 2,
            name = 'BEC', path = 'bec',
            desc = str_c('Leilões eletrônicos de compra de café realizados na',
                         ' BEC-SP entre 01/03/2011 e 31/12/2015'))
