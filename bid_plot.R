bid_plot <- function(data, lines = FALSE, bestFit = FALSE) {
  
  my_plot <- data %>%
    plot_ly(
      x = ~ data_hora, y = ~ valor_lance, hoverinfo = "text",
      text = ~ paste(
        "<b>Fornecedor:</b> ", Fornecedor,
        
        "<br><b>Data/Hora:</b> ", data_hora,
        
        "<br><b>Lance:</b> R$", valor_lance %>% formatar_numero(),
        
        "<br><b>Lance/kg:</b> R$",
        round(valor_lance_corr_defl, digits = 4) %>% formatar_numero(),
        
        "<br><b>Desconto total:</b>: R$",
        round(-1*desconto_bruto, digits = 4) %>% formatar_numero(),
        
        "<br><b>Desconto/kg:</b> R$",
        round(-1*increment, digits = 4) %>% formatar_numero(),
        
        "<br><b>Intervalo lance anterior:</b> ",
        round(intervalo_lance_anterior, 4) %>% formatar_numero(), "s",
        
        "<br><b>Intervalo menor lance:</b> ",
        round(intervalo_menor_lance, 4) %>% formatar_numero(), "s",
        
        "<br><b>Intervalo lance proprio:</b> ",
        round(intervalo_lance_proprio, 4) %>% formatar_numero(), "s"
      )
    )  %>%
    
    add_markers(color = ~ Fornecedor,
                opacity = 0.8,
                showlegend = TRUE) %>%
    
    add_segments(x = ~ mean(inicio_fase_aleatoria),
                 xend = ~ mean(inicio_fase_aleatoria),
                 y = ~min(valor_lance),
                 yend = ~max(valor_lance),
                 color = I("red"),
                 opacity = 0.8,
                 showlegend = FALSE) %>%
    
    layout(xaxis = list(title = 'Horario de registro do lance'),
           yaxis = list(title = 'Valor do lance (reais)'))
  
  if (lines) {
    my_plot <- my_plot %>%
      add_lines(color = ~ Fornecedor, opacity = 0.4)
  }
  
  if (bestFit) {
    m <- lm(valor_lance ~ data_hora, data = data)
    my_plot <- my_plot %>%
      add_lines(y = ~ fitted(m), color = I('blue'))
  }
  
  my_plot
  
}