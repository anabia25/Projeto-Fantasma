#1)Faturamento anual po categoria----

#1.1) Grafico de faturamento por categoria----

#Tratando os dados
faturamento_por_categoria <- vendas_s2 %>%
  group_by(categoria) %>%
  summarise(faturamento = sum(preço, na.rm = TRUE)) %>%
  arrange
#view(faturamento_por_categoria)

faturamento_por_categoria <- na.omit(faturamento_por_categoria)#tirando os NA
faturamento_por_categoria <- faturamento_por_categoria %>%
  mutate(
    categoria = case_when(
      categoria == "Men's Fashion" ~ "Moda Masculina",
      categoria == "Women's Fashion" ~ "Moda Feminina",
      categoria == "Kids' Fashion" ~ "Moda Infantil",
    )
  )

#total do faturamento
total_faturamento <- sum(faturamento_por_categoria$faturamento)

#porcentagem de faturamento paras categorias em relação ao total 
faturamento_por_categ <- faturamento_por_categoria %>%
  mutate(porcentagem = (faturamento / total_faturamento) * 100)


#valor em reais e a porcentagem
labels <- faturamento_por_categ %>%
  mutate(
    valor_reais = scales::dollar(faturamento) %>% str_replace_all(",", "."), # Trocar vírgula por ponto
    porcentagem_label = scales::percent(porcentagem / 100) %>% str_replace_all("\\.", ","),  # Trocar ponto por vírgula
    label = str_c(valor_reais, " (", porcentagem_label, ")") %>% str_squish()
  )

#gráfico
ggplot(faturamento_por_categ, aes(
  x = fct_reorder(categoria, faturamento, .desc = TRUE),
  y = faturamento,
  label = labels$label
)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    data = labels,
    aes(y = faturamento + 100, label = label),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Categoria", y = "Faturamento") +
  theme_estat()
#ggsave("fatura-categoria.pdf", width = 158, height = 93, units = "mm")


#1.2)Grafico da analise mensal de faturamento por categoria----
#arrumando os dados
vendas_hit <- vendas_s2 %>% 
  select(`data da venda`, categoria, preço) %>%
  mutate(`data da venda` = as.Date(`data da venda`, format = "%m/%d/%Y"),
         `data da venda` = format(`data da venda`, "%d/%m/%Y")) %>%
  na.omit() %>%
  mutate(categoria = case_when(
    categoria == "Men's Fashion" ~ "Moda Masculina",
    categoria == "Women's Fashion" ~ "Moda Feminina",
    categoria == "Kids' Fashion" ~ "Moda Infantil"
  )) %>%
  mutate(mes = month(`data da venda`))

vendas_hit$`data da venda` <- NULL
# Agrupar por categoria e mês\calcular a média do preço
venda_hit <- vendas_hit %>%
  group_by(categoria, mes) %>%
  summarise(media_preco = sum(preço, na.rm = TRUE))

ggplot(venda_hit) +
  aes(x = mes, y = media_preco, group = categoria, colour = categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", labels = c("Moda Feminina", "	
Moda Masculina","Moda Infantil")) +
  labs(x = "Mês", y = "Preço") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Fev", "Mar", "Abr", "Mai", 
                                               "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  theme_estat()
ggsave("media_fat.mensal_categ.pdf", width = 158, height = 93, units = "mm")

