#3.0)Relação entre categorias (apenas feminino e masculino) e cor----
'# Teste qui-quadrado'
# Filtrar dados e traduzir 
filt_vendas <- vendas_s2 %>%
  filter(categoria %in% c("Men's Fashion", "Women's Fashion")) %>%
  filter(!is.na(cor)) %>%
  mutate(
    categoria = case_when(
      categoria == "Men's Fashion" ~ "Moda Masculina",
      categoria == "Women's Fashion" ~ "Moda Feminina"
    ),
    cor = case_when(
      cor == "Black" ~ "Preto",
      cor == "Green" ~ "Verde",
      cor == "White" ~ "Branco",
      cor == "Blue" ~ "Azul",
      cor == "Red" ~ "Vermelho",
      cor == "Yellow" ~ "Amarelo"
    )
  )


# Calcular frequência relativa
df_freq <- filt_vendas %>%
  group_by(categoria, cor) %>%
  summarise(freq = n()) %>%
  group_by(categoria) %>%
  mutate(total = sum(freq),
         relative_freq = (freq / total) * 100)

# Criar rótulos para as barras
df_freq <- df_freq %>%
  mutate(legends = str_c(freq, " (", round(relative_freq, 2), "%)"))

# Criar o gráfico
ggplot(df_freq)+
  aes(
    x = fct_reorder(categoria, total, .desc = T),
    y = freq, 
    fill = cor, 
    label = legends
  )+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9), 
    vjust = 0.2, hjust = -0.1, 
    size = 3
  ) +
  labs(x = "Categoria", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Frequência por Cor", limits = c(0, 75)) +
  coord_flip()
ggsave("barras-bi-freq.pdf", width = 158, height = 93, units = "mm")



#3.1)Testes Qui-Quadrado----
tabela_contingencia <- table(filt_vendas$categoria, filt_vendas$cor)
resultado_teste <- chisq.test(tabela_contingencia)
#resultados do teste
print(resultado_teste)
'As variáveis são independentes uma da outra, pelo menos a um nível de significância de 0.05.'