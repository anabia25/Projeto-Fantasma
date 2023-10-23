library("ggplot2")
library("dplyr")
##Importando os dados
devolucao<- read.csv(".devolução.csv")
vendas<-read.csv(".vendas.csv")

##Arrumando o banco de dados---- 
#Mudando os nomes
colnames(vendas)[] = c("ordem","ordem1", "data da venda", "ID do usuário","ID do produto","nome do produto",
                       "marca","categoria","preço","avaliação","cor","tamanho","ID único","sodeussabe","motivo da devoluçaõ");

#Retirando informações
vendas$ordem1<- NULL
vendas$sodeussabe<-NULL


#Verificando a duplicidade no ID único
tem_dupli<- any(duplicated(vendas$`ID único`))
if(tem_dupli){
  print("Existe duplicidade")
} else {
  print("Não existe duplicidade")
}#existia duplicadas

#excluindo as duplicadas
vendas_s2<- vendas %>% distinct(`ID único`, .keep_all = TRUE)


#conferindo se deu certo
tem_dupli2<- any(duplicated(vendas_s2$`ID único`))
if(tem_dupli2){
  print("Existe duplicidade")
} else {
  print("Não existe duplicidade")
}


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
  summarise(media_preco = mean(preço, na.rm = TRUE))

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


#2.0)Variação do preço por marca----

#2.1)Quadro de resumo----
vendas_s2_na <- vendas_s2[!is.na(vendas_s2$preço) & !is.na(vendas_s2$marca),]
quadro_resumo <- vendas_s2_na %>% 
  group_by(marca) %>% # caso mais de uma categoria
  summarise(Média = round(mean(preço),2),
            `Desvio Padrão` = round(sd(preço),2),
            `Variância` = round(var(preço),2),
            `Mínimo` = round(min(preço),2),
            `1º Quartil` = round(quantile(preço, probs = .25),2),
            Mediana = round(quantile(preço, probs = .5),2),
            `3º Quartil` = round(quantile(preço, probs = .75),2),
            `Máximo` = round(max(preço),2))  %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) 

xtable::xtable(quadro_resumo)



#2.2) Box-Plot 
ggplot(vendas_s2_na) +
aes(x = marca, y = preço) +
geom_boxplot(fill = c("#A11D21"), width = 0.5) +
stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
labs(x = "Marca", y = "Preço") +
theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")