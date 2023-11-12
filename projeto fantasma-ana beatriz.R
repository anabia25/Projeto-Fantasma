if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  ggplot2,dplyr,ggpubr,car
)
library(pacman)
source("packages.R")

##Importando os dados
vendas<-read.csv("vendas.csv")
devolucao<- read.csv("devolução_atualizado.csv")


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
#ggsave("media_fat.mensal_categ.pdf", width = 158, height = 93, units = "mm")


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



#2.2) Box-Plot---- 
ggplot(vendas_s2_na) +
aes(x = marca, y = preço) +
geom_boxplot(fill = c("#A11D21"), width = 0.5) +
stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
labs(x = "Marca", y = "Preço") +
theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
#2.3)ANOVA----
#Teste ANOVA

modelo <- aov(preço ~ marca, data = vendas_s2_na)
mod_test <- lm(preço ~ marca, data = vendas_s2_na)
#nao rejeita a H0, p-valo> 0.05
summary(modelo)


#2.4)analisando os presuposto----
#2.4.1)Normalidade----
shapiro.test(modelo$residuals)
#nosso modelo é linear.
qqnorm(modelo$residuals)
qqline(modelo$residuals)


modelo_ <- lm(preço ~ marca, data = vendas_s2_na)
residuos_padronizados <- rstandard(modelo_)

#QQ-plot dos resíduos padronizados
ggplot(data = data.frame(ResiduosPadronizados = residuos_padronizados), aes(sample = ResiduosPadronizados)) +
  geom_qq(colour = "#A11D21", size = 2) +
  geom_abline(intercept = mean(residuos_padronizados), slope = sd(residuos_padronizados), color = "#0073C2", linetype = "dashed") +
  labs(
    x = "Quantis Teóricos",
    y = "Resíduos Padronizados"
  ) +
  theme_estat()
#ggsave("normalida.pdf", width = 200, height = 93, units = "mm")
'-> como p valor é maior que 0.05, rejeitamos H0, logo aceitamos a normalidade'


#2.4.2)Independência----
plot(modelo$residuals)
'->tende a ser independende '

#2.4.3)Homocedaticidade----
leveneTest(preço ~ marca, data = vendas_s2_na)
'-> como p valor é maior que 0.05, aceitamos a homocedasticidade'


#3.0)Relação entre categorias (apenas feminino e masculino) e cor----
#3.1)grafico entre categorias e cores----
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
    x = fct_reorder(cor, total, .desc = T),
    y = freq, 
    fill = categoria, 
    label = legends
  )+
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9), 
    vjust = 0.2, hjust = -0.1, 
    size = 3
  ) +
  labs(x = "Cores", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Frequência por Categoria", limits = c(0, 75)) +
  coord_flip()
#ggsave("barras-bi-freq.pdf", width = 158, height = 93, units = "mm")


##3.1.2)grafico invertido----

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

class_trans <- as.data.frame(table(filt_vendas$cor, filt_vendas$categoria))
ggplot(class_trans) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Categoria") +
  labs(x = "Cores", y = "Frequência") +
  theme_estat()



#3.1)Testes Qui-Quadrado----
tabela_contingencia <- table(filt_vendas$categoria, filt_vendas$cor)
resultado_teste <- chisq.test(tabela_contingencia)
#resultados do teste
print(resultado_teste)
'As variáveis são independentes uma da outra, pelo menos a um nível de significância de 0.05.'



#4.0)Relação entre preço e avaliação----

vendas_pa <- vendas_s2%>%
  filter(!is.na(preço))%>%
  filter(!is.na(avaliação))

#4.1)quadro de resumos----
quadro(vendas_pa[, c("preço", "avaliação")])

# Criar um boxplot
ggplot(vendas_pa) +
  aes(x = preço, y = avaliação) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

#4.2) grafico de dispersão----
ggplot(vendas_pa) +
  aes(x = avaliação, y = preço) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "Avaliação das Roupa",
    y = "Preço das Roupas em Reais"
  ) +
  theme_estat()
ggsave("graf.disp.pdf", width = 158, height = 93, units = "mm") 


# coeficiente de correlação
corr<- cor(vendas_pa$avaliação, vendas_pa$preço)
corr

#4.3)Tabela de informações do modelo ajustado----
#grafico de dispersão ajustado
modelo_regressao <- lm(avaliação ~ preço, data = vendas_pa)
summary(modelo_regressao)

#4.4)grafico de dispersão ajustado----
ggplot(vendas_pa) +
  aes(x = avaliação, y = preço) +
  geom_point(colour = "#A11D21", size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#003366", se = FALSE) +
  labs(
    x = "Avaliação das Roupa",
    y = "Preço das Roupas em Reais"
  ) +
  theme_estat()
#ggsave("graf.disp.ajust.pdf", width = 158, height = 93, units = "mm")




#4.5) grafico  resíduos versus valores ajustados-----

valores_ajustados <- fitted(modelo_regressao)
residuos <- residuals(modelo_regressao)
dados <- data.frame(Valores_Ajustados = valores_ajustados, Residuos = residuos)


ggplot(dados, aes(x = Valores_Ajustados, y = Residuos)) +
  geom_point(colour = "#A11D21", size = 2) +  
  geom_hline(yintercept = 0, color = "#003366") +  
  labs(
    x = "Valores Ajustados",
    y = "Resíduos"
  ) +
  theme_estat()+
  scale_y_continuous(breaks = seq(-10, 10, 0.5)) 
ggsave("graf.reg.lin1.pdf", width = 158, height = 93, units = "mm")

# 4.6) grafico dos residuos padronizados

residuos_padronizados <- rstandard(modelo_regressao)
dados_qqplot <- data.frame(Residuos_Padronizados = residuos_padronizados)


ggplot(dados_qqplot, aes(sample = Residuos_Padronizados)) +
  stat_qq(color = "#A11D21") + 
  geom_abline(intercept = 0, slope = 1, color = "#003366") +  
  xlab("Quantis Teóricos") +    ylab("Resíduos Padronizados") +  
  theme_estat()
#ggsave("graf.reg.lin2.pdf", width = 158, height = 93, units = "mm")



#4.6)grafico de residuos padronizados vs valores ajustados----
dados_scale_location <- data.frame(Valores_Ajustados = valores_ajustados, Residuos_Padronizados = residuos_padronizados)


ggplot(dados_scale_location, aes(x = Valores_Ajustados, y = sqrt(abs(Residuos_Padronizados)))) +
  geom_point(colour = "#A11D21", size = 2) +  
  geom_smooth(method = "lm", color = "#003366") +  
  labs(
    x = "Valores Ajustados",
    y = "Raiz Quadrada dos Resíduos Padronizados"
  ) +
  theme_estat() +
  scale_y_continuous(breaks = seq(0, 3, 0.5))  # Definir rótulos do eixo y
ggsave("graf.reg.lin3.pdf", width = 158, height = 93, units = "mm")

#4.7)grafico da distancia dos cooks
#distância de Cook
cooks_dist <- cooks.distance(modelo_regressao)
dados_cooks <- data.frame(Cooks_Distancia = cooks_dist, Num_Observacoes = 1:length(cooks_dist))

ggplot(dados_cooks, aes(x = Num_Observacoes, y = Cooks_Distancia)) +
  geom_line(color = "#A11D21", size = 1) +  
  labs(
    x = "Número de Observações",
    y = "Distância de Cook"
  ) +
  theme_estat() 
#ggsave("graf.distancia_cook.pdf", width = 158, height = 93, units = "mm")

#4.7) testes ----
#4.7.1) teste de normalidade----
shapiro.test(modelo_regressao$residuals)

#4.7.2) teste de correlação----

#pearson
cor.test(vendas_pa$preço, vendas_pa$avaliação)

#4.7.3) teste de homocedasticidade ----
#teste de breuch-....
bptest(modelo_regressao)



#5) Frequência de cada tipo de devolução por marca----
#Verificando a duplicidade no ID único
tem_dupli<- any(duplicated(devolucao$Unique.ID))
if(tem_dupli){
  print("Existe duplicidade")
} else {
  print("Não existe duplicidade")
}#existia duplicadas


#5.1) juntando os dois banco dos dados----
vendas_devol <- vendas_s2 %>%
  distinct(`ID único`, .keep_all = TRUE) %>%
  left_join(devolucao, by = c("ID único" = "Unique.ID"))%>%
  filter(!is.na(marca) & !is.na(`Motivo.devolução`))
vendas_devol$`motivo da devoluçaõ` <- NULL


#5.2)grafico de frequencia de marca vs motivo----
trans_drv <- vendas_devol %>%
  mutate(marca = case_when(
    marca %>% str_detect("Gucci") ~ "Gucci",
    marca %>% str_detect("Adidas") ~ "	Adidas",
    marca %>% str_detect("H&M") ~ "H&M",
    marca %>% str_detect("Nike") ~ "Nike",
    marca %>% str_detect("Zara") ~ "Zara"
  )) %>%
  group_by(marca, Motivo.devolução) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )


porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))



ggplot(trans_drv) +
  aes(
    x = fct_reorder(marca, freq, .desc = T),
    y = freq,
    fill = Motivo.devolução,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.2, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Marcas", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Frequência", limits = c(0, 43)) +
  coord_flip()
#ggsave("barras-bi-freq_dev.pdf", width = 158, height = 93, units = "mm")

#5.3)testes----
#5.3.1)teste qui-quadrado----
tabela_contingencia_geral <- table(vendas_devol$Motivo.devolução, vendas_devol$marca)
chisq.test(tabela_contingencia_geral)
#5.4)Tabela de frequencia----

resultado_teste<- chisq.test(tabela_contingencia_geral)
frequencias_esperadas <- resultado_teste$expected
frequencias_esperadas

