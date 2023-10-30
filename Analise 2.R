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
#view(vendas_s2_na)

#2.2) Box-Plot 
ggplot(vendas_s2_na) +
  aes(x = marca, y = preço) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

#2.3)Teste da ANOVA----

modelo <- aov(preço ~ marca, data = vendas_s2_na)
mod_test <- lm(preço ~ marca, data = vendas_s2_na)
#nao rejeita a H0, p-valo> 0.05

# Resumo do modelo ANOVA
summary(modelo)
SQtotal <- sum((vendas_s2_na$preço - mean(vendas_s2_na$preço))^2)
SQtotal

#analisando os presuposto
'Normalidade'
shapiro.test(modelo$residuals)
#nosso modelo é linear.
qqnorm(modelo$residuals)
qqline(modelo$residuals)
'-> como p valor é maior que 0.05, rejeitamos H0, logo aceitamos a normalidade'

'Independência'
plot(modelo$residuals)
'->tende a ser independende '

'Homocedaticidade'
#install.packages("car")
library(car)
leveneTest(preço ~ marca, data = vendas_s2_na)
'-> como p valor é maior que 0.05, aceitamos a homocedasticidade'

