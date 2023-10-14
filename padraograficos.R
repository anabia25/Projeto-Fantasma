# 0. Carregando pacotes ----
source("packages.R")

# 1. Barras/Colunas ----
# 1.1 Colunas com duas frequências ----
# 1.1.1 Univariado ----

classes <- mpg %>%
  filter(!is.na(class)) %>%
  count(class) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(
    x = fct_reorder(class, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "manufacturer", y = "Frequência") +
  theme_estat()

#ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

# 1.1.2 Bivariado ----
trans_drv <- mpg %>%
  mutate(trans = case_when(
    trans %>% str_detect("auto") ~ "auto",
    trans %>% str_detect("manual") ~ "manual"
  )) %>%
  group_by(trans, drv) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )


porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, freq, .desc = T),
    y = freq,
    fill = drv,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência") +
  theme_estat()
#ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

# 1.2 Barras com duas frequências ----
# 1.2.1 Univariado ----

ggplot(classes) +
  aes(
    x = fct_reorder(class, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0, hjust = -.1,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(0, 70, 20), limits = c(0, 70)) +
  labs(x = "manufacturer", y = "Frequência") +
  theme_estat() +
  coord_flip()
#ggsave("barras-uni-freq.pdf", width = 158, height = 93, units = "mm")

# 1.2.2 Bivariado ----
class_drv <- mpg %>%
  group_by(class, drv) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())
View(class_drv)
porcentagens <- str_c(class_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(class_drv$freq, " (", porcentagens, ")"))


ggplot(class_drv) +
  aes(
    x = fct_reorder(class, freq, .desc = T),
    y = freq,
    fill = drv,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.2, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Class", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Speed of cars", limits = c(0, 60)) +
  coord_flip()

#ggsave("barras-bi-freq.pdf", width = 158, height = 93, units = "mm")

# 1.3 Colunas ----
# 1.3.1 Univariado ----
ggplot(mpg) +
  aes(x = class) +
  geom_bar(fill = "#A11D21") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
#ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

# 1.3.2 Univariado com porcentagem no gráfico e no eixo
ggplot(mpg) +
  aes(x = class) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") +
  geom_text(aes(
    y = prop.table(..count..) * 100 + 0.5,
    label = paste0(gsub("\\.", ",", round(prop.table(..count..) * 100, 2)), "%")
  ),
  stat = "count", vjust = 0, size = 4
  ) +
  labs(x = "Classe do automóvel", y = "Frequência Relativa") +
  theme_estat()
#ggsave("colunas-uni-percent.pdf", width = 158, height = 93, units = "mm")

# 1.3.3 Univariado com porcentagem no gráfico e freq absoluta no eixo ----
ggplot(mpg$class %>% vector_frequencies()) +
  aes(
    x = groups,
    y = absolute,
    label = relative
  ) +
  geom_bar(stat = "identity", fill = "#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
#ggsave("colunas-uni-freq-percent.pdf", width = 158, height = 93, units = "mm")

# 1.3.4 Bivariado com dodge ----
class_trans <- as.data.frame(table(mpg$class, mpg$trans))
ggplot(class_trans) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
#ggsave("colunas-bi-dodge.pdf", width = 158, height = 93, units = "mm")

# 1.3.5 Bivariado com stack ----
ggplot(class_trans, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
#ggsave("colunas-bi-stack.pdf", width = 158, height = 93, units = "mm")


# 1.3.6 Bivariado com fill ----
ggplot(class_trans, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
#ggsave("colunas-bi-fill.pdf", width = 158, height = 93, units = "mm")

# 1.3.7 Bivariado com porcentagem ----

trans_class <- table(mpg$trans, mpg$class) %>%
  data.frame() %>%
  mutate(Pct = Freq / sum(Freq))

orderclass <- c(
  "2seater", "compact", "midsize",
  "minivan", "pickup", "subcompact",
  "suv"
)

ggplot(trans_class) +
  aes(
    x = factor(Var2, level = orderclass),
    y = Pct,
    fill = Var1
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Transmissão") +
  scale_y_continuous(
    limits = c(0, .15), expand = c(0, 0), breaks = seq(0, .3, .05),
    labels = paste0(seq(0, 30, 5), "%")
  ) +
  labs(x = "Classe do Automóvel", y = "Frequência Relativa") +
  theme_estat()
#ggsave("colunas-bivariado-percent.pdf", width = 158, height = 93, units = "mm")

# 1.4 Barras ----
# Basta adicionar coord_flip() nos códigos para Colunas

# 2. Setores ----
# 2.1 Com porcentagem ----
contagem <- mpg %>%
  group_by(drv) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(drv)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))


ggplot(contagem) +
  aes(
    x = factor(""),
    y = Prop,
    fill = factor(drv)
  ) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_x_discrete() +
  scale_fill_manual(name = "DRV") +
  theme_estat() +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  )
#ggsave("setor.pdf", width = 158, height = 93, units = "mm")

# Gráfico de roskinha kkk ----

ggplot(contagem, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=drv)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + 
  theme_estat() + 
  theme_void()

# 3. Boxplot ----
# 3.1 Univariado ----
ggplot(mpg) +
  aes(
    x = factor(""),
    y = cty
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()
#ggsave("box_uni.pdf", width = 158, height = 93, units = "mm")

# 3.2 Bivariado ----
ggplot(mpg) +
  aes(
    x = trans,
    y = cty
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Transmissão", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")




# 4. Histograma ----
# 4.1 Univariado ----
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Frequência Absoluta") +
  theme_estat()
#ggsave("hist_uni.pdf", width = 158, height = 93, units = "mm")

# 4.2 Univariado em porcentagem ----
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(
    aes(y = 100 * (..count..) / sum(..count..)),
    colour = "white",
    fill = "#A11D21",
    binwidth = 7
  ) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Porcentagem") +
  theme_estat()
#ggsave("hist_uni_porc.pdf", width = 158, height = 93, units = "mm")

# 4.3 Bivariado com facet grid ----
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  facet_grid(. ~ class) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Frequência") +
  theme_estat() +
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white")
  )
#ggsave("hist_grid.pdf", width = 200, height = 93, units = "mm")

# 5. Dispersão ----
# 5.1 Univariado com poucos pontos sobrepostos ----
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

# 5.2 Univariado com muitos pontos sobrepostos ----
# 5.2.1 geom_jitter ----
ggplot(mpg, aes(x = cyl, y = cty)) +
  geom_jitter(colour = "#A11D21", size = 3) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

# 5.2.2 Alpha ----
ggplot(mpg, aes(x = cyl, y = cty)) +
  geom_point(
    colour = "#A11D21",
    size = 3,
    alpha = 0.3
  ) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

# 5.3 Bivariado ----
mpg$trans <- factor(substr(mpg$trans, 1, nchar(mpg$trans) - 4))

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes(colour = trans)) +
  scale_colour_manual(
    name = "Transmissão", values = c("#A11D21", "#003366"),
    labels = c("Automático", "Manual")
  ) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

# 6. Linhas ----
dados <- tibble(
  ano = c(
    "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
    "2015", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
    "2014", "2015"
  ),
  preco = c(
    2.5, 5.7, 3.4, 3.7, 4.5, 4.8, 4.1, 4.6, 4.8,
    5, 4.5, 6.5, 3.5, 4.6, 4.7, 4.9, 5, 5.5, 3.5,
    7.5
  ),
  produto = c(
    "a", "a", "a", "a", "a", "a", "a", "a", "b",
    "b", "b", "b", "b", "b", "b", "b", "b", "b",
    "b", "b"
  )
)

# 6.1 Univariado ----
ggplot(dados) +
  aes(x = ano, y = preco, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()

# 6.2 Bivariado ----
ggplot(dados) +
  aes(x = ano, y = preco, group = produto, colour = produto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Produto", labels = c("A", "B")) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()

# 7.0 Diagrama de Sankey ----

prop <- mpg |>
  select(trans,class) |>
  count(trans, class) |>
  mutate(proptot = prop.table(n))

# Caso seja necessário ordenar os fatores, utilize a linha abaixo; além de remover
# os # do código ggplot abaixo para o filtro funcionar.

# fct_lvl <- c("Muito","Médio","Pouco","Nada")

p_load(ggalluvial)

ggplot(as.data.frame(prop),
       aes(y = proptot, axis1 = factor(trans
                                       #,level=fct_lvl
       ), axis2 = factor(class
                         #,level=fct_lvl
       ))) +
  geom_alluvium(aes(fill = factor(trans
                                  #,level=fct_lvl
  )), width = 1/12,alpha=.8,show.legend = FALSE) +
  geom_stratum(width = 1/12, fill = "#A11D21", colour = "black",alpha=1) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Trans", "Class"),
                   expand = c(.05, .05),
                   labels = c("Trans", "Class")) +
  scale_fill_manual(values = cores_estat) +
  scale_y_continuous(labels = NULL,
                     name = NULL,
                     breaks = NULL) + theme_estat()
#ggsave("diagrama_de_sankey.pdf", width = 158, height = 93, units = "mm") 

# 8.0 Matriz de correlação ----

p_load(Hmisc,corrplot)

dados <- mpg |> # utilizar apenas valores numéricos!
  select(displ,year,cyl,cty,hwy)
res2 <- rcorr(as.matrix(dados))

# 8.1 Explorar os parâmetros: desta forma, as correlações insignificantes (<0.05) ficam de fora ----
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

# 8.2 Desta forma, ficam com um X ----
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05)

# 8.3 Desta forma, inverte o triângulo ----
corrplot(res2$r, type="lower", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

# 9.0 Treemap ----

p_load(ggfittext,treemapify)

cars <- mtcars
cars$carname <- rownames(cars)
cars <- mutate(cars, cyl = factor(cyl))

ggplot(cars, aes(area = disp, fill = cyl, label = carname)) +
  geom_treemap() +
  geom_treemap_text(
    fontface = "italic",
    colour = "white",
    place = "centre",
    grow = TRUE
  ) + 
  theme_estat()

# 10.0 Clusters ----
# 10.1 K-means ----
# Gráficos necessários para visualização de clusterização k-means

data("mtcars")
df <- scale(mtcars)

p_load(factoextra)

# Graficar o número ótimo de clusters pelo método de elbow ----
fviz_nbclust(df, kmeans, 
             method = "wss", # soma de quadrados totais
             k.max = 10, # máximo de clusters
             nboot = 1000, # Máximo de bootstraps
             barfill = "#A11D21",
             barcolor = "#A11D21",
             linecolor = "#A11D21") +
  geom_vline(xintercept = 4, linetype = 2) +
  theme_estat()

set.seed(150167636)
km.res <- kmeans(df, 4, nstart=25)

aggregate(mtcars, by=list(cluster=km.res$cluster), mean)
mtcars2 <- cbind(mtcars, cluster=km.res$cluster)

# Visualizando os clusters
fviz_cluster(km.res, data=mtcars2,
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_estat())

# 11. Radar ----

devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
library(ggradar)

mtcars_radar <- mtcars |>
  rownames_to_column(var = "group") |>
  mutate(across(-group, ~ rescale(.))) |>
  tail(5) |> # "Indivíduos" demais deixam este gráfico bem feio. Ideal utilizar em torno de 5.
  select(1:10) # "Categorias" demais também poluem este gráfico. Ideal utilizar em torno de 10.

ggradar(mtcars_radar) +
  scale_colour_manual(values = cores_estat) # A função de tema da estat não fica muito legal com esse gráfico; talvez seja melhor não utilizar..
  #theme_estat() 

# ---------------------------------------------------------------------------- #

# 12. Nuvem de palavras ----
if (!require("pacman")) install.packages("pacman")
p_load(readr,stringr,tidyverse,lubridate,wordcloud,RColorBrewer,wordcloud2,tm)

# O Input é um df (xlsx,txt,csv...) com apenas uma coluna, em que cada linha é uma string
# funciona também se cada linha for apenas uma palavra...

df <- read_csv("Conversa do WhatsApp com Gameficação do Time do Hexa 23_2.txt")

df <- as_tibble(df)
colnames(df) <- 'mensagem'

# Bloco Whatsapp - Este bloco deve ser executado se o input é um txt de log de whatsapp ----
# Caso contrário, rode apenas o código da linha 14, removendo o `#`, e ignore as linhas 16 a 23.
# df2 <- df

df$mensagens <- df$mensagem
df2 <- as_tibble(str_subset(df$mensagens,pattern="entrou usando o link de convite deste grupo",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Arquivo de mídia oculto",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="https",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="criou o grupo",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="mudou o nome para",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Mensagem apagada",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Seu código de segurança com",negate=T))

lista <- list()
for (i in 1:nrow(df2)){
  lista[[i]] <- df2[i,1]
}
vetor <- unlist(lista)

# Nuvem de palavras:

docs <- Corpus(VectorSource(vetor))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Estas palavras são filtradas da nuvem. Adicione e exclua conforme necessidade.
# Para uma lista de stopwords genéricas, utilize a linha 50. caso deseje fazer sua propria lista,
# utilize o filtro iniciado na linha 53

filtro <- unlist(read_csv("https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt",
                          col_names = F))

filtro <- c('que','para','com','isso','tem','por','uma','pra','esse','mais',
            'dos','das','essa','tá','nas','nem','sem','aos','sobre','aí','pois',
            'este','esse','dos','ela','pra', 'está', 'mas',"vamos","vai",
            "estão","nossa","foi","nosso","nos","aqui","ainda","meu","ter",
            "porque","nossos","são","vão","você","sua","seu","acima","seja",
            "temos","será","sera","mesmo","pelo","só","so","então","entao",
            "umas","como","dele","fazendo","galera","alguém","alguem","assim",
            "número","numero","qualquer","ser","ficar","fazer","ja","já","nossas",
            "manter","estou","cara","nesse","olha","sair","passar","esses","estas",
            "dar","pela","esta","apenas","pro","souza","silva","chegar","dessa",
            "roberto","outra","sendo","estava","gilson","josé","durante","marcus",
            "marcos","desse","vou","tirar","era","teve","vocês","voces","ontem",
            "também","tbm","tambem","atraves","através","única","unica","costa",
            "consegue","as","às","ás","vem","vêm","null","não","ele","la","lá",
            "sim","boa","estamos","agora","hoje","dia","muito","quem","até",
            "pode","bom","nome","quando","coisa") # adicionar mais conforme necessidade

#rm(df2,docs,dtm,lista,matrix,filtro,i,vetor,words)
#gc()

# rode este bloco apenas se desejar filtrar palavras específicas
df <- df |>
  filter(!(word %in% filtro)) |> # Aplicando filtros
  filter(freq >9) # esta linha coloca um mínimo de frequência para as palavras aparecerem na nuvem. Altere conforme necessidade.

# Gera a núvem
set.seed(150167636) 
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, # Ajuste o parâmetro max.words conforme necessidade. O máximo é 200.
          colors=rev(cores_estat))

#write.table(filtro,"filtradas.txt",row.names = F,col.names = F)

# ---------------------------------------------------------------------------- #

