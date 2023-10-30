'Organizando os Dados'
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
