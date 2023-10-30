'Organizando os Dados'
library("dplyr")
##Importando os dados
devolucao<- read.csv(".devolução.csv")
vendas<-read.csv("C:\\Users\\beatr\\OneDrive\\Documentos\\ESTAT\\Projeto-Fantasma\\vendas.csv")

##Arrumando o banco de dados---- 
#Mudando os nomes
colnames(vendas)[] = c("ordem","ordem1", "data_da_venda", "ID_do_usuário","ID_do_produto","nome_do_produto",
                       "marca","categoria","preço","avaliação","cor","tamanho","ID_único","sodeussabe","motivo_da_devoluçaõ");

#Retirando informações
vendas$ordem1<- NULL
vendas$sodeussabe<-NULL

#Verificando a duplicidade no ID único
tem_dupli<- any(duplicated(vendas$ID_único))
if(tem_dupli){
  print("Existe duplicidade")
} else {
  print("Não existe duplicidade")
}#existia duplicadas

#excluindo as duplicadas
vendas_s2<- vendas %>% distinct(ID_único, .keep_all = TRUE)


#conferindo se deu certo
tem_dupli2<- any(duplicated(vendas_s2$ID_único))
if(tem_dupli2){
  print("Existe duplicidade")
} else {
  print("Não existe duplicidade")
}

