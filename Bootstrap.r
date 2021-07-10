#Bootstrap 
#ainda não alterada


###########################################################################################################

library(tidyverse)

library(fmsb)

library(readODS)

library(xlsx)

##########################################################################################################

novmort <- read.ods("novmort.ods")

copi <- novmort
copi <- as.data.frame(copi)
colnames(copi) <- copi[1,]
copi <- copi[-1,]
copi <- copi[,-c(1,2,3,4,5,6,9,10,13,14,15,16,19,20,21,22,23,24,25,26,27,28,29,32,33,34,35,36,37)]

neopcop <-copi %>%  filter(`GRUPO CAUSA`=="NEOPLASIA MALIGNA")

nrow(neopcop)#numero de casos de neoplasia maligna

###############################################################################################################

#criando tabela de casos expostos e nao expostos

copim <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Sim")
#casos de cancer em trabalhadores com exposicao a eletricidade

copim2 <- copi %>% filter(NEOPLASIA=="Sim",copi$`TRABALHANDO DIRETAMENTE COM ELETRICIDADE`=="Não")
#casos de cancer em trabalhadores sem exposicao a eletricidade e ocupacao de baixa periculosidade

cancerel <- table(copim$Nova_classificação_diagnóstico_principal.1)#casos de cancer expostos

cancerel <- cancerel[-1]

cancerel <- as.data.frame(cancerel)#electridade e cancer

cancernel <- table(copim2$Nova_classificação_diagnóstico_principal.1)
#casos de cancer em nao expostos

cancernel <- cancernel[-1]

cancernel <- as.data.frame(cancernel)#cancer em nao expostos

###########################################################################################

tem=FALSE

cancersemelet=NULL

for(i in 1:length(cancerel$Var1)){
  for(j in 1:length((cancernel$Var1))){
    if(cancerel$Var1[i]==cancernel$Var1[j]){
      cancersemelet[i]=cancernel$Freq[j]
      tem=TRUE
    }
  }
  
  if(tem==FALSE){
    cancersemelet[i]==NA
  }
  
  tem=FALSE
} #espelhando os casos de cancer no grupo exposto e nao exposto

cancercsele <- cbind(cancerel,cancersemelet)#cancer com eletricidade e sem

cancercsele <- na.omit(cancercsele)#tirando os valores NA

#############################################################################################################################

newdata=cancercsele

results=NA

for(i in 1:length(cancercsele$Var1)){
  results[i]=0
}#lista de resultados

#Calculando a mor


#A= expostos a eletricidade e sofrem de uma condição

#B=não expostos a eletricidade e sofrem de uma condição

#ao variar apenas A e B,  as outras variáveis necessárias para o calculo da MOR também variam
#abaixo o valor B corresponde ao K

for(m in 1:length(cancercsele$Var1)){
  cancername <- cancercsele$Var1[m]
  
  newdata$Var1 <- if_else(cancercsele$Var1==cancername,1,0)
  #o valor que possui o cancer passa a ser 1
  
  newdata
  
  #calculo da MOR
  
  a=newdata$Freq[newdata$Var1==1]#eletricidade e sem saude a.k.a A
  
  c=sum(cancercsele$Freq)-a#eletricidade e com saude
  
  k=newdata$cancersemelet[newdata$Var1==1]#sem eletricidade e sem saude a.k.a B
  
  d=sum(cancernel$Freq)-k#sem eletricidade e com saude
  
  #####################################################################################################
  
  odd=oddsratio(a,k,c,d,p.calc.by.independence = TRUE)
  #funcao da biblioteca FMSB
  
  odd$estimate#MOR valor
  
###################################################################################################################################
  #Bootstrap
  
  #A= expostos a eletricidade e sofrem de uma condição
  
  #B=não expostos a eletricidade e sofrem de uma condição
  
  #ao variar apenas A e B,  as outras variáveis necessárias para o calculo da MOR também variam
  
  #bootstrapping em A
  
  set.seed(112358)
  
  n <-1#numero de linhas
  
  b <- 10000#numero de amostras bootstrap
  
  variable <-a #numero de casos
  
  bootstrapsamples <- matrix(sample(variable,size=n*b,replace = TRUE),ncol =b ,nrow =n )
  #cada coluna e uma amostra bootstrap
  
  #checando as dimensoes
  
  dim(bootstrapsamples)
  
  odd[[3]]#onde o valor original da MOR esta localizado
  
  #bootstrapping no valor de B
  
  n <-1
  
  b <- 10000
  
  variable <-k #k=B
  
  bootstrapsamples1 <- matrix(sample(variable,size=1*b,replace = TRUE),ncol =b ,nrow =1 )
  

  
  dim(bootstrapsamples1)
  
  
  odd[[3]

  
  bootlist=list(rep(0,b))#lista criada para salvar os valores da funcao da MOR
  
  for(i in 1:b){
    c=sum(cancercsele$Freq)-bootstrapsamples[1,i]
    d=sum(cancernel$Freq)-bootstrapsamples1[1,i]
    
    bootlist[[i]]=oddsratio(bootstrapsamples[1,i],bootstrapsamples1[1,i],c,d,p.calc.by.independence = TRUE)
    
  }#salvando listas em uma lista
  
  
  morboot=rep(0,b)
  #lista para salvar apenas os valores da MORBoot
  
  for(i in 1:b){
    morboot[i]=bootlist[[i]]$estimate
  }
  
  results[m]=list(c("conf"=round(quantile(morboot,c(0.025,0.975)),4),"MOR"=mean(morboot),"PVALUE"=mean((round(morboot,0)==1))))#confidence interval boot 95%
  
}

names(results)=cancercsele$Var1

print("cheque a lista chamada results")

#################################################################################################################


#criando tabelas

#tabela bootstrap

cases=names(results)
confintsup=c(NULL)
confintinf=c(NULL)
estimative=c(NULL)
pvalue=c(NULL)

results

for(i in 1:length(results)){
  
  
  confintsup[i]=round(results[[i]][["conf.97.5%"]],2)
  
  confintinf[i]=round(results[[i]][["conf.2.5%"]],2)
  
  
  estimative[i]=round(results[[i]][["MOR"]],2)
  
  
  pvalue[i]=round(results[[i]][["PVALUE"]],4)
  
}


tabelaboot=NULL

tabelaboot=cbind(cases,confintinf,confintsup,estimative,pvalue)

tabelaboot <- as.data.frame(tabelaboot)


tabelaboot$confint <-paste(tabelaboot$confintinf,tabelaboot$confintsup,sep="-") 

tabelaboot$confint <-paste("(",tabelaboot$confint,")",sep=" ") 

tabelaboot$Amplitude <- as.numeric(as.character(tabelaboot$confintsup))-as.numeric(as.character(tabelaboot$confintinf))
#R esta lendo os dados como fatores entao para consertar e necessario converter
#para caractere e depois numerico

tabelaboot <- tabelaboot[,-c(2,3)]

tabelaboot <- tabelaboot[,c(1,2,4,5,3)]

colnames(tabelaboot) <- c("Casos","Mor","IC-95%","Amplitude","P-valor")

tabelaboot$Casos

tabelaboot$Casos <- c("Bexiga","Colorretal","Esôfago","Estômago","Fígado","Laringe","Linfoma não Hodgkin","Mama","Osso","Pâncreas","Pele","Próstata","Pulmão","Rim")


#####################################################################################################################################################################

mean(tabelaboot$Amplitude)
median(tabelaboot$Amplitude)

sd(tabelaboot$Amplitude, na.rm=TRUE)/
  mean(tabelaboot$Amplitude, na.rm=TRUE)*100
