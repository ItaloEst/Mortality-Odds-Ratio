###################################################
#MOR GERAL

###########################################################################################################

### MOR FAIXA ETÁRIA
#Da PARA FAZER DUAS FUNCOES AQUI, UMA PARA A FAIX ETARIA E OUTRA PARA A MOR

classes_5 = c("Até 20 anos","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","Mais de 95")
#

brk_5 = 0
brk_5[1] = 0

faixa_5 = brk_5[2:17]<-seq(20,95,5)#aqui sao apenas colunas numericas?
#idade inicial - idade final - amplitude  do intervalo
brk_5[18] = 120#aqui?

faixa_etaria_5 = data.frame(table(cut(as.numeric(neopcop$`IDADE DO OBITO`),breaks=brk_5,right=FALSE,labels=classes_5)))
#divisao e criando faixas etarias

######################################################3

#CALCULO DA MOR
absol_mor_faixa = faixa_etaria_5
outras_mor_faixa = NULL
total_mor_faixa = NULL

for (i in 1:nrow(absol_mor_faixa)){
  outras_mor_faixa[i] = nrow(neopcop) - absol_mor_faixa$Freq[i]
  total_mor_faixa[i] = absol_mor_faixa$Freq[i]/outras_mor_faixa[i]
}

absol_mor_faixa$outras_mor_faixa = outras_mor_faixa
absol_mor_faixa$total_mor_faixa = total_mor_faixa

Mor_faixa = matrix(nrow = 17, ncol = 17)

for (i in 1:nrow(absol_mor_faixa)) {
  for (j in 1:nrow(absol_mor_faixa)) {
    Mor_faixa[i,j] = absol_mor_faixa$total_mor_faixa[j]/absol_mor_faixa$total_mor_faixa[i]
  }
}
Mor_faixa = data.frame(Mor_faixa)
rownames(Mor_faixa) = classes_5
colnames(Mor_faixa) = classes_5
Mor_faixa = round(Mor_faixa, digits = 3)

### IC MOR FAIXA ETÁRIA

ic_mor_faixa = matrix(nrow = 17, ncol = 17)
log_mor_faixa = matrix(nrow = 17, ncol = 17)
var_log_mor_faixa = matrix(nrow = 17, ncol = 17)
limite_inf_mor_faixa = matrix(nrow = 17, ncol = 17)
limite_sup_mor_faixa = matrix(nrow = 17, ncol = 17)

for (i in 1:nrow(absol_mor_faixa)) {
  for (j in 1:nrow(absol_mor_faixa)) {
    log_mor_faixa[i,j] = log(Mor_faixa[i,j])
    var_log_mor_faixa[i,j] = ((1/absol_mor_faixa$Freq[i])+(1/absol_mor_faixa$outras_mor_faixa[i])+(1/absol_mor_faixa$Freq[j])+(1/absol_mor_faixa$outras_mor_faixa[j]))    
    limite_inf_mor_faixa[i,j] = exp((log_mor_faixa[i,j]) - (1.96*(sqrt(var_log_mor_faixa[i,j]))))
    limite_sup_mor_faixa[i,j] = exp((log_mor_faixa[i,j]) + (1.96*(sqrt(var_log_mor_faixa[i,j]))))
  }
}

limite_inf_mor_faixa = round(limite_inf_mor_faixa,digits = 3)
limite_sup_mor_faixa = round(limite_sup_mor_faixa,digits = 3)

CLASSES = classes_5
MOR = unlist(Mor_faixa[1,])
IC = paste("[",limite_inf_mor_faixa[1,],";",limite_sup_mor_faixa[1,],"]")

TAB_MOR_faixa = data.frame(MOR, IC)

