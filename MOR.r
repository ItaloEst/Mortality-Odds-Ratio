###################################################
#MOR GERAL

###########################################################################################################

### MOR FAIXA ETÁRIA

classes_5 = c("Até 20 anos","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","Mais de 95")
brk_5 = 0
brk_5[1] = 0
faixa_5 = brk_5[2:17]<-seq(20,95,5)
brk_5[18] = 120
faixa_etaria_5 = data.frame(table(cut(COVIDBAHIA1$IDADE,breaks=brk_5,right=FALSE,labels=classes_5)))
absol_mor_faixa = faixa_etaria_5
outras_mor_faixa = NULL
total_mor_faixa = NULL

for (i in 1:nrow(absol_mor_faixa)){
  outras_mor_faixa[i] = nrow(COVIDBAHIA1) - absol_mor_faixa$Freq[i]
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

write.csv2(TAB_MOR_faixa,"C:/Users/Visitor/Desktop\\tabela MOR.csv")


###############################################################################################################

### MOR GÊNERO

absol_mor_genero = data.frame(table(COVIDBAHIA1$SEXO))
outras_mor_genero = NULL
chance_mor_genero = NULL
for (i in 1:nrow(absol_mor_genero)) {
  outras_mor_genero[i] = nrow(COVIDBAHIA1) - absol_mor_genero$Freq[i]
  chance_mor_genero[i] = absol_mor_genero$Freq[i]/outras_mor_genero[i]
}
absol_mor_genero$outras_mor_genero = outras_mor_genero
absol_mor_genero$chance_mor_genero = chance_mor_genero

Mor_genero = matrix(nrow = 2, ncol = 2)

for (i in 1:nrow(absol_mor_genero)) {
  for (j in 1:nrow(absol_mor_genero)) {
    Mor_genero[i,j] = absol_mor_genero$chance_mor_genero[j]/absol_mor_genero$chance_mor_genero[i]
  }
}
Mor_genero = data.frame(Mor_genero)
rownames(Mor_genero) = c("Feminino","Masculino")
colnames(Mor_genero) = c("Feminino","Masculino")
Mor_genero = round(Mor_genero, digits = 3)

### IC MOR GÊNERO
```{r,echo=FALSE}
ic_mor_genero = matrix(nrow = 2, ncol = 2)
log_mor_genero = matrix(nrow = 2, ncol = 2)
var_log_mor_genero = matrix(nrow = 2, ncol = 2)
limite_inf_mor_genero = matrix(nrow = 2, ncol = 2)
limite_sup_mor_genero = matrix(nrow = 2, ncol = 2)

for (i in 1:nrow(absol_mor_genero)) {
  for (j in 1:nrow(absol_mor_genero)) {
    log_mor_genero[i,j] = log(Mor_genero[i,j])
    var_log_mor_genero[i,j] = ((1/absol_mor_genero$Freq[i])+(1/absol_mor_genero$outras_mor_genero[i])+(1/absol_mor_genero$Freq[j])+(1/absol_mor_genero$outras_mor_genero[j]))    
    limite_inf_mor_genero[i,j] = exp((log_mor_genero[i,j]) - (1.96*(sqrt(var_log_mor_genero[i,j]))))
    limite_sup_mor_genero[i,j] = exp((log_mor_genero[i,j]) + (1.96*(sqrt(var_log_mor_genero[i,j]))))
  }
}

limite_inf_mor_genero = round(limite_inf_mor_genero,digits = 3)
limite_sup_mor_genero = round(limite_sup_mor_genero,digits = 3)

for (i in 1:2) {
  for (j in 1:2) {
    limite_inf_mor_genero[i,j]
    limite_sup_mor_genero[i,j]
    ic_mor_genero[i,j] = paste("[",limite_inf_mor_genero[i,j],";",limite_sup_mor_genero[i,j],"]")
  }
}

MOR = unlist(Mor_genero[1,])
IC = paste("[",limite_inf_mor_genero[1,],";",limite_sup_mor_genero[1,],"]")
TAB_MOR_ge = data.frame(MOR, IC)

###########################################################################################################

### MOR SISTEMA   
absol_mor_sistema = data.frame(table(COVIDBAHIA1$TIPO.ORGAO))
absol_mor_sistema = absol_mor_sistema[-1,]
outras_mor_sistema = NULL
chance_mor_sistema = NULL
for (i in 1:nrow(absol_mor_sistema)) {
  outras_mor_sistema[i] = sum(absol_mor_sistema$Freq) - absol_mor_sistema$Freq[i]
  chance_mor_sistema[i] = absol_mor_sistema$Freq[i]/outras_mor_sistema[i]
}
absol_mor_sistema$outras_mor_sistema = outras_mor_sistema
absol_mor_sistema$chance_mor_sistema = chance_mor_sistema

Mor_sistema = matrix(nrow = 3, ncol = 3)

for (i in 1:nrow(absol_mor_sistema)) {
  for (j in 1:nrow(absol_mor_sistema)) {
    Mor_sistema[i,j] = absol_mor_sistema$chance_mor_sistema[j]/absol_mor_sistema$chance_mor_sistema[i]
  }
}
Mor_sistema = data.frame(Mor_sistema)
rownames(Mor_sistema) = c("Filatrópico","Privado","Público")
colnames(Mor_sistema) = c("Filatrópico","Privado","Público")
Mor_sistema = round(Mor_sistema, digits = 3)

```

### IC MOR SISTEMA
```{r,echo=FALSE}
ic_mor_sistema = matrix(nrow = 3, ncol = 3)
log_mor_sistema = matrix(nrow = 3, ncol = 3)
var_log_mor_sistema = matrix(nrow = 3, ncol = 3)
limite_inf_mor_sistema = matrix(nrow = 3, ncol = 3)
limite_sup_mor_sistema = matrix(nrow = 3, ncol = 3)

for (i in 1:nrow(absol_mor_sistema)) {
  for (j in 1:nrow(absol_mor_sistema)) {
    log_mor_sistema[i,j] = log(Mor_sistema[i,j])
    var_log_mor_sistema[i,j] = ((1/absol_mor_sistema$Freq[i])+(1/absol_mor_sistema$outras_mor_sistema[i])+(1/absol_mor_sistema$Freq[j])+(1/absol_mor_sistema$outras_mor_sistema[j]))    
    limite_inf_mor_sistema[i,j] = exp((log_mor_sistema[i,j]) - (1.96*(sqrt(var_log_mor_sistema[i,j]))))
    limite_sup_mor_sistema[i,j] = exp((log_mor_sistema[i,j]) + (1.96*(sqrt(var_log_mor_sistema[i,j]))))
  }
}

limite_inf_mor_sistema = round(limite_inf_mor_sistema,digits = 3)
limite_sup_mor_sistema = round(limite_sup_mor_sistema,digits = 3)

for (i in 1:3) {
  for (j in 1:3) {
    limite_inf_mor_sistema[i,j]
    limite_sup_mor_sistema[i,j]
    ic_mor_sistema[i,j] = paste("[",limite_inf_mor_sistema[i,j],";",limite_sup_mor_sistema[i,j],"]")
  }
}

ic_mor_sistema = data.frame(ic_mor_sistema)
rownames(ic_mor_sistema) = c("Filatrópico","Privado","Público")
colnames(ic_mor_sistema) = c("Filatrópico","Privado","Público")

MOR = unlist(Mor_sistema[2,])
IC = paste("[",limite_inf_mor_sistema[2,],";",limite_sup_mor_sistema[2,],"]")
TAB_MOR_sistema = data.frame(MOR, IC)

########################################################################################################

### MOR EXISTENCIA DE COMORBIDADES
absol_mor_existencia = data.frame(table(COVIDBAHIA1$COMORBIDADE)[-3])
outras_mor_existencia = NULL
chance_mor_existencia = NULL
for (i in 1:nrow(absol_mor_existencia)) {
  outras_mor_existencia[i] = sum(absol_mor_existencia$Freq) - absol_mor_existencia$Freq[i]
  chance_mor_existencia[i] = absol_mor_existencia$Freq[i]/outras_mor_existencia[i]
}
absol_mor_existencia$outras_mor_existencia = outras_mor_existencia
absol_mor_existencia$chance_mor_existencia = chance_mor_existencia

Mor_existencia = matrix(nrow = 2, ncol = 2)

for (i in 1:nrow(absol_mor_existencia)) {
  for (j in 1:nrow(absol_mor_existencia)) {
    Mor_existencia[i,j] = absol_mor_existencia$chance_mor_existencia[j]/absol_mor_existencia$chance_mor_existencia[i]
  }
}
Mor_existencia = data.frame(Mor_existencia)
rownames(Mor_existencia) = c("Não","Sim")
colnames(Mor_existencia) = c("Não","Sim")
Mor_existencia = round(Mor_existencia, digits = 3)

### IC MOR EXISTENCIA
ic_mor_existencia = matrix(nrow = 2, ncol = 2)
log_mor_existencia = matrix(nrow = 2, ncol = 2)
var_log_mor_existencia = matrix(nrow = 2, ncol = 2)
limite_inf_mor_existencia = matrix(nrow = 2, ncol = 2)
limite_sup_mor_existencia = matrix(nrow = 2, ncol = 2)

for (i in 1:nrow(absol_mor_existencia)) {
  for (j in 1:nrow(absol_mor_existencia)) {
    log_mor_existencia[i,j] = log(Mor_existencia[i,j])
    var_log_mor_existencia[i,j] = ((1/absol_mor_existencia$Freq[i])+(1/absol_mor_existencia$outras_mor_existencia[i])+(1/absol_mor_existencia$Freq[j])+(1/absol_mor_existencia$outras_mor_existencia[j]))    
    limite_inf_mor_existencia[i,j] = exp((log_mor_existencia[i,j]) - (1.96*(sqrt(var_log_mor_sistema[i,j]))))
    limite_sup_mor_existencia[i,j] = exp((log_mor_existencia[i,j]) + (1.96*(sqrt(var_log_mor_sistema[i,j]))))
  }
}

limite_inf_mor_existencia = round(limite_inf_mor_existencia,digits = 3)
limite_sup_mor_existencia = round(limite_sup_mor_existencia,digits = 3)

for (i in 1:2) {
  for (j in 1:2) {
    limite_inf_mor_existencia[i,j]
    limite_sup_mor_existencia[i,j]
    ic_mor_existencia[i,j] = paste("[",limite_inf_mor_existencia[i,j],";",limite_sup_mor_existencia[i,j],"]")
  }
}

ic_mor_existencia = data.frame(ic_mor_existencia)
rownames(ic_mor_existencia) = c("Não","Sim")
colnames(ic_mor_existencia) = c("Não","Sim")

MOR = unlist(Mor_existencia[1,])
IC = paste("[",limite_inf_mor_existencia[1,],";",limite_sup_mor_existencia[1,],"]")
TAB_MOR_existencia = data.frame(MOR, IC)

##########################################################################################

### MOR MESORREGIAO

absol_mor_meso = data.frame(table(COVIDBAHIA1$MESORREGIAO))
absol_mor_meso = absol_mor_meso[-1,]
outras_mor_meso = NULL
chance_mor_meso = NULL
for (i in 1:nrow(absol_mor_meso)) {
  outras_mor_meso[i] = sum(absol_mor_meso$Freq) - absol_mor_meso$Freq[i]
  chance_mor_meso[i] = absol_mor_meso$Freq[i]/outras_mor_meso[i]
}
absol_mor_meso$outras_mor_meso = outras_mor_meso
absol_mor_meso$chance_mor_meso = chance_mor_meso

Mor_meso = matrix(nrow = 7, ncol = 7)

for (i in 1:nrow(absol_mor_meso)) {
  for (j in 1:nrow(absol_mor_meso)) {
    Mor_meso[i,j] = absol_mor_meso$chance_mor_meso[j]/absol_mor_meso$chance_mor_meso[i]
  }
}
Mor_meso = data.frame(Mor_meso)
rownames(Mor_meso) = MESO_MORT$Var1
colnames(Mor_meso) = MESO_MORT$Var1
Mor_meso = round(Mor_meso, digits = 3)

ic_mor_meso = matrix(nrow = 7, ncol = 7)
log_mor_meso = matrix(nrow = 7, ncol = 7)
var_log_mor_meso = matrix(nrow = 7, ncol = 7)
limite_inf_mor_meso = matrix(nrow = 7, ncol = 7)
limite_sup_mor_meso = matrix(nrow = 7, ncol = 7)

for (i in 1:nrow(absol_mor_meso)) {
  for (j in 1:nrow(absol_mor_meso)) {
    log_mor_meso[i,j] = log(Mor_meso[i,j])
    var_log_mor_meso[i,j] = ((1/absol_mor_meso$Freq[i])+(1/absol_mor_meso$outras_mor_meso[i])+(1/absol_mor_meso$Freq[j])+(1/absol_mor_meso$outras_mor_meso[j]))    
    limite_inf_mor_meso[i,j] = exp((log_mor_meso[i,j]) - (1.96*(sqrt(var_log_mor_meso[i,j]))))
    limite_sup_mor_meso[i,j] = exp((log_mor_meso[i,j]) + (1.96*(sqrt(var_log_mor_meso[i,j]))))
  }
}

limite_inf_mor_meso = round(limite_inf_mor_meso,digits = 3)
limite_sup_mor_meso = round(limite_sup_mor_meso,digits = 3)

for (i in 1:7) {
  for (j in 1:7) {
    limite_inf_mor_meso[i,j]
    limite_sup_mor_meso[i,j]
    ic_mor_meso[i,j] = paste("[",limite_inf_mor_meso[i,j],";",limite_sup_mor_meso[i,j],"]")
  }
}

ic_mor_meso = data.frame(ic_mor_meso)
rownames(ic_mor_meso) = MESO_MORT$Var1
colnames(ic_mor_meso) = MESO_MORT$Var1

MOR = unlist(Mor_meso[,4])
IC = paste("[",limite_inf_mor_meso[,4],";",limite_sup_mor_meso[,4],"]")

TAB_MOR_meso = data.frame(MOR, IC)

