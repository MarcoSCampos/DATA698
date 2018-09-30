#--------------------------------------------------------
#  SENIOR RESEARCH PROJECT 
#  CUNY MSDS DATA 698.03	
# 
#   Marco Siqueira Campos
#   Sharon Morris
#
#  SOURCE:                                                                                                      
#           MICRODADOS_ENEM_2017
#
#  Version: 09/14
#--------------------------------------------------------
#  DESCRIPTION:
#           R program 
#           Detecting similarities at MICRODADOS_ENEM_2017
#           
#--------------------------------------------------------

#--------------------
#  load and install library
# 
#--------------------

if(!require(data.table)){install.packages('data.table')}
library(dplyr)


#---------------
# memory allocation
#---------------
memory.limit(24576)

#------------------
# Load microdados_enem_2017 file
#------------------

enem_2017 <- data.table::fread(input='microdados_enem_2017.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

#---------------------------
# Load file ITENS_PROVA_2017 key 
#---------------------------

itens_prova_2017<-read.csv(file="ITENS_PROVA_2017.csv", header=TRUE, stringsAsFactors = FALSE, sep=";")

##------------------------------
#  PRE PROCESSING
#  
##-------------------------------
# Remove columns "socioeconomico"

myVar = c("Q001","Q002", "Q003","Q004","Q005","Q006","Q007","Q008","Q009","Q010","Q011","Q012",
          "Q013","Q014", "Q015","Q016","Q017","Q018","Q019","Q020","Q021","Q022","Q023","Q024",
          "Q025","Q026","Q027")
enem_2017[, (myVar):=NULL] 

# Remove columns "redação"
myVar = c("TP_STATUS_REDACAO","NU_NOTA_COMP1","NU_NOTA_COMP2","NU_NOTA_COMP3","NU_NOTA_COMP4","NU_NOTA_COMP5","NU_NOTA_REDACAO")
enem_2017[, (myVar):=NULL] 

# Remove columns "dados do pedido de atendimento especializado"
myVar = c("IN_BAIXA_VISAO","IN_CEGUEIRA","IN_SURDEZ","IN_DEFICIENCIA_AUDITIVA","IN_SURDO_CEGUEIRA","IN_DEFICIENCIA_FISICA",
          "IN_DEFICIENCIA_MENTAL","IN_DEFICIT_ATENCAO","IN_DISLEXIA","IN_DISCALCULIA","IN_AUTISMO",
          "IN_VISAO_MONOCULAR","IN_OUTRA_DEF")
enem_2017[, (myVar):=NULL] 

# Remove columns "dados de atendimento pedidos específico"
myVar = c("IN_GESTANTE","IN_LACTANTE","IN_IDOSO","IN_ESTUDA_CLASSE_HOSPITALAR")
enem_2017[, (myVar):=NULL] 

# Remove columns "dados de pedido especíalizados e específicos"
myVar = c("IN_SEM_RECURSO","IN_BRAILLE","IN_AMPLIADA_24","IN_AMPLIADA_18","IN_LEDOR","IN_ACESSO",
          "IN_TRANSCRICAO","IN_LIBRAS","IN_LEITURA_LABIAL","IN_MESA_CADEIRA_RODAS","IN_MESA_CADEIRA_SEPARADA",
          "IN_APOIO_PERNA","IN_GUIA_INTERPRETE","IN_COMPUTADOR","IN_CADEIRA_ESPECIAL","IN_CADEIRA_CANHOTO",
          "IN_CADEIRA_ACOLCHOADA","IN_PROVA_DEITADO","IN_MOBILIARIO_OBESO","IN_LAMINA_OVERLAY",
          "IN_PROTETOR_AURICULAR","IN_MEDIDOR_GLICOSE","IN_MAQUINA_BRAILE","IN_SOROBAN","IN_MARCA_PASSO",
          "IN_SONDA","IN_MEDICAMENTOS","IN_SALA_INDIVIDUAL","IN_SALA_ESPECIAL","IN_SALA_ACOMPANHANTE",
          "IN_MOBILIARIO_ESPECIFICO","IN_MATERIAL_ESPECIFICO","IN_NOME_SOCIAL")
enem_2017[, (myVar):=NULL] 

# Remove columns "dados do participante"
myVar = c("NU_IDADE","TP_SEXO","TP_ESTADO_CIVIL","TP_COR_RACA","TP_NACIONALIDADE","CO_MUNICIPIO_NASCIMENTO",
          "NO_MUNICIPIO_NASCIMENTO", "CO_UF_NASCIMENTO", "SG_UF_NASCIMENTO","TP_ST_CONCLUSAO",
          "TP_ANO_CONCLUIU","TP_ESCOLA","TP_ENSINO","IN_TREINEIRO")
enem_2017[, (myVar):=NULL] 

# Remove columns "dados da escola"
myVar = c("CO_ESCOLA","CO_MUNICIPIO_ESC","NO_MUNICIPIO_ESC","CO_UF_ESC","SG_UF_ESC","TP_DEPENDENCIA_ADM_ESC",
          "TP_LOCALIZACAO_ESC","TP_SIT_FUNC_ESC")
enem_2017[, (myVar):=NULL] 

# Remove "dados da prova objetiva"
myVar=c("TP_PRESENCA_CN", "TP_PRESENCA_CH","TP_PRESENCA_LC","TP_PRESENCA_MT")
enem_2017[, (myVar):=NULL]   
  
# Remove NA from grade 
enem_2017<-na.omit(enem_2017, cols=c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC","NU_NOTA_MT"))

# Compute mean grade
enem_2017[,NU_NOTA_MEDIA:=rowMeans(.SD),.SDcols=c("NU_NOTA_CN","NU_NOTA_CH","NU_NOTA_LC","NU_NOTA_MT")]

# histogram
hist(enem_2017$NU_NOTA_MEDIA)

# summary stats
summary(enem_2017$NU_NOTA_MEDIA)

# quantile mean rank
quantile(enem_2017$NU_NOTA_MEDIA, prob = seq(0.5, 1, length = 11), type = 5)

# subsetting only mean grades greater than 600
sub_enem_2017 <- subset(enem_2017, NU_NOTA_MEDIA > 600)

# Subsetting only standard color for CN
#sub_enem_2017 <- subset(sub_enem_2017, CO_PROVA_CN < 400)

# Subsetting only standard color for CH
#sub_enem_2017 <- subset(sub_enem_2017, CO_PROVA_CH < 400)

# Subsetting only standard color for LC
#sub_enem_2017 <- subset(sub_enem_2017, CO_PROVA_LC < 403)

# Subsetting only standard color for LC
#sub_enem_2017 <- subset(sub_enem_2017, CO_PROVA_LC < 407)

# save subsetting
#write.csv(sub_enem_2017, file = "sub_enem_2017.csv",row.names=FALSE)

# remove main
rm(enem_2017)

#################################

# remove subsetting
# rm(sub_enem_2017)

# load subsettting
#sub_enem_2017<-read.csv(file="sub_enem_2017.csv", header=TRUE, stringsAsFactors = FALSE)

#------------------
# Pre process itens_prova_2017
# The objective is change the order of responses 
#
#------------------

# Vector of valid exam color
col<-c(391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406)

# Subsetting with only standard color
itens_prova_2017<-itens_prova_2017[itens_prova_2017$CO_PROVA  %in% col,]

# Remove column 
itens_prova_2017$IN_ITEM_ADAPTADO<-NULL

# rename rows names
row.names(itens_prova_2017) <- 1:740

# generate standard blue item position for LC
azul_itens_LC<-itens_prova_2017[1:50,c(1,3,6)]

# change column name               
names(azul_itens_LC)[1]<-"CO_REL_POSICAO"
names(azul_itens_LC)[3]<-"TX_COR_FROM"

# generate standard blue item position for CH
azul_itens_CH<-itens_prova_2017[201:245,c(1,3,6)]

# change column name               
names(azul_itens_CH)[1]<-"CO_REL_POSICAO"
names(azul_itens_CH)[3]<-"TX_COR_FROM"

# generate standard yellow item position for CN
amarelo_itens_CN<-itens_prova_2017[381:425,c(1,3,6)]

# change column name               
names(amarelo_itens_CN)[1]<-"CO_REL_POSICAO"
names(amarelo_itens_CN)[3]<-"TX_COR_FROM"

# generate standard yellow item position for MT
amarelo_itens_MT<-itens_prova_2017[561:605,c(1,3,6)]

# change column name               
names(amarelo_itens_MT)[1]<-"CO_REL_POSICAO"
names(amarelo_itens_MT)[3]<-"TX_COR_FROM"

# Row bind all data frames
itens_sdt<-do.call("rbind", list(azul_itens_LC, azul_itens_CH, amarelo_itens_CN,amarelo_itens_MT))


itens_prova<-left_join(itens_prova_2017, itens_sdt, by = "CO_ITEM")

# save as csv file
#write.table(itens_prova_2017, file = "itens_prova_2017.csv", row.names = FALSE , sep=",")


# Read file with the new position
# itens_prova<-read.csv(file="itens_prova_2017.csv", header=TRUE, stringsAsFactors = FALSE)

rm(itens_prova_2017)

#------------------
# Create cross reference to change all responses
# Responses cross reference for CN
#------------------

itens_391<-itens_prova[itens_prova$CO_PROVA == 391,c(1,9)] 
itens_391<-itens_391[order(itens_391$CO_REL_POSICAO),]
itens_392<-itens_prova[itens_prova$CO_PROVA == 392,c(1,9)]  
itens_392<-itens_392[order(itens_392$CO_REL_POSICAO),]
itens_393<-itens_prova[itens_prova$CO_PROVA == 393,c(1,9)]  
itens_393<-itens_393[order(itens_393$CO_REL_POSICAO),]
itens_394<-itens_prova[itens_prova$CO_PROVA == 394,c(1,9)]  
itens_394<-itens_394[order(itens_394$CO_REL_POSICAO),]

# Responses cross reference for MT
itens_403<-itens_prova[itens_prova$CO_PROVA == 403,c(1,9)] 
itens_403<-itens_403[order(itens_403$CO_REL_POSICAO),]
itens_404<-itens_prova[itens_prova$CO_PROVA == 404,c(1,9)]  
itens_404<-itens_404[order(itens_404$CO_REL_POSICAO),]
itens_405<-itens_prova[itens_prova$CO_PROVA == 405,c(1,9)]  
itens_405<-itens_405[order(itens_405$CO_REL_POSICAO),]
itens_406<-itens_prova[itens_prova$CO_PROVA == 406,c(1,9)]  
itens_406<-itens_406[order(itens_406$CO_REL_POSICAO),]

# Responses cross reference for LC
itens_399<-itens_prova[itens_prova$CO_PROVA == 399,c(1,9)] 
itens_399<-itens_399[order(itens_399$CO_REL_POSICAO),]
itens_400<-itens_prova[itens_prova$CO_PROVA == 400,c(1,9)]  
itens_400<-itens_400[order(itens_400$CO_REL_POSICAO),]
itens_401<-itens_prova[itens_prova$CO_PROVA == 401,c(1,9)]  
itens_401<-itens_401[order(itens_401$CO_REL_POSICAO),]
itens_402<-itens_prova[itens_prova$CO_PROVA == 402,c(1,9)]  
itens_402<-itens_402[order(itens_402$CO_REL_POSICAO),]

# Responses cross reference for CH
itens_395<-itens_prova[itens_prova$CO_PROVA == 395,c(1,9)] 
itens_395<-itens_395[order(itens_395$CO_REL_POSICAO),]
itens_396<-itens_prova[itens_prova$CO_PROVA == 396,c(1,9)]  
itens_396<-itens_396[order(itens_396$CO_REL_POSICAO),]
itens_397<-itens_prova[itens_prova$CO_PROVA == 397,c(1,9)]  
itens_397<-itens_397[order(itens_397$CO_REL_POSICAO),]
itens_398<-itens_prova[itens_prova$CO_PROVA == 398,c(1,9)]  
itens_398<-itens_398[order(itens_398$CO_REL_POSICAO),]

#------------
# Create new column with standard color yellow for CN
#------------

#-----------
# Save and change from datatable to dataframe
#-----------
write.csv(sub_enem_2017, file = "sub_enem_2017.csv",row.names=FALSE)
rm(sub_enem_2017)
sub_enem_2017<-read.csv(file="sub_enem_2017.csv", header=TRUE, stringsAsFactors = FALSE)


sub_enem_2017$TX_RESPOSTAS_CN_YEL <- NA

# Convert responses to CN to yellow 
for (i in 1:nrow(sub_enem_2017)){
  if (sub_enem_2017$CO_PROVA_CN[i] == 391){
    x<-sub_enem_2017[i,"TX_RESPOSTAS_CN"]
    a <- unlist(strsplit(x,""))
    a<-a[itens_391$CO_POSICAO]
    sub_enem_2017[i,"TX_RESPOSTAS_CN_YEL"]<-paste(a, collapse="")
  } else
    if (sub_enem_2017$CO_PROVA_CN[i] == 392){
      x<-sub_enem_2017[i,"TX_RESPOSTAS_CN"]
      a <- unlist(strsplit(x,""))
      a<-a[itens_392$CO_POSICAO]
      sub_enem_2017[i,"TX_RESPOSTAS_CN_YEL"]<-paste(a, collapse="")
    } else
      if (sub_enem_2017$CO_PROVA_CN[i] == 393){
        x<-sub_enem_2017[i,"TX_RESPOSTAS_CN"]
        a <- unlist(strsplit(x,""))
        a<-a[itens_393$CO_POSICAO]
        sub_enem_2017[i,"TX_RESPOSTAS_CN_YEL"]<-paste(a, collapse="")
      } else
        if (sub_enem_2017$CO_PROVA_CN[i] == 394)
      {
        x<-sub_enem_2017[i,"TX_RESPOSTAS_CN"]
        a <- unlist(strsplit(x,""))
        a<-a[itens_394$CO_POSICAO]
        sub_enem_2017[i,"TX_RESPOSTAS_CN_YEL"]<-paste(a, collapse="")}
}

#---------------
# Create new column with standard color yellow for MT
#---------------

sub_enem_2017$TX_RESPOSTAS_MT_YEL <- NA

for (i in 1:nrow(sub_enem_2017)){
  if (sub_enem_2017$CO_PROVA_MT[i] == 403){
    x<-sub_enem_2017[i,"TX_RESPOSTAS_MT"]
    a <- unlist(strsplit(x,""))
    a<-a[itens_403$CO_POSICAO]
    sub_enem_2017[i,"TX_RESPOSTAS_MT_YEL"]<-paste(a, collapse="")
  } else
    if (sub_enem_2017$CO_PROVA_MT[i] == 404){
      x<-sub_enem_2017[i,"TX_RESPOSTAS_MT"]
      a <- unlist(strsplit(x,""))
      a<-a[itens_404$CO_POSICAO]
      sub_enem_2017[i,"TX_RESPOSTAS_MT_YEL"]<-paste(a, collapse="")
    } else
      if (sub_enem_2017$CO_PROVA_MT[i] == 405){
        x<-sub_enem_2017[i,"TX_RESPOSTAS_MT"]
        a <- unlist(strsplit(x,""))
        a<-a[itens_405$CO_POSICAO]
        sub_enem_2017[i,"TX_RESPOSTAS_MT_YEL"]<-paste(a, collapse="")
      } else
        if (sub_enem_2017$CO_PROVA_MT[i] == 406)
        {
          x<-sub_enem_2017[i,"TX_RESPOSTAS_MT"]
          a <- unlist(strsplit(x,""))
          a<-a[itens_406$CO_POSICAO]
          sub_enem_2017[i,"TX_RESPOSTAS_MT_YEL"]<-paste(a, collapse="")}
}

#----------------
# Create new column with standard color blue for CH
#----------------


sub_enem_2017$TX_RESPOSTAS_CH_BLU <- NA

for (i in 1:nrow(sub_enem_2017)){
  if (sub_enem_2017$CO_PROVA_CH[i] == 395){
    x<-sub_enem_2017[i,"TX_RESPOSTAS_CH"]
    a <- unlist(strsplit(x,""))
    a<-a[itens_395$CO_POSICAO]
    sub_enem_2017[i,"TX_RESPOSTAS_CH_BLU"]<-paste(a, collapse="")
  } else
    if (sub_enem_2017$CO_PROVA_CH[i] == 396){
      x<-sub_enem_2017[i,"TX_RESPOSTAS_CH"]
      a <- unlist(strsplit(x,""))
      a<-a[itens_396$CO_POSICAO]
      sub_enem_2017[i,"TX_RESPOSTAS_CH_BLU"]<-paste(a, collapse="")
    } else
      if (sub_enem_2017$CO_PROVA_CH[i] == 397){
        x<-sub_enem_2017[i,"TX_RESPOSTAS_CH"]
        a <- unlist(strsplit(x,""))
        a<-a[itens_397$CO_POSICAO]
        sub_enem_2017[i,"TX_RESPOSTAS_CH_BLU"]<-paste(a, collapse="")
      } else
        if (sub_enem_2017$CO_PROVA_CH[i] == 398)
        {
          x<-sub_enem_2017[i,"TX_RESPOSTAS_CH"]
          a <- unlist(strsplit(x,""))
          a<-a[itens_398$CO_POSICAO]
          sub_enem_2017[i,"TX_RESPOSTAS_CH_BLU"]<-paste(a, collapse="")}
}

#--------------
# Create new column with standard color blue for LC
#--------------

sub_enem_2017$TX_RESPOSTAS_LC_BLU <- NA

for (i in 1:nrow(sub_enem_2017)){
  if (sub_enem_2017$CO_PROVA_LC[i] == 399){
    x<-sub_enem_2017[i,"TX_RESPOSTAS_LC"]
    a <- unlist(strsplit(x,""))
    a<-a[itens_399$CO_POSICAO]
    sub_enem_2017[i,"TX_RESPOSTAS_LC_BLU"]<-paste(a, collapse="")
  } else
    if (sub_enem_2017$CO_PROVA_LC[i] == 400){
      x<-sub_enem_2017[i,"TX_RESPOSTAS_LC"]
      a <- unlist(strsplit(x,""))
      a<-a[itens_400$CO_POSICAO]
      sub_enem_2017[i,"TX_RESPOSTAS_LC_BLU"]<-paste(a, collapse="")
    } else
      if (sub_enem_2017$CO_PROVA_LC[i] == 401){
        x<-sub_enem_2017[i,"TX_RESPOSTAS_LC"]
        a <- unlist(strsplit(x,""))
        a<-a[itens_401$CO_POSICAO]
        sub_enem_2017[i,"TX_RESPOSTAS_LC_BLU"]<-paste(a, collapse="")
      } else
        if (sub_enem_2017$CO_PROVA_LC[i] == 402)
        {
          x<-sub_enem_2017[i,"TX_RESPOSTAS_LC"]
          a <- unlist(strsplit(x,""))
          a<-a[itens_402$CO_POSICAO]
          sub_enem_2017[i,"TX_RESPOSTAS_LC_BLU"]<-paste(a, collapse="")}
}


# New column with right student response fitting the correct language, removing 99999
sub_enem_2017$TX_RESPOSTAS_LC2_BLU<-ifelse(sub_enem_2017$TP_LINGUA==0, paste(substr(sub_enem_2017$TX_RESPOSTAS_LC_BLU, start=1, stop=5), substr(sub_enem_2017$TX_RESPOSTAS_LC_BLU, start=11, stop=50),sep=""),substr(sub_enem_2017$TX_RESPOSTAS_LC_BLU, start=6, stop=50))

# New column without language response from student
sub_enem_2017$TX_RESPOSTAS_LC3_BLU<-substr(sub_enem_2017$TX_RESPOSTAS_LC2_BLU, start=6, stop=50)

# stack all student response in one column, 1st day, LC and CH   
sub_enem_2017$TX_RESPOSTAS_DAY1<- with(sub_enem_2017, paste0(TX_RESPOSTAS_LC3_BLU, TX_RESPOSTAS_CH_BLU))

# stack all student response in one column, 2nd day, CN and MT
sub_enem_2017$TX_RESPOSTAS_DAY2<- with(sub_enem_2017, paste0(TX_RESPOSTAS_CN_YEL, TX_RESPOSTAS_MT_YEL))

# stack all student response in one column, LC, CH, CN and MT
sub_enem_2017$TX_RESPOSTAS_ALL<-with(sub_enem_2017, paste0(TX_RESPOSTAS_LC3_BLU, TX_RESPOSTAS_CH_BLU, TX_RESPOSTAS_CN_YEL, TX_RESPOSTAS_MT_YEL))

######## key

# key LC blue
key_LC=sub_enem_2017[which(sub_enem_2017$CO_PROVA_LC==399),'TX_GABARITO_LC']
key_LC=substr(key_LC[1], start=11, stop=50) # remove languages

# key CH blue
key_CH=sub_enem_2017[which(sub_enem_2017$CO_PROVA_CH==395),'TX_GABARITO_CH']
key_CH=key_CH[1]

# key CN yellow
key_CN=sub_enem_2017[which(sub_enem_2017$CO_PROVA_CN==392),'TX_GABARITO_CN']
key_CN=key_CN[1]

# key MT yellow
key_MT=sub_enem_2017[which(sub_enem_2017$CO_PROVA_MT==404),'TX_GABARITO_MT']
key_MT=key_MT[1]

# key day1, day2 and all
key_day1=paste0(key_LC,key_CH)
key_day2=paste0(key_CN,key_MT)
key_all=paste0(key_day1,key_day2)



##################

# Change character to number from to TX_RESPOSTAS_DAY1N
sub_enem_2017$TX_RESPOSTAS_DAY1N<-unlist(lapply(sub_enem_2017[,"TX_RESPOSTAS_DAY1"], chartr, old="ABCDE*.", new="1234599"))

# Change character to number from to TX_RESPOSTAS_DAY2N
sub_enem_2017$TX_RESPOSTAS_DAY2N<-unlist(lapply(sub_enem_2017[,"TX_RESPOSTAS_DAY2"], chartr, old="ABCDE*.", new="1234599"))

# Change character to number from to TX_RESPOSTAS_DAY2N
sub_enem_2017$TX_RESPOSTAS_ALLN<-unlist(lapply(sub_enem_2017[,"TX_RESPOSTAS_ALL"], chartr, old="ABCDE*.", new="1234599"))


#Remove rows with '.' and '*' and save new file sub_enem_b
vec_dot<-grep("[\\*\\.]",sub_enem_2017$TX_RESPOSTAS_ALL)
length(vec_dot)
sub_enem_2017b <- sub_enem_2017[-c(vec_dot), ]


# Change character to number from to TX_RESPOSTAS_DAY1N
sub_enem_2017b$TX_RESPOSTAS_DAY1N<-unlist(lapply(sub_enem_2017b[,"TX_RESPOSTAS_DAY1"], chartr, old="ABCDE", new="12345"))

# Change character to number from to TX_RESPOSTAS_DAY2N
sub_enem_2017b$TX_RESPOSTAS_DAY2N<-unlist(lapply(sub_enem_2017b[,"TX_RESPOSTAS_DAY2"], chartr, old="ABCDE", new="12345"))

# Change character to number from to TX_RESPOSTAS_ALL
sub_enem_2017b$TX_RESPOSTAS_ALLN<-unlist(lapply(sub_enem_2017b[,"TX_RESPOSTAS_ALL"], chartr, old="ABCDE", new="12345"))



#------------
# Save last 
write.csv(sub_enem_2017, file = "sub_enem_2017.csv",row.names=FALSE)
write.csv(sub_enem_2017b, file = "sub_enem_2017b.csv",row.names=FALSE)







