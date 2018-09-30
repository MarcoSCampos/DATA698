#--------------------------------------------------------
#  SENIOR RESEARCH PROJECT 
#  CUNY MSDS DATA 698.03	
# 
#   Marco Siqueira Campos
#   Sharon Morris
#
#  SOURCE:                                                                                                      
#           SUB_ENEM_2017b
#   VERSION 09/26
#--------------------------------------------------------
#  DESCRIPTION:
#           Sub_set by state Day1
#                      
#--------------------------------------------------------


# read file
sub_enem_2017b<-read.csv(file="sub_enem_2017b.csv", header=TRUE, stringsAsFactors = FALSE)

#Remove asterisk and dot  day1
vec_dot<-grep("[\\*\\.]",sub_enem_2017b$TX_RESPOSTAS_DAY1N)
length(vec_dot)
sub_enem_2017b <- sub_enem_2017b[-c(vec_dot), ]


sample_enem<-sub_enem_2017b


# key LC blue
key_LC=sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_LC==399),'TX_GABARITO_LC']
key_LC=substr(key_LC[1], start=11, stop=50) # remove languages

# key CH blue
key_CH=sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_CH==395),'TX_GABARITO_CH']
key_CH=key_CH[1]

# key CN yellow
key_CN=sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_CN==392),'TX_GABARITO_CN']
key_CN=key_CN[1]

# key MT yellow
key_MT=sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_MT==404),'TX_GABARITO_MT']
key_MT=key_MT[1]

# key day1, day2 and all
key_day1=paste0(key_LC,key_CH)
key_day2=paste0(key_CN,key_MT)
key_all=paste0(key_day1,key_day2)

#Key from character to int number 
key_day1n=chartr(old="ABCDE", new="12345",key_day1)
key_day1n=as.integer(unlist(strsplit(key_day1n,"")))

key_day2n=chartr(old="ABCDE", new="12345",key_day2)
key_day2n=as.integer(unlist(strsplit(key_day2n,"")))

key_alln=chartr(old="ABCDE", new="12345",key_all)
key_alln=as.integer(unlist(strsplit(key_alln,"")))


rm(sub_enem_2017b)

sample_enem<-sample_enem[complete.cases(sample_enem[,'TX_RESPOSTAS_DAY1N']),]
#sample_enem<-sample_enem[sample_enem$SG_UF_PROVA=='PI',]
#sample_enem<-sample_enem[sample_enem$SG_UF_PROVA=='PB',]
#sample_enem<-sample_enem[sample_enem$SG_UF_PROVA=='PE',]
#sample_enem<-sample_enem[sample_enem$SG_UF_PROVA=='CE',]
sample_enem<-sample_enem[sample_enem$SG_UF_PROVA=='RN',]


# converting string in columns in new df
# New def day1

n_rows<-nrow(sample_enem)
#n_rows<-10000

mylist<-list()
col<-numeric(85)
vec<-numeric(85)
for(i in 1:n_rows){
  x<-sample_enem[i, "TX_RESPOSTAS_DAY1N"]
  col<-as.integer(unlist(strsplit(x,"")))
  for (j in 1:85){
    vec[j]<-col[j]
  }
  mylist[[i]]<-vec
}


# Change matrix to dataframe with column name
features <- c(sprintf("day1_Q%02d", seq(1,85)))
df_day1<-data.frame(do.call('rbind',mylist))
colnames(df_day1) <- features


# Remove 
rm(mylist)


# Remove NA
to_remove<-df_day1[!complete.cases(df_day1),]
df_day1<-df_day1[complete.cases(df_day1),]
to_remove2<-as.integer(rownames(to_remove))

# Remove index NA from sample enem
if (length(to_remove2)!=0) sample_enem<-sample_enem[-to_remove2,]

#write.csv(sample_enem, file = "sample_enem_day1_PI.csv",row.names=FALSE)
#write.csv(sample_enem, file = "sample_enem_day1_PB.csv",row.names=FALSE)
#write.csv(sample_enem, file = "sample_enem_day1_PE.csv",row.names=FALSE)
#write.csv(sample_enem, file = "sample_enem_day1_CE.csv",row.names=FALSE)
write.csv(sample_enem, file = "sample_enem_day1_RN.csv",row.names=FALSE)

#rm(sample_enem)
rm(to_remove)

new.key<-key_day1n


# Recode the data so that the correct option is allways scored as 5

for(i in 1:ncol(df_day1)) {
  hold1 <- which(df_day1[,i]==new.key[i])
  hold2 <- which(df_day1[,i]==5)
  df_day1[hold1,i]= 5
  df_day1[hold2,i]= new.key[i]
}

rm(hold1)
rm(hold2)

head(df_day1)
#write.csv(df_day1, file = "df_day1_PI.csv",row.names=FALSE)
#write.csv(df_day1, file = "df_day1_PB.csv",row.names=FALSE)
#write.csv(df_day1, file = "df_day1_PE.csv",row.names=FALSE)
#write.csv(df_day1, file = "df_day1_CE.csv",row.names=FALSE)
write.csv(df_day1, file = "df_day1_RN.csv",row.names=FALSE)
#
#
#
#


