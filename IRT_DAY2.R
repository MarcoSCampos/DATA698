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
#           IRT DAY 2
#           R program IRT and M4 function
#           Detecting similarities at MICRODADOS_ENEM_2017
#           
#--------------------------------------------------------

# load package
library(mirt)


# read file
sub_enem_2017b<-read.csv(file="sub_enem_2017b.csv", header=TRUE, stringsAsFactors = FALSE)


#  Remove asterisk and dot  day2
vec_dot<-grep("[\\*\\.]",sub_enem_2017b$TX_RESPOSTAS_DAY2N)
length(vec_dot)
sub_enem_2017b <- sub_enem_2017b[-c(vec_dot), ]


##### M4 function

gtd <- function(P,Q,m,n) {
  R <- 1-(P+Q)
  I=length(P)
  rec <- vector("list",I+1)
  rec[[1]]=matrix(0,nrow=I+1,ncol=I+1)
  rec[[1]][1,1] <- 1
  for(k in 2:(I+1)){
    rec[[k]] = R[k-1]*rec[[k-1]]+rbind(0,P[k-1]*rec[[k-1]])[-(I+2),]+cbind(0,Q[k-1]*rec[[k-1]])[,-(I+2)]
  }
  for(k in 1:(I+1)){ rec[[k]]=t(rec[[k]])}
  upper <- matrix(nrow=I+1,ncol=I+1)
  for(x in 1:(I+1)){
    for(y in 1:(I+1)) {
      upper[x,y] = sum(rec[[I+1]][x:(I+1),y:(I+1)])
    }
  }
  prob.table <- expand.grid(0:I,0:I)
  colnames(prob.table) <- c("IncorrectMatch","CorrectMatch")
  prob.table <- prob.table[which(rowSums(prob.table)<=I),]
  prob.table <- prob.table[order(prob.table[,1]),]
  prob.table <- cbind(prob.table,0,0,0,0)
  prob.table[,3] <- I-(rowSums(prob.table[,1:2]))
  for(i in 1:(nrow(prob.table))){
    x=prob.table[i,1]
    y=prob.table[i,2]
    prob.table[i,4] <- upper[x+1,y+1]
    prob.table[i,5] <- rec[[I+1]][x+1,y+1]
  }
  for(i in 1:(nrow(prob.table))){
    r = prob.table[i,4]
    marked = which(prob.table[,4] <= r)
    prob.table[i,6] <- sum(prob.table[marked,5])
  }
  colnames(prob.table)[3:6] <- c("NonMatch","Upper",
                                 "Probability","TailProbability")
  p = prob.table[which(prob.table[,1]==n & prob.table[,2]==m),6]
  list(prob.table[,-4],p)
}

#### IRT Pro function
irtprob <- function(th, item.param) {
  # item.param - n x t item parameter matrix, where t is twice the
  # number of categories. This matrix is provided by the mirt package
  # First four columns are slopes, and the last four columns are
  # intercepts
  # th - a numeric value for person parameter estimate
  n.opt = ncol(item.param)/2
  prob <- matrix(nrow = nrow(item.param), ncol = ncol(item.param)/2)
  for (j in 1:ncol(prob)) {
    prob[,j] = exp((item.param[, j] * th) + item.param[,j + n.opt])
  }
  prob <- prob/rowSums(prob)
  prob
}



# key LC blue
key_LC<-sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_LC==399),'TX_GABARITO_LC']
key_LC<-substr(key_LC[1], start=11, stop=50) # remove languages

# key CH blue
key_CH<-sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_CH==395),'TX_GABARITO_CH']
key_CH<-key_CH[1]

# key CN yellow
key_CN<-sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_CN==392),'TX_GABARITO_CN']
key_CN<-key_CN[1]

# key MT yellow
key_MT<-sub_enem_2017b[which(sub_enem_2017b$CO_PROVA_MT==404),'TX_GABARITO_MT']
key_MT<-key_MT[1]

# key day1, day2 and all
key_day1<-paste0(key_LC,key_CH)
key_day2<-paste0(key_CN,key_MT)
key_all<-paste0(key_day1,key_day2)

#Key from character to int number 
key_day1n<-chartr(old="ABCDE", new="12345",key_day1)
key_day1n<-as.integer(unlist(strsplit(key_day1n,"")))

key_day2n<-chartr(old="ABCDE", new="12345",key_day2)
key_day2n<-as.integer(unlist(strsplit(key_day2n,"")))

key_alln<-chartr(old="ABCDE", new="12345",key_all)
key_alln<-as.integer(unlist(strsplit(key_alln,"")))

# random sample to compute IRT 1000 samples
#set.seed(123)
#sample_enem<-sub_enem_2017b[sample(nrow(sub_enem_2017b), 1000), ]

sample_enem<-sub_enem_2017b
rm(sub_enem_2017b)

sample_enem<-sample_enem[complete.cases(sample_enem[,'TX_RESPOSTAS_DAY2N']),]

###################
# Generate IRT for DAY 2


# converting string in columns in new df
# New def day2

n_rows<-nrow(sample_enem)

mylist<-list()
col<-numeric(90)
vec<-numeric(90)
for(i in 1:n_rows){
  x<-sample_enem[i, "TX_RESPOSTAS_DAY2N"]
  col<-as.integer(unlist(strsplit(x,"")))
  for (j in 1:90){
    vec[j]<-col[j]
  }
  mylist[[i]]<-vec
  }

# Change matrix to dataframe with column name
features <- c(sprintf("day2_Q%02d", seq(1,90)))
df_day2<-data.frame(do.call('rbind',mylist))
colnames(df_day2) <- features


# Remove 
rm(mylist)


# Remove NA
to_remove<-df_day2[!complete.cases(df_day2),]
df_day2<-df_day2[complete.cases(df_day2),]
to_remove2<-as.integer(rownames(to_remove))

# Remove index NA from sample enem
if (length(to_remove2)!=0) sample_enem<-sample_enem[-to_remove2,]

# Save file for cross reference
write.csv(sample_enem, file = "sample_enem_day2.csv",row.names=TRUE)
rm(sample_enem)
rm(to_remove)

new.key<-key_day2n


# Recode the data so that the correct option is allways scored as 5

for(i in 1:ncol(df_day2)) {
  hold1 <- which(df_day2[,i]==new.key[i])
  hold2 <- which(df_day2[,i]==5)
  df_day2[hold1,i]= 5
  df_day2[hold2,i]= new.key[i]
}

rm(hold1)
rm(hold2)


head(df_day2)

# Run IRT function Day 2
nrm_day2<-mirt(df_day2,1,'nominal')

# See IRT parameters Day 2
ipar.nrm_day2<-coef(nrm_day2,simplify=T,IRTpars=TRUE)$item
head(ipar.nrm_day2) # IRT parameters day2

# Theta parameter Day 2
theta_Day2.ML<-fscores(nrm_day2,method='ML')
head(theta_Day2.ML)

########################################################
# Function to Compute de probability with M4 for a pair
# based on NRM


m4<-function(std1,std2){
  th1_Day2<-theta_Day2.ML[std1,1]
  th2_Day2<-theta_Day2.ML[std2,1]
  P1_Day2<-irtprob(item.param=ipar.nrm_day2,th=th1_Day2)
  P2_Day2<-irtprob(item.param=ipar.nrm_day2,th=th2_Day2)
  P_Day2<-P1_Day2[,5]*P2_Day2[,5]
  Q_Day2<-rowSums(P1_Day2[,1:4]*P2_Day2[,1:4])
  m_Day2 <- sum((df_day2[std1,]==df_day2[std2,] & df_day2[std1,]==5)*1)
  n_Day2 <- sum((df_day2[std1,]==df_day2[std2,] & df_day2[std1,]!=5)*1)
  M4_Day2 <- gtd(P=P_Day2,Q=Q_Day2,m=m_Day2,n=n_Day2)
  return (M4_Day2[[2]])
}

# Compute de probability with M4 index for a pair 1000 and 1200
m4(1000,1200)

# Function for countour plot with number of correct matches and incorrect matches

m4plot<-function(std1,std2){
  th1_Day2<-theta_Day2.ML[std1,1]
  th2_Day2<-theta_Day2.ML[std2,1]
  P1_Day2<-irtprob(item.param=ipar.nrm_day2,th=th1_Day2)
  P2_Day2<-irtprob(item.param=ipar.nrm_day2,th=th2_Day2)
  P_Day2<-P1_Day2[,5]*P2_Day2[,5]
  Q_Day2<-rowSums(P1_Day2[,1:4]*P2_Day2[,1:4])
  m_Day2 <- sum((df_day2[std1,]==df_day2[std2,] & df_day2[std1,]==5)*1)
  n_Day2 <- sum((df_day2[std1,]==df_day2[std2,] & df_day2[std1,]!=5)*1)
  M4_Day2 <- gtd(P=P_Day2,Q=Q_Day2,m=m_Day2,n=n_Day2)
  obs<-c(m_Day2, n_Day2)
  plot_obj<-contourplot(TailProbability ~ CorrectMatch + IncorrectMatch,
                        data=M4_Day2[[1]],
                        labels=FALSE,
                        xlab="Number of Correct Matches",
                        ylab="Number of Incorrect Matches",
                        main=paste("Contour Plot for",std1,std2,"Day 2", sep=" "),
                        panel=function(at,lty,...){
                          panel.contourplot(at = .00001, lty = 1,...)
                          panel.contourplot(at = .0001, lty = 2,...)
                          panel.contourplot(at = .001, lty = 3,...)
                          panel.contourplot(at = .01, lty = 4,...)
                          panel.points(x=obs[1],y=obs[2], pch=15, cex=1)
                        },
                        key=list(corner=c(1,.9),
                                 lines=list(lty=c(1,2,3,4)),
                                 text=list(c("p=0.00001","p=0.0001","p=0.001","p=0.01"))),
                        scales=list(
                          y=list(at=seq(0,90,4)),
                          x=list(at=seq(0,90,4)))
  )
  print(plot_obj)
  }



# Plot for a pair 1000 and 1200
m4plot(100,120)



# Function to recovery new student ID and new rows to apply M4

recover<-function(df_t,state,day){
  # state, two capital letters, state acronym
  # day, 1 for day1 and 2 for day2
  # df with high score match with two pairs
  # read the mirror file to recovery the student ID  
  
  file_n<-paste("sample_enem_day",day,"_",state,".csv", sep="")
  df<-read.csv(file=file_n, header=TRUE, stringsAsFactors = FALSE) 
  a1<-df[df_t[,"row1"],"NU_INSCRICAO"]
  a2<-df[df_t[,"row2"],"NU_INSCRICAO"] 
  
  file_m<-paste("sample_enem_day",day,".csv", sep="")
  df1<-read.csv(file=file_m, header=TRUE, stringsAsFactors = FALSE)
  
  b1<-which(df1$NU_INSCRICAO %in% a1)
  b2<-which(df1$NU_INSCRICAO %in% a2)
  
  r1<-df_t[,"row1"]
  r2<-df_t[,"row2"]
  
  i<-df_t[,"incorrect"]
  c<-df_t[,"correct"]
  
  df_rec<<-data.frame("std1"=a1,"std2"=a2, "nrow1"=b1, "nrow2"=b2,
                      "correct"=c, "incorrect"=i, "row1"=r1,"row2"=r2)
  return(df_rec)
}


# Example for test the function

# row1<-c(1000, 1010, 1020, 1030, 1040)
# row2<-c(1001, 1002, 1003, 1004, 1005)
# correct<-c(20,21,23,18,27)
# incorrect<-c(10,12,15,18,12)

#df_test<-data.frame(row1, row2, correct, incorrect)

recover(df_test, "PI", 2)


