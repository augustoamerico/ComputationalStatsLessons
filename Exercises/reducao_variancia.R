#Reducao de variancia do estimador do pi
#usando uma variavel de controlo
#OUTPUT: res_final, data frame com crescente
#tamanho de amostragem. verificar a decrescente
#variancia

kk=1
res_final <- data.frame(integer(), double(), double(), double(), double())
n <- 10

while(kk<8) {
  
  if(kk!=1) {
    if(kk%%2!=0)
      n<-n*2
    else
      n<-n*5
  }

  gerador<-c(NA, nrow=n)
  uk<-runif(n) 
  g<-function(x){sqrt(1-x^2)}
  va<-4*g(uk)
  
  pi_estimated<-mean(va)
  pi_var<-(1/(3*n)*(32-(3*pi_estimated^2)))
  
  umed<-1/2
  uvar<-1/(12*n)
  ucov<-1/(6*n)*(8-3*pi_estimated)
  beta<-2*(8-3*pi_estimated)
  
  pi_c_estimated<-(pi_estimated-(ucov/uvar)*(mean(uk)-(1/2)))
  pi_c_var<-(pi_var+(beta^2*uvar) - (2*beta*ucov))
  
  res<-data.frame(n, pi_estimated, pi_var, pi_c_estimated, pi_c_var)
  res_final<-rbind(res_final,res)
  kk<-kk+1
  
}

