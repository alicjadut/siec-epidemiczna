source('ewolucja.R')
source('tworz_siec.R')

symulacja<- function(N=100,
                     gamma=0.5,
                     beta=0.5,
                     siec_typ=NA,
                     proc_chorych=0.1,
                     l_krokow=100){
  siec<-tworz_siec(N=N,proc_chorych=proc_chorych,typ=siec_typ)
  #ramka z prawdopodobieństwem zachorowania od czasu
  sym<-data.frame(
    krok= c(0),
    proc_chorych= c(length(V(siec)[czy_chory==1])/vcount(siec)))
  
  #ewolucja sieci
  
  for(krok in 1:l_krokow){
    siec=ewolucja(siec,beta=beta,gamma=gamma)
    wyniki<-c(krok, length(V(siec)[czy_chory==1])/vcount(siec))
    sym<-rbind(sym, wyniki)
    
    
  }
  return(sym)
  
}

grid_wart<-read.csv('sym_wyniki/wartosci.csv')

ix=which(grid_wart$czy_zrobione==0)
for (i in intersect(ix,which(grid_wart$N==1000))){
  grid_wart%>%slice(i)%>%as.vector()->param
  a<-symulacja(N=param$N, gamma=param$gamma, beta=param$beta,
               proc_chorych = param$proc_chorych, 
               l_krokow = param$l_krokow, siec_typ = param$siec_typ)
  write.csv(a, paste0("sym_wyniki/sym", paste0(param[1:6],  collapse = "-" ), ".csv"))
  grid_wart$czy_zrobione[i]=1
  print(i)
}
write.csv(grid_wart, "sym_wyniki/wartosci.csv")
