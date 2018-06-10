ewolucja<-function(siec,beta=0.5,gamma=0.5){
  #weź listę zarażonych
  #węź ich sąsiadów
  lista_sas<-neighbors(siec, V(siec)[czy_chory==1])
  
  #dla każdego zaraź z P beta
  wek_zarazania<-sample(c(0,1), size=length(lista_sas), prob=c(1-beta, beta), replace=T)
  V(siec)[lista_sas]$czy_chory<- pmax(V(siec)[lista_sas]$czy_chory, wek_zarazania)
  
  #weź znowu listę chorych
  #każdego wyzdrowiej z P gamma
  V(siec)[czy_chory==1]$czy_chory<-sample(c(0,1), size=length(V(siec)[czy_chory==1]), prob=c(gamma, 1-gamma), replace=T)
  return(siec)
}

