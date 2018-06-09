######Tworzenie symulacji sis
library(igraph)
install.packages( "tidyr")
library(dplyr)
library(tidyr)
ba <-  sample_pa(n=100, power=1, m=1,  directed=F)

V(ba)$name<-c(1:100)
# B P zarażenia
#utwórz sieć
#początkowo pzypisz losowym wierzchołkom labele zarażony
#dla każdego kroku
#weź listę zarażonych
#węź ich sąsiadów
#dla każdego zaraź z P beta
#weź znowu listę chorych 
#każdego wyzdrowiej z P gamma
#zapisz  liczbę zarażonych i zdrowych, liczbę zarażonych w tym kroku i wyzdrowiałych w tym kroku

N=100
gamma=0.5
beta=0.5
siec_typ=NA
proc_chorych=0.1
l_krokow<-100

# B P zarażenia
#utwórz sieć
siec<-sample_pa(n=N, power=1, m=1,  directed=F)
siec <- make_full_graph(N)
#początkowo pzypisz losowym wierzchołkom labele zarażony
#1-chory

V(siec)$czy_chory<-sample(c(0,1),size=N, 
                             prob=c(1-proc_chorych, proc_chorych), replace = TRUE)
out_df<-data.frame(krok= c(0), liczba_chorych= c(length(V(siec)[czy_chory==1])))
#dla każdego kroku
for (krok in 1:l_krokow){

#weź listę zarażonych
#węź ich sąsiadów
lista_sas<-neighbors(siec, V(siec)[czy_chory==1])

#dla każdego zaraź z P beta
wek_zarazania<-sample(c(0,1), size=length(lista_sas), prob=c(1-beta, beta), replace=T)
V(siec)[lista_sas]$czy_chory<- pmax(V(siec)[lista_sas]$czy_chory, wek_zarazania)

#weź znowu listę chorych
#każdego wyzdrowiej z P gamma
V(siec)[czy_chory==1]$czy_chory<-sample(c(0,1), size=length(V(siec)[czy_chory==1]), prob=c(gamma, 1-gamma), replace=T)

#zapisz  liczbę zarażonych i zdrowych, liczbę zarażonych w tym kroku i wyzdrowiałych w tym kroku
wyniki<-c(krok, length(V(siec)[czy_chory==1]))
out_df<-rbind(out_df, wyniki)
}


gen_sim<-function(N=100,
                  gamma=0.5,
                  beta=0.5,
                  siec_typ=NA,
                  proc_chorych=0.1,
                  l_krokow=100){
  
  siec <- make_full_graph(N)
  #początkowo pzypisz losowym wierzchołkom labele zarażony
  #1-chory
  
  V(siec)$czy_chory<-sample(c(0,1),size=N, 
                            prob=c(1-proc_chorych, proc_chorych), replace = TRUE)
  out_df<-data.frame(krok= c(0), liczba_chorych= c(length(V(siec)[czy_chory==1])))
  #dla każdego kroku
  for (krok in 1:l_krokow){
    
    #weź listę zarażonych
    #węź ich sąsiadów
    lista_sas<-neighbors(siec, V(siec)[czy_chory==1])
    
    #dla każdego zaraź z P beta
    wek_zarazania<-sample(c(0,1), size=length(lista_sas), prob=c(1-beta, beta), replace=T)
    V(siec)[lista_sas]$czy_chory<- pmax(V(siec)[lista_sas]$czy_chory, wek_zarazania)
    
    #weź znowu listę chorych
    #każdego wyzdrowiej z P gamma
    V(siec)[czy_chory==1]$czy_chory<-sample(c(0,1), size=length(V(siec)[czy_chory==1]), prob=c(gamma, 1-gamma), replace=T)
    
    #zapisz  liczbę zarażonych i zdrowych, liczbę zarażonych w tym kroku i wyzdrowiałych w tym kroku
    wyniki<-c(krok, length(V(siec)[czy_chory==1]))
    out_df<-rbind(out_df, wyniki)
    #print(out_df)
  }
  #TODO:dodać kolumny w df
  out_df%>%mutate(liczba_zdrowych=N-liczba_chorych, proc_chorych=liczba_chorych/N,
                  proc_zdrowych=liczba_zdrowych/N,
                  delta_chorych=liczba_chorych-lag(liczba_chorych)
                  )->out_df
  return(out_df)
  }

a<-gen_sim(l_krokow = 100)


