library(caret)
library(readr)
library(shiny)
library(ggplot2)
library(igraph)
source('ewolucja.R')
source('tworz_siec.R')
N=100
gamma=0.5
beta=0.5
siec_typ="ba"
proc_chorych=0.1
l_krokow<-100

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
a<-symulacja(N=100, gamma=0.1, beta=0.1, proc_chorych = 0.1, l_krokow = 100, siec_typ = "ba")

grid_wart<-expand.grid(N=1000, gamma=seq(0, 1, 0.1), 
                       beta= seq(0, 1, 0.1), 
                       proc_chorych=c(0.05, 0.1, 0.2, 0.5), 
                       l_krokow=100,
                       siec_typ= "ba")

write_csv(grid_wart, "wartosci.csv")

grid_wart%>%slice(1)%>%as.vector()->c
setwd("sym_wyniki")
for (i in 80:nrow(grid_wart)){
  grid_wart%>%slice(i)%>%as.vector()->param
  a<-symulacja(N=param$N, gamma=param$gamma, beta=param$beta,
               proc_chorych = param$proc_chorych, 
               l_krokow = param$l_krokow, siec_typ = param$siec_typ)
  write_csv(a, paste0("sym", paste0(param,  collapse = "-" ), ".csv"))
  print(i)
}

###na 80
library(stringr)
list.files()%>%
  str_match("sym([0-9]{1,})-([0-9 \\.]{1,})-([0-9 \\.]{1,})-([0-9 \\.]{1,})-([0-9 \\.]{1,})-[0-9]{1}\\.csv")
  

lista_csv<-vector("list", length= length(list.files()))

for ( i in 1:length(list.files())){
  lista_csv[[i]]<-read_csv(list.files()[i])
}
names(lista_csv)<-list.files()




lista_csv%>%lapply(FUN=function(x) tail(x, 5)%>%summarise(a=mean(proc_chorych)) )








