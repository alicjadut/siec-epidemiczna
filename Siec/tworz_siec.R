library(igraph)

tworz_siec<- function(N, proc_chorych, typ){
  if (typ=="ba"){
    siec<- sample_pa(n=N, power=1, m=1,  directed=F)
    
  } else if (typ=="ws"){
    siec<- sample_smallworld(dim=2, size=N, nei=1, p=0.1)
  } else if (typ== "fg"){
    siec<-make_full_graph(N)
  } else if (typ=="sq"){
    siec<- make_lattice(dim = 2, length = round(sqrt(N)))
  }
  
  V(siec)$czy_chory<-sample(c(0,1),size=N, 
                            prob=c(1-proc_chorych, proc_chorych), replace = TRUE)
  return(siec)
  
}
