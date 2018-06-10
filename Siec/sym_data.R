grid_wart<-read.csv('sym_wyniki/wartosci.csv')

diag_faz=data.frame()
ix=which(grid_wart$czy_zrobione==1)
for (row_index in ix){
  grid_wart%>%slice(row_index)%>%as.vector()->param
  param=param[1:6]
  data=read.csv(paste0("sym_wyniki/sym", paste0(param,  collapse = "-" ), ".csv"))
  data=tail(data,10)$proc_chorych
  diag_faz=rbind(diag_faz,
                 data.frame(
                   procent_chorych=mean(data),
                   sd=sd(data),
                   N=param$N,
                   beta=param$beta,
                   gamma=param$gamma,
                   siec_typ=param$siec_typ,
                   l_krokow=param$l_krokow,
                   proc_chorych0=param$proc_chorych,
                   k_sr=0,
                   lambda_kr=0,
                   lambda=0,
                   wsp=0
                 ))
}

#uzupełnienie brakujacych danych
for(typ in levels(diag_faz$siec_typ)){
  ix_typ=which(diag_faz$siec_typ==typ)
  for(N in unique(filter(diag_faz,siec_typ==typ)$N)){
    ix_n=which(diag_faz$N==N)
    siec=tworz_siec(N,0.1,typ)
    deg=degree(siec)
    diag_faz$k_sr[intersect(ix_typ,ix_n)]=mean(deg)
    diag_faz$lambda_kr[intersect(ix_typ,ix_n)]=mean(deg^2)/mean(deg)
    
  }
}


diag_faz=mutate(diag_faz,lambda=beta/gamma,wsp=lambda/lambda_kr)

write.csv(diag_faz,'diag_faz.csv')
