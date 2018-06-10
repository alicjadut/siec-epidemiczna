grid_wart<-read.csv('sym_wyniki/wartosci.csv')
diag_faz=data.frame()
for (row_index in 2:nrow(grid_wart)){
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
                   proc_chorych0=param$proc_chorych
                 ))
}

siec=tworz_siec(1000,0.1,'ba')
deg=degree(siec)
diag_faz$k_sr=mean(deg)
diag_faz$lambda_kr=mean(deg^2)/mean(deg)
diag_faz=mutate(diag_faz,wsp=beta/gamma)
write.csv(diag_faz,'diag_faz.csv')
