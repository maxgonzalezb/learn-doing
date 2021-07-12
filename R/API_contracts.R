source('C:\\repos\\learn-doing\\R\\API_contracts_functions.R')

#Create preliminary datasets
listaUrlsActas=createDefinitiveDatasetURLS()
codes.find=df%>%dplyr::group_by(CodigoExterno)%>%dplyr::summarise(CodigoExterno=CodigoExterno[1])
codes.find.missing=codes.find%>%filter(!(CodigoExterno%in%listaUrlsActas$id))
listaids=(codes.find.missing$CodigoExterno)
print(length(listaids))
1-print(nrow(codes.find.missing)/nrow(codes.find))

listaCriteria=createDefinitiveDatasetCriteria()

listaUrlsActas.faltantes=listaUrlsActas%>%filter(!(id%in%listaCriteria$id))
print(nrow(listaUrlsActas.faltantes))
1-print(nrow(listaUrlsActas.faltantes)+nrow(codes.find.missing))/nrow(codes.find)

#.1 Get Missing URLS
contracts=data.frame()
bidders=data.frame()
fails=data.frame()

##Get the URLS
table.urls=data.frame()
fails=data.frame()
delay=1.5
for (h in seq(2744,1)) {
  if(h%%500==0){
    save(table.urls,file=paste0('C:\\repos\\learn-doing\\data\\table_urls_incomplete_',h,gsub(x=as.character(Sys.time()),pattern = ':',''),'.Rdata'))
    table.urls=data.frame()
    }
  possibleError <- tryCatch(
    update_api(listaids[h]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){
    fails=rbind(data.frame(h=h),fails)
    print(paste0(h,': error'))
    Sys.sleep(delay)
    next
  }
  if(!inherits(possibleError, "error")){
    table.urls=rbind(possibleError,table.urls)
    print(paste0(h,': good'))
    Sys.sleep(delay)
  }
}

##2.1 Individual completing of misssing
table.criteria=data.frame()
fails=data.frame()
delay=1.1
for (h in seq(2311,3266)) {
  print(h)
  if(h%%500==0){
    #print(table(table.criteria$Ítem))
    save(table.criteria,file=paste0('C:\\repos\\learn-doing\\data\\table_criteria_incomplete_',h,gsub(x=as.character(Sys.time()),pattern = ':',''),'.Rdata'))
    table.criteria=data.frame()
  }
  possibleError <- tryCatch(
    updatelist(urlActa = listaUrlsActas.faltantes$urlActa[h],idcheck = listaUrlsActas.faltantes$id[h],timeoutt = 60),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){
    fails=rbind(data.frame(h=h),fails)
    print(paste0(h,': error'," ",possibleError))
    next
  }
  if(!inherits(possibleError, "error")){
    table.criteria=rbind(possibleError,table.criteria)}
  print(paste0(h,': good'))
  Sys.sleep(delay)
  closeAllConnections()
}

##2.2Parallerl completing
table.criteria=getCriteriaFromURLS_parallel(listaUrlsActas.faltantes,start=220,end=300,pass=50,saveresults=T,innerdelay = 1,externaldelay = 5)

#3. Clean Get awarding criteria
listaUrlsActas=createDefinitiveDatasetURLS()
listaCriteria=createDefinitiveDatasetCriteria()
listaCriteria.clean=listaCriteria%>%mutate(item_clean=tolower(Ítem))%>%
                                                      mutate(item_clean=stri_trans_general(str = item_clean, 
                                                                                id = "Latin-ASCII"))%>%
                                                    mutate(item_clean=trimws(item_clean))%>%
                                                    mutate(item_clean=gsub("[[:punct:]]", "", item_clean))%>%
                                                    mutate(item_clean=gsub("\\t", "", item_clean))%>%
                                                    mutate(hasexp=grepl( 'exp', item_clean, fixed = TRUE))%>%
                                                    mutate(weight=gsub(x=Ponderación,replacement = "",pattern = "%",fixed = T)%>%as.numeric())%>%
                                                    mutate(hasPrecio=(grepl( 'precio', item_clean, fixed = TRUE)|grepl('oferta economica', item_clean, fixed = TRUE)|grepl('valor de la oferta', item_clean, fixed = TRUE)))%>%
                                                    mutate(hasQuality=(grepl( 'calidad', item_clean, fixed = TRUE)|grepl('tecnica', item_clean, fixed = TRUE)))


listaContractExp=listaCriteria.clean%>%group_by(id)%>%dplyr::summarise(hasExp=(sum(hasexp)>0),percExp=sum(weight[hasexp==1]),percPrice=sum(weight[hasPrecio==1]),percQuality=sum(weight[hasQuality==1]))
ggplot(listaContractExp,aes(x=as.numeric(percPrice)))+geom_histogram(binwidth = 10)+xlim(0,100)
ggplot(listaContractExp,aes(x=as.numeric(percQuality)))+geom_histogram(binwidth = 10)+xlim(0,100)

types.factors=listaCriteria.clean%>%group_by(item_clean)%>%count()%>%arrange(-n)

# 4. Write all final datasets in experience folder
write.csv2(listaContractExp,file='C:\\repos\\learn-doing\\data\\experience\\ExperienceFactorContract.csv')
save(listaUrlsActas,file = 'C:\\repos\\learn-doing\\data\\experience\\RawURLS.Rdata')
save(listaCriteria,file = 'C:\\repos\\learn-doing\\data\\experience\\RawCriteria.Rdata')
save(listaCriteria.clean,file = 'C:\\repos\\learn-doing\\data\\experience\\CleanCriteria.Rdata')




#2. Get full contract information
for (h in seq(10088,length(listaids))) {
print(h)
 
path=paste0('http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=',listaids[h],'&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82')
r <- GET(url = path)
c <- content(r)  
if(length(c$Cantidad)==0){
  fails=rbind(data.frame(h=h),fails)
  #Sys.sleep(5)
  next()
}
if((c$Cantidad)==0){
  fails=rbind(data.frame(id=h),fails)
  #Sys.sleep(5)
  next()
}

#Get bidders data
if(length(c$Listado[[1]]$Adjudicacion)==0){
  fails=rbind(data.frame(id=h),fails)
  #Sys.sleep(5)
  next()
}

urlActa=c$Listado[[1]]$Adjudicacion$UrlActa

#Intentar encontrar los criterios
identificador.licitacion=word(urlActa, 2, sep="qs=")
dirtabla=paste0('https://www.mercadopublico.cl/Procurement/Modules/RFB/DetailsAcquisition.aspx?qs=',identificador.licitacion)
pagina.base <- read_html(dirtabla,n=128*1024,encoding = 'cp-1252')
table=pagina.base%>%html_nodes("#grvCriterios")%>%html_table()
if(length(table)>=1){
table.criteria.iter=table[[1]]%>%as.data.frame()%>%mutate(Ítem=gsub(x=Ítem,pattern = '\n',replacement=''))%>%
  mutate(Ítem=gsub(x=Ítem,pattern = '\r',replacement=''))%>%mutate(Ítem=gsub('[[:digit:]]+', '', Ítem))%>%mutate(Ítem=trimws(Ítem))%>%mutate(id=listaids[h])
table.criteria=rbind(table.criteria.iter,table.criteria)
}
#Antiguo
if(FALSE){
simple <- read_html(urlActa)
table=simple%>%html_nodes(xpath='//*[@id="grdItemOC_ctl02_ucAward_gvLines"]')%>%html_table()
if(length(table)==0){
  fails=rbind(data.frame(id=h),fails)
  Sys.sleep(5)
  next()
}
table.format=table[[1]]%>%as.data.frame()%>%dplyr::select(-2)%>%mutate(id=listaids[h])  
bidders=rbind(table.format,bidders)
print('biddders stage completed')
#Get contract info
l=lengths(c$Listado[[1]])==1
infonounitaria=c$Listado[[1]][!l]
info1=c$Listado[[1]][l]%>%as.data.frame()%>%select(-Tipo)
info2=c$Listado[[1]]$Comprador%>%as.data.frame()
info3=t(infonounitaria$Fechas)%>%as.data.frame()
info4=t(infonounitaria$Adjudicacion)%>%as.data.frame()
info5=infonounitaria$Items$Listado%>%as.data.frame()%>%select(-Descripcion)
#infocontrato=as.data.frame(cbind(info1,info2,info3,info4,info5))%>%mutate(MontoEstimado=ifelse(!is.na(MontoEstimado),yes=MontoEstimado,no=NA))
cols=c(MontoEstimado=NA)
infocontrato=as.data.frame(cbind(info1,info2,info3,info4,info5))%>%add_column(!!!cols[!names(cols) %in% names(.)])
#browser()

contracts=rbind(infocontrato,contracts)
print('contracts stage completed')
Sys.sleep(5)
}

}


pass=50
start=220
end=300
saveresults=T
innerdelay=1
externaldelay=10
j=220