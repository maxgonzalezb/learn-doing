library(readxl)
library(httr) 
library(jsonlite)
library(rvest)
library(tibble())
library(stringr)

#1. Get awarding criteria
contracts=data.frame()
bidders=data.frame()
fails=data.frame()
idcheck=listaids[11743]

update_api<-function(idcheck){
  path = paste0(
    'http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=',
    idcheck,
    '&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82'
  )
  r <-
    GET(
      url = path,
      config = list(
        accepttimeout_ms = 5,
        connecttimeout = 5,
        dns_cache_timeout = 5,
        timeout = 5
      )
    )
  c <- content(r)
  #browser()
  urlActa = c$Listado[[1]]$Adjudicacion$UrlActa
  table.urls.iter=data.frame(id=idcheck,urlActa=urlActa)
  closeAllConnections()
  return(table.urls.iter)
}

updatelist=function(urlActa){
  
#Go to contract page to find awarding criteria
  identificador.licitacion = word(urlActa, 2, sep = "qs=")
  dirtabla = paste0(
    'https://www.mercadopublico.cl/Procurement/Modules/RFB/DetailsAcquisition.aspx?qs=',
    identificador.licitacion
  )
  
  pagina.base <- read_html(dirtabla,encoding = 'cp-1252',n=64*1024,)
  
  table = pagina.base %>% html_nodes("#grvCriterios") %>% html_table()
  
  table.criteria.iter = table[[1]] %>% as.data.frame() %>% mutate(Ítem =
                                                                     gsub(
                                                                       x = Ítem,
                                                                       pattern = '\n',
                                                                       replacement = ''
                                                                     )) %>%
    mutate(Ítem = gsub(
      x = Ítem,
      pattern = '\r',
      replacement = ''
    )) %>% mutate(Ítem = gsub('[[:digit:]]+', '',  Ítem)) %>% mutate(Ítem =
                                                                        trimws(Ítem)) %>% mutate(id = listaids[h])
  return(table.criteria.iter)
  

}  

onlydcodes=df%>%group_by(CodigoExterno)%>%summarise(CodigoExterno=CodigoExterno[1])
listaids=(onlydcodes$CodigoExterno)

#Get the URLS
table.urls=data.frame()
delay=1.5
for (h in seq(19001,21000)) {
  if(h%%500==0){
    save(table.urls,file=paste0('C:\\repos\\learn-doing\\data\\table_urls_incomplete_',h,'.Rdata'))}
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


#Get the criteria
table.criteria=data.frame()
for (h in seq(10100,16000)) {
  print(h)
  if(h%%500==0){
    print(table(table.criteria$Ítem))
    save(table.criteria,file=paste0('C:\\repos\\learn-doing\\data\\table_criteria_incomplete_',h,'.Rdata'))}
    possibleError <- tryCatch(
    updatelist(listaids[h]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")){
   fails=rbind(data.frame(h=h),fails)
     next
  }
  if(!inherits(possibleError, "error")){
  table.criteria=rbind(possibleError,table.criteria)}
  
  }
table.criteria%>%tail()

#Get the criteria, parallelized
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
result <- foreach(i = seq_along(urls),
                  .packages = "rvest",
                  .combine = "rbind",
                  .errorhandling='pass') %dopar% {
                    # get the header for each page
                    tabla.criteria.iter <- updatelist(i)
                  }







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


