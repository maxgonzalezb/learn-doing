
library(readxl)
library(httr) 
library(jsonlite)
library(rvest)
library(stringr)
library(parallel)
library(doParallel)


getCriteriaFromURLS_parallel<-function(listaUrlsActas.faltantes,start, end, pass, saveresults=True,externaldelay=0,innerdelay=1.5){

  table.criteria=data.frame()
  for (j in seq(start,end)) {
  minidata=listaUrlsActas.faltantes[(pass*j):(pass*j+pass-1),]
  print(paste0(pass*j," ",(pass*j+pass)))
  #Get the criteria, parallelized
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  result <- foreach(i = seq_len(nrow(minidata)),
                    .packages = c("rvest",'stringr','dplyr','httr'),
                    .combine = "rbind",
                    .errorhandling='pass') %dopar% {
                      # get the header for each page
                    Sys.sleep(innerdelay)
                    table.criteria.iter<-updatelist(urlActa = minidata$urlActa[i],idcheck = minidata$id[i],innerdelay=innerdelay,logfile=F,timeoutt=60)
                    i
                    }
  print(head(result,n = 2))
  table.criteria=rbind(result,table.criteria)
  Sys.sleep(externaldelay)
  closeAllConnections()
  numtot=pass*(j-start+1)

  if(saveresults&(numtot%%500==0)){
  save(table.criteria,file=paste0('C:\\repos\\learn-doing\\data\\criteria\\table_criteria_incomplete_',(j*pass),gsub(x=as.character(Sys.time()),pattern = ':',''),'.Rdata'))
  print('Table written')
  print(head(table.criteria,n = 2))
  table.criteria=table.criteria[1,]
    }
  }
  return(table.criteria)
}  

getCriteriaFromURLS<-function(start, end, pass,delay, saveresults=True){
table.criteria=data.frame()
  for (h in seq(start,end)) {
  if(h%%500==0&saveresults){
    print(table(table.criteria$Ítem))
    save(table.criteria,file=paste0('C:\\repos\\learn-doing\\data\\table_criteria_incomplete_',h,gsub(x=as.character(Sys.time()),pattern = ':',''),'.Rdata'))
    
    }
    possibleError <- tryCatch(
    updatelist(urlActa = listaUrlsActas.faltantes$urlActa[h],idcheck = listaUrlsActas.faltantes$id[h]),
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

}

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

updatelist=function(urlActa,idcheck,innerdelay=0,logfile=F,timeoutt=100){
  
  #Go to contract page to find awarding criteria
  identificador.licitacion = word(urlActa, 2, sep = "qs=")
  dirtabla = paste0(
    'https://www.mercadopublico.cl/Procurement/Modules/RFB/DetailsAcquisition.aspx?qs=',
    identificador.licitacion
  )
  if(logfile){write.table('C:\\repos\\learn-doing\\data\\logfile.txt' ,idcheck)}
  
  pagina.base <-  GET(dirtabla, timeout(timeoutt)) %>% read_html(encoding = 'cp-1252',n=64*1024) 
  
  table = pagina.base %>% html_nodes("#grvCriterios") %>% html_table()
  table.criteria.iter=data.frame(Ítem=c(NA),Observacione=c(NA),Ponderación=c(NA),id=c(NA))
  table.criteria.iter.2 = table[[1]] %>% as.data.frame() %>% mutate(Ítem =
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
                                                                        trimws(Ítem)) %>% mutate(id = idcheck)
  Sys.sleep(innerdelay)
  if(!is.data.frame(table.criteria.iter.2)|ncol(table.criteria.iter.2)!=4){return(table.criteria.iter)}
  return(table.criteria.iter.2)
}  

createDefinitiveDatasetURLS<-function(){
  filenames <- list.files("C:\\repos\\learn-doing\\data\\urls\\", pattern="*.Rdata", full.names=TRUE)
  mylist<- lapply(filenames, function(x) {
    load(file = x)
    get(ls()[ls()!= "filename"])
  })
  all <- do.call("rbind", mylist)
  all.simplified=all%>%group_by(id,urlActa)%>%count()
  return(all.simplified)  
}

createDefinitiveDatasetCriteria<-function(){
  filenames <- list.files("C:\\repos\\learn-doing\\data\\criteria\\", pattern="*.Rdata", full.names=TRUE)
  mylist<- lapply(filenames, function(x) {
    load(file = x)
    get(ls()[ls()!= "filename"])
  })
  for (v in seq_len(length(mylist))) {
    print(paste0(ncol(mylist[[v]]),"  ",filenames[v]))
  }
  
  all <- do.call("rbind", mylist)
  all.simplified=all%>%group_by(Ítem,Observaciones,Ponderación,id)%>%count()
  return(all.simplified)  
}

# #Probar. todos desde el 15.000 hasta el 15050
# #1
# time = proc.time()
# c1=getCriteriaFromURLS(start=13150,end=13170,pass=1,saveresults=F,delay=1.5)
# time.1 = proc.time()-time
# 
# #10
# time = proc.time()
# c10=getCriteriaFromURLS_parallel(listaUrlsActas.faltantes,start=1500,end=1501,pass=10,saveresults=T,innerdelay = 0.5,externaldelay = 5)
# time.10 = proc.time()-time
# 
# #25
# time = proc.time()
# c25=getCriteriaFromURLS_parallel(listaUrlsActas.faltantes,start=600,end=601,pass=25,saveresults=F,innerdelay = 2,externaldelay = 5)
# time.25 = proc.time()-time
# 
# #50
# time =proc.time()
# c50=getCriteriaFromURLS_parallel(listaUrlsActas.faltantes,start=300,end=300,pass=50,saveresults=F,innerdelay = 1.5,externaldelay = 6)
# time.50 = proc.time()-time
# 
# print(time.1)
# print(time.10)
# print(time.25)
# print(time.50)




