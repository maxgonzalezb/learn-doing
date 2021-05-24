library(readxl)
library(httr) ; library(jsonlite)
library('rvest')
library(tibble())
library(stringr)

#1. Get awarding criteria
contracts=data.frame()
bidders=data.frame()
fails=data.frame()

updatelist=function(idcheck){
  
  path=paste0('http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=',idcheck,'&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82')
  r <- GET(url = path)
  c <- content(r)  
  urlActa=c$Listado[[1]]$Adjudicacion$UrlActa
  
  #Intentar encontrar los criterios
  
  identificador.licitacion=word(urlActa, 2, sep="qs=")
  dirtabla=paste0('https://www.mercadopublico.cl/Procurement/Modules/RFB/DetailsAcquisition.aspx?qs=',identificador.licitacion)
  pagina.base <- read_html(dirtabla)
  table=pagina.base%>%html_nodes("#grvCriterios")%>%html_table()
  
    table.criteria.iter=table[[1]]%>%as.data.frame()%>%mutate(Ítem=gsub(x=Ítem,pattern = '\n',replacement=''))%>%
      mutate(Ítem=gsub(x=Ítem,pattern = '\r',replacement=''))%>%mutate(Ítem=gsub('[[:digit:]]+', '', Ítem))%>%mutate(Ítem=trimws(Ítem))%>%mutate(id=listaids[h])
  return(table.criteria.iter)
 
  #Antiguo

  
}  

onlydcodes=df%>%group_by(CodigoExterno)%>%summarise(CodigoExterno=CodigoExterno[1])
listaids=(onlydcodes$CodigoExterno)
table.criteria=data.frame()
for (h in seq(2627,length(listaids))) {
  print(h)
  if(h%%500==0){
    print(table(table.criteria$Ítem))
    save(table.criteria,file='C:\\repos\\learn-doing\\data\\table_criteria_incomplete.Rdata')}
  possibleError <- tryCatch(
    updatelist(listaids[h]),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  if(!inherits(possibleError, "error")){
  table.criteria=rbind(possibleError,table.criteria)}
  
  }
table.criteria%>%tail()
 
#2. Get full contract information
for (h in seq(1,length(listaids))) {
print(h)
 
path=paste0('http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=',listaids[h],'&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82')
r <- GET(url = path)
c <- content(r)  
if(length(c$Cantidad)==0){
  fails=rbind(data.frame(id=h),fails)
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
pagina.base <- read_html(dirtabla)
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


#3. Old attempts
library(rvest)
listaids=toupper(gsub((conlist%>%filter(ano>2015))$id,pattern=' ',replacement='-'))
listaids=listaids[nchar(listaids)<=14]
listaids=sample(df$CodigoExterno,size = 3)
dfcontracts=read_xlsx('C:\\repos\\learn-doing\\data\\transparencia\\sol_AM009T0000106_2\\contratos_5.xlsx')
api_codes=dfcontracts%>%select(CODIGOCONTRATO,IDCHILECOMPRA)

path='http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=1736-267-LQ16&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82'
r1 <- GET(url = path)
c1 <- content(r1)
c1$Listado

c2<-content(r,as = 'parsed')
c3<-content(r,as = 'text')
C

js=fromJSON(c3)
js2=js$
  c$Cantidad
do.call(rbind.data.frame, c$Listado[[1])

url="https://www.mercadopublico.cl/Procurement/Modules/RFB/StepsProcessAward/PreviewAwardAct.aspx?qs=TEI16X+tba/MeZWY9OpKPM0Ap9iBZ0j3A6duf8IHsMY="
c('5283-12-LP17')

simple <- read_html(urlActa)
table=simple%>%html_nodes(xpath='//*[@id="grdItemOC_ctl02_ucAward_gvLines"]')%>%html_table()
table.format=table[[1]]%>%as.data.frame()%>%dplyr::select(-2)

conlist=read_xlsx('C:\\repos\\learn-doing\\data\\transparencia\\contratos_2005_al_2019.xlsx')
colnames(conlist)=c('ano','nombre','id','bip')
conlist%>%filter(ano>2015)
a=conlist[conlist$ano>2015,]
a$id

l=lengths(c$Listado[[1]])==1
infonounitaria=c$Listado[[1]][!l]
info1=c$Listado[[1]][l]%>%as.data.frame()
info2=infonounitaria$Comprador%>%as.data.frame()
info3=t(infonounitaria$Fechas)%>%as.data.frame()
info4=t(infonounitaria$Adjudicacion)%>%as.data.frame()
info5=infonounitaria$Items$Listado%>%as.data.frame()
infocontrato=cbind(info1,info2,info3,info4,info5)
colnames(infocontrato)
path=('http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=1896-63-O119&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82')



info1$Tipo
info5$Tipo
ncol(infocontrato)
ncol(contracts)
table(names(infocontrato))
base=infocontrato[2:1,]
colnames(infocontrato)[!colnames(infocontrato)%in%colnames(contracts)]
head(conlist)
contracts$JustificacionMontoEstimado
infocontrato$MontoEstimado
colnames(infocontrato)
info4$

info4$
info2$DireccionUnidadesc
info1$Desc
info1$Desc
info1$Desc


response
r$

headers(r)
str(content(r))


df.repsonse <- fromJSON(response, flatten = F) 
str(content(r, "parsed"))
response$Listado