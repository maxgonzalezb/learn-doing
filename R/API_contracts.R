library(readxl)
library(httr) ; library(jsonlite)

library('rvest')
library(rvest)
dfcontracts=read_xlsx('C:\\repos\\learn-doing\\data\\transparencia\\sol_AM009T0000106_2\\contratos_5.xlsx')
api_codes=dfcontracts%>%select(CODIGOCONTRATO,IDCHILECOMPRA)


path='http://api.mercadopublico.cl/servicios/v1/publico/licitaciones.json?codigo=5283-12-LP17&ticket=93BA2B5A-B87E-487B-8E37-47CB6D7F5F82'
r <- GET(url = path)
c <- content(r)
str(c)
c2<-content(r,as = 'parsed')
c3<-content(r,as = 'text')


js=fromJSON(c3)
js2=js$
c$Cantidad
c$Listado


codesActa=c('5283-12-LP17')
url="https://www.mercadopublico.cl/Procurement/Modules/RFB/StepsProcessAward/PreviewAwardAct.aspx?qs=TEI16X+tba/MeZWY9OpKPM0Ap9iBZ0j3A6duf8IHsMY="
simple <- read_html(url)
table=simple%>%html_nodes(xpath='//*[@id="grdItemOC_ctl02_ucAward_gvLines"]')%>%html_table()
table.format=table[[1]]%>%as.data.frame()%>%dplyr::select(-2)

.cssPRCGridViewHeader
simple%>%html_nodes('.cssPRCGridViewHeader') %>%
  html_text()
simple%>%html_nodes('tr.cssPRCGridViewAltRow:nth-child(3) > td:nth-child(1)')%>%
  html_text()


.cssPRCGridViewHeader
<th scope="col">Proveedor</th><th scope="col">Especificaciones del Proveedor</th><th scope="col">Monto Unitario Oferta</th><th scope="col">Cantidad Adjudicada</th><th scope="col">Total Neto Adjudicado</th><th scope="col">Estado</th>
  
  .cssPRCGridViewHeader

response
r$

headers(r)
str(content(r))


df.repsonse <- fromJSON(response, flatten = F) 
str(content(r, "parsed"))
response$Listado