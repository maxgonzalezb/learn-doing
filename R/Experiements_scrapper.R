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


#trey to use directly the search bar
site='https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1'
session <- html_session(site)
blank_form=html_form(read_html(site))
submit_form=blank_form[[1]]%>%set_values('txtSearch'='1896-63-O119')
submitted = submit_form(session=session,form=filled_form,submit = 'btnBusqueda')
submitted%>% read_html()%>%html_nodes('#lblSubTitle')
?submit_form

submitted%>% read_html()%>%html_nodes('#heaFecha > span:nth-child(1) > label:nth-child(2)')
blank_form
filled_form$fields$
  result <- submitted %>%read_html()%>%html_structure()
?submit_form
result <- submit_form(session, form)

session_submit(session, filled_form, )
?session_submit
rvest::sub
read_html(session)
?html_structure

submitted%>% read_html()%>%html_structure()
submitted$handle

form%>%html_form('txtSearch'='1896-63-O119')

set_values(`ein` = "060646973", # example EIN
           `pn` = "001") # example PN


closeAllConnections()

###CReate POST request
busqueda=httr::POST(
  url = site, 
  body = list(txtSearch	='asdsadads'),
  encode = "form")
busqueda%>% read_html()%>%html_nodes('#lblSubTitle')


busqueda2=POST(
  url = site, 
  add_headers(
    Accept= '*/*',
    `Accept-Encoding`= 'gzip, deflate, br',
    `Accept-Language`= 'es-CL,es;q=0.8,en-US;q=0.5,en;q=0.3',
    `Cache-Control`='no-cache',
    Connection='keep-alive',
    `Content-Length`= '13106',
    `Content-Type`= 'application/x-www-form-urlencoded; charset=utf-8',
    Cookie= '_gcl_au=1.1.2132036260.1617831893; ASP.NET_SessionId=g3zebvxl4v5vexyxqxi3ojlc',
    DNT= '1',
    Host= 'www.mercadopublico.cl',
    Origin= 'https://www.mercadopublico.cl',
    Referer= 'https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1',
    `User-Agent`= 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0',
    `X-MicrosoftAjax`= 'Delta=true',
    `X-Requested-With`= 'XMLHttpRequest'
  ), 
  body=list(
    ToolkitScriptManager1 = 'PanelGeneral|btnBusqueda',
    `__LASTFOCUS`	="",
    ToolkitScriptManager1_HiddenField	=";;AjaxControlToolkit, Version=4.1.40412.0, Culture=neutral, PublicKeyToken=28f01b0e84b6d53e:es-CL:acfc7575-cdee-46af-964f-5d85d9cdcf92:475a4ef5:effe2a26:1d3ed089:5546a2b:497ef277:a43b07eb:751cdd15:dfad98a5:3cf12cf1:addc6819:d2e10b12:37e2e5c9:5a682656:c7029a2:e9e598a9",
    `__EVENTTARGET`	="",
    `__EVENTARGUMENT`	="",
    `__VIEWSTATE`	="ZUOeKw6mrOnphiHkJ3XgqrQPWlCRHWsZstEhk5qUHZBOjh5ISlcaxh9o/g4oxvsqvXwWvqM+DsN1A5kI1M1eVG7DEQVE7FAv/h7jT67hDtVDPrtrs0ANLffps6r8u3gxhnrReEpBN+v6bFDCbBFXWoSxwqVC3uozHoiUfpTEUjHa+oeLMGTvlLTSSOkwItQzvEdYQ02lXHEKjCcis88KsKh6Vbarci++aE/kO+7SGqcTAVnqKcuJJ/6WInXI5uS8A5jhfhBuSP4zDOjtzFY/qseaeP7ZkwMIZ8Ox98tmat6Y1lP60NzWYwjF5IgH9RpGM3bXx4+9Wr/jkzWAq11TibeJgbdwCwpxML35JeC5Oird7UnZzEF8yGJ+5hf/9XwB/alYdgZUjzcRN6tctCw66EggdC5zNn2pvJAKC2vKQlCVJ34EB19VTGFCS7qXkmypdp9Ns37UPAEOmqrTOo8Hei62a0lOuCbDlbDczqQ0+vTwAZ9+y0S3m9R7B9eaycn.CBp8vElxcer0O7wzBHMk032xOLkom1v08OYysMm4bOb2DjAfpvH6De0X+/kTpisfnjo8+F63jkmAmjLTEqXPU68oKCWeRX10B3gIIQ03Bv1C2fPWZYyIsS/NOFICLPPC2q9LnvEiTrwZQsQUHl0qOQ4wqLel8/rDVNHk+7dJSRSEGax1votidg+/qyWBBMbVJ9WC5TZuLEAsLdxY2dfjx0pgzagoCk71jZCBQRI5sWl6z17kWPmiol4HVHFDYLX6MCRS3tErQK6BZDKTwLfswwY0rDcpzLbOG5/qxLrZKfT5dipsmujqZCZg8IalSBhum6CI/inzo0Zv3HmEaID/1hgFc0rTVGEPMZtIIqN0/mGtxXWWAuACIkCXSKU5CJcz/wJf5KlRH85FrrHeSrKw/yCVUBSbLiePeui4SaEs1mUhE2tQtkt01KNVZ5wAPWGACvAhKfSOGsXtLj+Ac/YdLDgqgShAVT07dZQz8ixtB7QnZJUSCuEX5Gv636jLE=",
    `__VIEWSTATEGENERATOR`=	"1EEC9126",
    `__SCROLLPOSITIONX`=	"0",
    `__SCROLLPOSITIONY`	="0",
    txtSearch	="asdsadads",
    rdoSearch	="Todos los organismos públicos",
    ddlRegion	="-1",
    ddlAdquisitionState	="-1",
    ddlDateType	="-1",
    txtFecha1	="25-04-2021",
    txtFecha2	="24-06-2021",
    `hdnAdquisitionState_`	="",
    hdnAccion=	"",
    txtEnterpriseName=	"",
    txtTaxIdEnterprise=	"",
    txtSearchContent=	"",
    txtAgencyName=	"",
    txtTaxId=	"",
    hdnCodigoComprador=	"",
    hdnCodigoProveedor=	"",
    hdnTablaRubros=	"",
    hdnTablaCodigosRubros	="",
    hdnBusqueda	="",
    hdnOldQuerry=	"",
    `__ASYNCPOST`	="true",
    btnBusqueda.x	="9",
    btnBusqueda.y	="13"
  ), 
  encode = "form"
) 



POST
https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1
Estado200
OK
VersiónHTTP/1.1
Transferidos68,11 KB (tamaño 67,81 KB)
Política de referenciastrict-origin-when-cross-origin


Cache-Control
no-cache
Content-Length
69433
Content-Type
text/plain; charset=iso-8859-1
Date
Tue, 25 May 2021 14:15:51 GMT
Expires
-1
Pragma
no-cache
Server
Microsoft-IIS/10.0
Strict-Transport-Security
max-age=31536000
X-Frame-Options
SAMEORIGIN
X-XSS-Protection
1; mode=block

header.post=list(
  Accept: '*/*',
  `Accept-Encoding`: 'gzip, deflate, br',
  `Accept-Language`: 'es-CL,es;q=0.8,en-US;q=0.5,en;q=0.3',
  `Cache-Control`:'no-cache',
  Connection:'keep-alive',
  `Content-Length`: 13106,
  `Content-Type`: 'application/x-www-form-urlencoded; charset=utf-8',
  Cookie: '_gcl_au=1.1.2132036260.1617831893; ASP.NET_SessionId=g3zebvxl4v5vexyxqxi3ojlc',
  DNT: 1,
  Host: 'www.mercadopublico.cl',
  Origin: 'https://www.mercadopublico.cl',
  Referer: 'https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1',
  `User-Agent`: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0',
  `X-MicrosoftAjax`: 'Delta=true',
  `X-Requested-With`: 'XMLHttpRequest'
)

#1
busqueda=POST(
  url = site, 
  add_headers(
    Accept: '*/*',
    `Accept-Encoding`: 'gzip, deflate, br',
    `Accept-Language`: 'es-CL,es;q=0.8,en-US;q=0.5,en;q=0.3',
    `Cache-Control`:'no-cache',
    Connection:'keep-alive',
    `Content-Length`: 13106,
    `Content-Type`: 'application/x-www-form-urlencoded; charset=utf-8',
    Cookie: '_gcl_au=1.1.2132036260.1617831893; ASP.NET_SessionId=g3zebvxl4v5vexyxqxi3ojlc',
    DNT: 1,
    Host: 'www.mercadopublico.cl',
    Origin: 'https://www.mercadopublico.cl',
    Referer: 'https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1',
    `User-Agent`: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0',
    `X-MicrosoftAjax`: 'Delta=true',
    `X-Requested-With`: 'XMLHttpRequest'
  ), 
  body=list(
    ToolkitScriptManager1 : 'PanelGeneral|btnBusqueda',
    `__LASTFOCUS`	:"",
    ToolkitScriptManager1_HiddenField	:";;AjaxControlToolkit, Version=4.1.40412.0, Culture=neutral, PublicKeyToken=28f01b0e84b6d53e:es-CL:acfc7575-cdee-46af-964f-5d85d9cdcf92:475a4ef5:effe2a26:1d3ed089:5546a2b:497ef277:a43b07eb:751cdd15:dfad98a5:3cf12cf1:addc6819:d2e10b12:37e2e5c9:5a682656:c7029a2:e9e598a9",
    `__EVENTTARGET`	:"",
    `__EVENTARGUMENT`	:"",
    `__VIEWSTATE`	:"ZUOeKw6mrOnphiHkJ3XgqrQPWlCRHWsZstEhk5qUHZBOjh5ISlcaxh9o/g4oxvsqvXwWvqM+DsN1A5kI1M1eVG7DEQVE7FAv/h7jT67hDtVDPrtrs0ANLffps6r8u3gxhnrReEpBN+v6bFDCbBFXWoSxwqVC3uozHoiUfpTEUjHa+oeLMGTvlLTSSOkwItQzvEdYQ02lXHEKjCcis88KsKh6Vbarci++aE/kO+7SGqcTAVnqKcuJJ/6WInXI5uS8A5jhfhBuSP4zDOjtzFY/qseaeP7ZkwMIZ8Ox98tmat6Y1lP60NzWYwjF5IgH9RpGM3bXx4+9Wr/jkzWAq11TibeJgbdwCwpxML35JeC5Oird7UnZzEF8yGJ+5hf/9XwB/alYdgZUjzcRN6tctCw66EggdC5zNn2pvJAKC2vKQlCVJ34EB19VTGFCS7qXkmypdp9Ns37UPAEOmqrTOo8Hei62a0lOuCbDlbDczqQ0+vTwAZ9+y0S3m9R7B9eaycn.CBp8vElxcer0O7wzBHMk032xOLkom1v08OYysMm4bOb2DjAfpvH6De0X+/kTpisfnjo8+F63jkmAmjLTEqXPU68oKCWeRX10B3gIIQ03Bv1C2fPWZYyIsS/NOFICLPPC2q9LnvEiTrwZQsQUHl0qOQ4wqLel8/rDVNHk+7dJSRSEGax1votidg+/qyWBBMbVJ9WC5TZuLEAsLdxY2dfjx0pgzagoCk71jZCBQRI5sWl6z17kWPmiol4HVHFDYLX6MCRS3tErQK6BZDKTwLfswwY0rDcpzLbOG5/qxLrZKfT5dipsmujqZCZg8IalSBhum6CI/inzo0Zv3HmEaID/1hgFc0rTVGEPMZtIIqN0/mGtxXWWAuACIkCXSKU5CJcz/wJf5KlRH85FrrHeSrKw/yCVUBSbLiePeui4SaEs1mUhE2tQtkt01KNVZ5wAPWGACvAhKfSOGsXtLj+Ac/YdLDgqgShAVT07dZQz8ixtB7QnZJUSCuEX5Gv636jLE=",
    `__VIEWSTATEGENERATOR`:	"1EEC9126",
    `__SCROLLPOSITIONX`:	"0",
    `__SCROLLPOSITIONY`	:"0",
    txtSearch	:"asdsadads",
    rdoSearch	:"Todos los organismos públicos",
    ddlRegion	:"-1",
    ddlAdquisitionState	:"-1",
    ddlDateType	:"-1",
    txtFecha1	:"25-04-2021",
    txtFecha2	:"24-06-2021",
    hdnAdquisitionState_	:"",
    hdnAccion:	"",
    txtEnterpriseName:	"",
    txtTaxIdEnterprise:	"",
    txtSearchContent:	"",
    txtAgencyName:	"",
    txtTaxId:	"",
    hdnCodigoComprador:	"",
    hdnCodigoProveedor:	"",
    hdnTablaRubros:	"",
    hdnTablaCodigosRubros	:"",
    hdnBusqueda	:"",
    hdnOldQuerry:	"",
    `__ASYNCPOST`	:"true",
    btnBusqueda.x	:"9",
    btnBusqueda.y	:"13"
  ), 
  encode = "form"
) 

#2. 

#2
busqueda=POST(
  url = site, 
  add_headers(
    Accept: '*/*',
    `Accept-Encoding`: 'gzip, deflate, br',
    `Accept-Language`: 'es-CL,es;q=0.8,en-US;q=0.5,en;q=0.3',
    `Cache-Control`:'no-cache',
    Connection:'keep-alive',
    `Content-Length`: 13106,
    `Content-Type`: 'application/x-www-form-urlencoded; charset=utf-8',
    Cookie: '_gcl_au=1.1.2132036260.1617831893; ASP.NET_SessionId=g3zebvxl4v5vexyxqxi3ojlc',
    DNT: 1,
    Host: 'www.mercadopublico.cl',
    Origin: 'https://www.mercadopublico.cl',
    Referer: 'https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1',
    `User-Agent`: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0',
    `X-MicrosoftAjax`: 'Delta=true',
    `X-Requested-With`: 'XMLHttpRequest'
  ), 
  body=list(
    ToolkitScriptManager1 : 'PanelGeneral|btnBusqueda',
    `__LASTFOCUS`	:"",
    ToolkitScriptManager1_HiddenField	:";;AjaxControlToolkit, Version=4.1.40412.0, Culture=neutral, PublicKeyToken=28f01b0e84b6d53e:es-CL:acfc7575-cdee-46af-964f-5d85d9cdcf92:475a4ef5:effe2a26:1d3ed089:5546a2b:497ef277:a43b07eb:751cdd15:dfad98a5:3cf12cf1:addc6819:d2e10b12:37e2e5c9:5a682656:c7029a2:e9e598a9",
    `__EVENTTARGET`	:"",
    `__EVENTARGUMENT`	:"",
    `__VIEWSTATE`	:"ZUOeKw6mrOnphiHkJ3XgqrQPWlCRHWsZstEhk5qUHZBOjh5ISlcaxh9o/g4oxvsqvXwWvqM+DsN1A5kI1M1eVG7DEQVE7FAv/h7jT67hDtVDPrtrs0ANLffps6r8u3gxhnrReEpBN+v6bFDCbBFXWoSxwqVC3uozHoiUfpTEUjHa+oeLMGTvlLTSSOkwItQzvEdYQ02lXHEKjCcis88KsKh6Vbarci++aE/kO+7SGqcTAVnqKcuJJ/6WInXI5uS8A5jhfhBuSP4zDOjtzFY/qseaeP7ZkwMIZ8Ox98tmat6Y1lP60NzWYwjF5IgH9RpGM3bXx4+9Wr/jkzWAq11TibeJgbdwCwpxML35JeC5Oird7UnZzEF8yGJ+5hf/9XwB/alYdgZUjzcRN6tctCw66EggdC5zNn2pvJAKC2vKQlCVJ34EB19VTGFCS7qXkmypdp9Ns37UPAEOmqrTOo8Hei62a0lOuCbDlbDczqQ0+vTwAZ9+y0S3m9R7B9eaycn.CBp8vElxcer0O7wzBHMk032xOLkom1v08OYysMm4bOb2DjAfpvH6De0X+/kTpisfnjo8+F63jkmAmjLTEqXPU68oKCWeRX10B3gIIQ03Bv1C2fPWZYyIsS/NOFICLPPC2q9LnvEiTrwZQsQUHl0qOQ4wqLel8/rDVNHk+7dJSRSEGax1votidg+/qyWBBMbVJ9WC5TZuLEAsLdxY2dfjx0pgzagoCk71jZCBQRI5sWl6z17kWPmiol4HVHFDYLX6MCRS3tErQK6BZDKTwLfswwY0rDcpzLbOG5/qxLrZKfT5dipsmujqZCZg8IalSBhum6CI/inzo0Zv3HmEaID/1hgFc0rTVGEPMZtIIqN0/mGtxXWWAuACIkCXSKU5CJcz/wJf5KlRH85FrrHeSrKw/yCVUBSbLiePeui4SaEs1mUhE2tQtkt01KNVZ5wAPWGACvAhKfSOGsXtLj+Ac/YdLDgqgShAVT07dZQz8ixtB7QnZJUSCuEX5Gv636jLE=",
    `__VIEWSTATEGENERATOR`:	"1EEC9126",
    `__SCROLLPOSITIONX`:	"0",
    `__SCROLLPOSITIONY`	:"0",
    txtSearch	:"asdsadads",
    rdoSearch	:"Todos los organismos públicos",
    ddlRegion	:"-1",
    ddlAdquisitionState	:"-1",
    ddlDateType	:"-1",
    txtFecha1	:"25-04-2021",
    txtFecha2	:"24-06-2021",
    hdnAdquisitionState_	:"",
    hdnAccion:	"",
    txtEnterpriseName:	"",
    txtTaxIdEnterprise:	"",
    txtSearchContent:	"",
    txtAgencyName:	"",
    txtTaxId:	"",
    hdnCodigoComprador:	"",
    hdnCodigoProveedor:	"",
    hdnTablaRubros:	"",
    hdnTablaCodigosRubros	:"",
    hdnBusqueda	:"",
    hdnOldQuerry:	"",
    `__ASYNCPOST`	:"true",
    btnBusqueda.x	:"9",
    btnBusqueda.y	:"13"
  ), 
  encode = "form"
) 


POST
https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1
Estado200
OK
VersiónHTTP/1.1
Transferidos68,11 KB (tamaño 67,81 KB)
Política de referenciastrict-origin-when-cross-origin


Cache-Control
no-cache
Content-Length
69433
Content-Type
text/plain; charset=iso-8859-1
Date
Tue, 25 May 2021 14:15:51 GMT
Expires
-1
Pragma
no-cache
Server
Microsoft-IIS/10.0
Strict-Transport-Security
max-age=31536000
X-Frame-Options
SAMEORIGIN
X-XSS-Protection
1; mode=block

header.post=list(
  Accept: '*/*',
  `Accept-Encoding`: 'gzip, deflate, br',
  `Accept-Language`: 'es-CL,es;q=0.8,en-US;q=0.5,en;q=0.3',
  `Cache-Control`:'no-cache',
  Connection:'keep-alive',
  `Content-Length`: 13106,
  `Content-Type`: 'application/x-www-form-urlencoded; charset=utf-8',
  Cookie: '_gcl_au=1.1.2132036260.1617831893; ASP.NET_SessionId=g3zebvxl4v5vexyxqxi3ojlc',
  DNT: 1,
  Host: 'www.mercadopublico.cl',
  Origin: 'https://www.mercadopublico.cl',
  Referer: 'https://www.mercadopublico.cl/Portal/Modules/Site/Busquedas/BuscadorAvanzado.aspx?qs=1',
  `User-Agent`: 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/88.0',
  `X-MicrosoftAjax`: 'Delta=true',
  `X-Requested-With`: 'XMLHttpRequest'
)
