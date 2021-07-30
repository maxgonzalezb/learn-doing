#Explorations
## Create a df with just tje key variables
df.exploration = df%>%group_by(Codigo,RegionUnidad,tipoAdquisicion)%>%count()

# Regions
market.region = df %>% group_by(RegionUnidad) %>% summarise(
  N.contracts = length(unique(Codigo)),
  N.bids = length(Codigo),
  unique.gov = length(unique(NombreOrganismo)),
  unique.firms = length(unique(RutProveedor))
)%>%mutate(av.participants=1/N.contracts*N.bids)%>%
  left_join(read.csv('C:\\repos\\learn-doing\\data\\RegionNames.csv'))%>%arrange(Number)%>%
  select(8,2,3,6,4,5)%>%mutate(across(where(is.numeric),function(x) signif(x,2)))
colnames(market.region)<-c('Region','Contracts','Bids (N)', 'Bids/Contract','Unique Gov. Units','Unique Firms')

region.output=kable(
  market.region, "latex",
  booktabs = T,
  linesep = "",
  align = c('l',rep('l', ncol(market.region)-1)),label = 'sampleregion',
  caption = 'Sample coverage by Region'
) %>% column_spec(1, width = "2.5in") %>%
  kable_styling(latex_options = c("hold_position","scale_down"))%>%
  footnote(general="The table displays key variables for the data sample avalaible for each geographic region in Chile. Note that Ñuble was created as a separate political division in 2018.
  Previously, it was a part of the Biobio Región, and its contracts were labeled as such. ",footnote_as_chunk=T, threeparttable = T, title_format = c("italic", "underline"))

region.output%>% cat(., file = file("C:\\repos\\learn-doing\\thesis\\tables\\sampleregion.txt",encoding = 'UTF-8'))

# Top 10 government
top10.gov=df%>%group_by(NombreOrganismo)%>%summarise(
  N.contracts = length(unique(Codigo)),
  N.bids = length(Codigo),
  Years= length(unique(year)),
  unique.firms = length(unique(RutProveedor)),
  .groups = 'drop' )%>%arrange(-N.contracts)%>%slice_head(n=10)%>%mutate(av.participants=1/N.contracts*N.bids)%>%
  select(1,2,3,5)%>%mutate(across(where(is.numeric),function(x) signif(x,4)))%>%mutate(NombreOrganismo=simpleCap(substr(NombreOrganismo,1,40)))
top10.gov
colnames(top10.gov)<-c('Government Unit','Contracts Held','Bids','Firms Related')

top10.gov.output=kable(
  top10.gov, "latex",
  booktabs = T,
  linesep = "",
  align = c('l',rep('c', ncol(top10.gov)-1)),label = 'topgov',
  caption = 'Top Gov.Units'
)  %>% column_spec(1, width = "3.5in") %>%
  kable_styling(latex_options = c("hold_position","scale_down"))

top10.gov.output%>% cat(., file = file("C:\\repos\\learn-doing\\thesis\\tables\\topgov.txt",encoding = 'UTF-8'))

# Top 10 firms.
top10.firms=df%>%group_by(NombreProveedor)%>%summarise(
  N.contracts.bid = length(unique(Codigo)),
  N.contracts.won = length(unique(Codigo[winner=='Seleccionada'])),
  N.govs=length(unique(NombreOrganismo)),
  #Years= length(unique(year)),
  .groups = 'drop' )%>%arrange(-N.contracts.won)%>%slice_head(n=10)%>%
  mutate(across(where(is.numeric),function(x) signif(x,2)))%>%mutate(NombreProveedor=simpleCap(NombreProveedor))

colnames(top10.firms)<-c('Firm Name','Contracts Won','Bids', 'Gov. Units Related')

top10.firms.output=kable(
  top10.firms, "latex",
  booktabs = T,
  linesep = "",
  align = c('l',rep('c', ncol(top10.firms)-1)),label = 'topfirms',
  caption = 'Top Firms'
) %>% column_spec(1, width = "3.5in") %>%
kable_styling(latex_options = c("hold_position","scale_down"))
                      
top10.firms.output%>% cat(., file = file("C:\\repos\\learn-doing\\thesis\\tables\\topfirms.txt",encoding = 'UTF-8'))









top10.firms.output=create_kable(top10.firms,caption = 'Top Firms',label = 'topfirms',numcols = 6)


slices.exp1

# Types?











#What kinds of projects do we have in the cleaned sample?
colnames(df)

#See types of projects according to avalaible classifications
##Questions: can we identify projects that are clearly private of public beyond just going for size?

slice.projects=df%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%ungroup%>%mutate(percentile=ntile(montoOferta,100))%>%mutate()
projectType.table=slice.projects%>%group_by(Rubro3)%>%count()%>%ungroup%>%mutate(perc=n/sum(n))
projectType.table

p1.distr<-ggplot(slice.projects, aes(x=montoOferta))+geom_density(fill='lightblue', color="black", alpha=0.3) +
  scale_x_continuous( breaks=c(1e6,10e6,100e6,30e6,100e6,1000e6),trans="log1p", expand=c(0,0)) +facet_wrap(~Rubro3,ncol = 1)

p2.distr<-ggplot(slice.projects, aes(x=montoOferta))+geom_density(fill='lightblue', color="black", alpha=0.3) +
  scale_x_continuous( breaks=c(1e6,10e6,100e6,30e6,100e6,1000e6),trans="log1p", expand=c(0,0)) +facet_wrap(~`Nombre producto genérico`,ncol = 1)

table(slice.projects$`Nombre producto genérico`,slice.projects$Rubro3)
table(slice.projects$Rubro3,slice.projects$CodigoProductoONU)

casas=slice.projects%>%filter(`Nombre producto genérico`=='CONSTRUCCIÓN DE CASAS')
deptos=slice.projects%>%filter(`Nombre producto genérico`=='CONSTRUCCIÓN DE EDIFICIOS Y DEPARTAMENTOS')
infra=slice.projects%>%filter(`Nombre producto genérico`=='CONSTRUCCIÓN DE OBRAS CIVILES')
calles=slice.projects%>%filter(`Nombre producto genérico`=='PAVIMENTACIÓN DE CARRETERAS O CAMINOS')
locales=slice.projects%>%filter(`Nombre producto genérico`=='CONSTRUCCIÓN DE LOCALES, PLANTAS COMERCIALES O INDUSTRIALES')

#Crear una variable de tipo de institucion
table(df$TypeOrganism)
uk=  df[,c('NombreOrganismo','NombreOrganismo_clean','TypeOrganism')]%>%filter(TypeOrganism=='Other')%>%group_by(NombreOrganismo_clean)%>%count()%>%arrange(-n)

#CRear una variable de tipo de projecto


##Study top projects
slice.projects.top=slice.projects%>%filter(percentile>90)
head(slice.projects$percentile)

##Types of projects according to Type of Auction
summary(slice.projects$montoOferta)
nrow(slice.projects)
head(slice.projects$Nombre)
