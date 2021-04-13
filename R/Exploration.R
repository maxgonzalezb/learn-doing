#Explorations

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
