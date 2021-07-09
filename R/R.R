source('C:\\repos\\learn-doing\\R\\functions.R')

thresholdClose=0.005
thresholdSize=20e6

df = arrow::read_feather('C:\\repos\\learn-doing\\data\\lic_construccion_feather.feather') %>%
                  rename(estadoOferta=`Estado Oferta`,montoOferta = `Valor Total Ofertado`, winner = `Oferta seleccionada`,cantidadOferta=`Cantidad Ofertada`,tipoAdquisicion=`Tipo de Adquisición`)


listaContractExp=read.csv2(file='C:\\repos\\learn-doing\\data\\experience\\ExperienceFactorContract.csv')
#glimpse(df)
#skim(df)

#Accepted Offers? Be careful computing statistics, should not eliminate straight away. TODO:desert lics. 

#Cleaning
#df <- read.csv("C:/repos/public-procurement/bids.csv", encoding="CP1252")
df=df%>%mutate(FechaInicio=as.Date(FechaInicio))%>%mutate(year=year(FechaInicio),MCA_MPO=montoOferta/MontoEstimado)
df=df%>%mutate(year=as.numeric(year))%>%mutate(rutp_clean=gsub(pattern = '.',replacement = '',tolower(RutProveedor),fixed = T))%>%
                                                    mutate(NombreOrganismo_clean=tolower(NombreOrganismo))

df=df%>%mutate(TypeOrganism=case_when(grepl(x=NombreOrganismo_clean,pattern = 'munici',fixed = T)  == T ~ "Municipality",
                                      grepl(x=NombreOrganismo_clean,pattern = 'ilustre',fixed = T)  == T ~ "Municipality",
                                      grepl(x=NombreOrganismo_clean,pattern = 'serv',fixed = T)  == T ~ "Ministry",
                                      grepl(x=NombreOrganismo_clean,pattern = 'minis',fixed = T)  == T ~ "Ministry",
                                      grepl(x=NombreOrganismo_clean,pattern = 'subse',fixed = T)  == T ~ "Ministry",
                                      grepl(x=NombreOrganismo_clean,pattern = 'dirección de obras hidráulicas',fixed = T)  == T ~ "Ministry",
                                      grepl(x=NombreOrganismo_clean,pattern = 'universidad',fixed = T)  == T ~ "University",
                                      grepl(x=NombreOrganismo_clean,pattern = 'goberna',fixed = T)  == T ~ "Regional Government",
                                      grepl(x=NombreOrganismo_clean,pattern = 'gobierno regional',fixed = T)  == T ~ "Regional Government",
                                      grepl(x=NombreOrganismo_clean,pattern = 'carabiner',fixed = T)  == T ~ "Police, Investigations",
                                      grepl(x=NombreOrganismo_clean,pattern = 'investigaciones',fixed = T)  == T ~ "Police, Investigations",
                                      grepl(x=NombreOrganismo_clean,pattern = 'gendarmer',fixed = T)  == T ~ "Police, Investigations",
                                      grepl(x=NombreOrganismo_clean,pattern = 'junta nacional de jardines',fixed = T)  == T ~ "Child School Board",
                                      grepl(x=NombreOrganismo_clean,pattern = 'ejército',fixed = T)  == T ~ "Army,Navy",
                                      grepl(x=NombreOrganismo_clean,pattern = 'armada de chile',fixed = T)  == T ~ "Army,Navy",
                                      TRUE~'Other'))

pre.summarybids=generateDfBidsSummary(bids = df)

#Indicators about missing data

#Filtering
df=df%>%filter(cantidadOferta==1&CantidadAdjudicada<=1)%>%filter(montoOferta>=10e6)
df=df%>%filter(MontoEstimado>thresholdSize)

  #Create unique ID
df=df%>%mutate(id=paste0(Codigo,RutProveedor))
df.repetidos=df%>%group_by(id)%>%select(Codigo,Nombre,NombreOrganismo,montoOferta,NombreProveedor,FechaInicio)%>%mutate(n=length(NombreProveedor))%>%filter(n>1)
df=df%>%filter(!Codigo%in%(df.repetidos$Codigo))
#df.repetidos.3=df%>%group_by(id2)%>%select(Codigo,Nombre,NombreOrganismo,montoOferta,NombreProveedor,FechaInicio)%>%mutate(n=length(NombreProveedor))%>%filter(n>1)
post.summarybids=generateDfBidsSummary(bids = df)
post.summarybids%>%create_kable(caption = 'Sample Descriptive Statistics')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\sample_descriptive.txt")
rows1=nrow(df)

#Create Firm datasets
df.wins = df %>% group_by(RutProveedor) %>% summarise(
  ofertas = length(winner),
  wins = length(winner[winner == 'Seleccionada']),
  probWin = wins / ofertas
)
df.names = df %>% dplyr::select(RutProveedor, NombreProveedor) %>% group_by(RutProveedor, NombreProveedor) %>%
  slice(1)

#Study how are the close wins
df.difs.ind=df.difs%>%mutate(isClose=ifelse(dif<0.005,yes='Close Win','No Close Win'))
df=df%>%left_join(df.difs.ind)
comparison.1=df%>%filter(isClose=='No Close Win')%>%generateDfBidsSummary()%>%dplyr::select('mean','std')%>%rename('mean_notClose'='mean','std_notClose'='std')
comparison.2=df%>%filter(isClose=='Close Win')%>%generateDfBidsSummary()%>%dplyr::select(name,'mean','std')%>%rename('mean_close'='mean','std_close'='std')%>%cbind(comparison.1)%>%select(name,mean_notClose,mean_close,std_notClose,std_close)
colnames(comparison.2)<-c('Variable','Mean (Not close win)','Mean (Close win)','Sd (Not close win)','Sd (Close win)')
#create_kable(comparison.2,caption = "Comparison between close and non-close wins")

##Merge with experiece dataset
df=df%>%left_join(listaContractExp,by=c('CodigoExterno'='id'))
df.difs = createDifsDf(df,thresholdCLose = 0.005, weightPrice = 50,thresholdCloseRank = NA)
df=df%>%left_join(df.difs)
rows2=(nrow(df))

##Create Ranked DF
df.rating.elo=createHelpElo(df)
max_players=(df.rating.elo%>%ncol()-1)/2
n = 10000
#Important. Set up how much players win/lose with each auction
base = c(25, rep(-10, max_players - 1))#seq(max_players,1,by = -1)
tabletimes = df.rating %>% select(FechaInicio, Codigo, time)
vector = seq_len(nrow(df))

vectorubicacion = df$Codigo %in% tabletimes$Codigo
winPoints=24
losePoints=8

df = CreateFullRankedDataset(
  max_players,
  df,
  df.rating.elo,
  n = 10000,
  winPoints,
  losePoints,
  startPoints = 1500
)
lista.close.ranks=createDifsDf_rank(df,thresholdCloseRank = 1.03)
df=df%>%left_join(lista.close.ranks)
rows3=nrow(df)
print(paste0('All correct in merging: ' , (rows1==rows2&rows2==rows3)))
#saveRDS(df,file = 'C:\\repos\\learn-doing\\data\\contractData.rds')
rm(df)



