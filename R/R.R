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
                                                    mutate(NombreOrganismo_clean=tolower(NombreOrganismo))%>%
    mutate(FechaEnvioOferta=as.Date(FechaEnvioOferta))

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

#pre.summarybids=generateDfBidsSummary(bids = df)

#Indicators about missing data

# Auxiliary dataset of multiple 
moreThanOneItem=df%>%group_by(Codigo)%>%summarise(cantidadAdjudicada.sum=sum(CantidadAdjudicada),
                                                                         cantidadAdjudicada.max=max(CantidadAdjudicada),
                                                                         cantidadOfertada.max=max(cantidadOferta))%>%
                                                                      filter(cantidadAdjudicada.max>1|cantidadOfertada.max>1)

lowContracts=df%>%group_by(Codigo)%>%summarise(bid.max=max(montoOferta,na.rm = T))%>%filter(bid.max<15e6)

#Filtering contracts with more than oine item offered.
df=df%>%filter(!Codigo%in%moreThanOneItem$Codigo)

# Filter too cheap contracts. If estimate present, bigger than. in any case, always bigger than 10e6. 
df=df%>%filter((is.na(MontoEstimado)|MontoEstimado>thresholdSize)&
                 !(Codigo%in%(lowContracts$Codigo)))

#nrow(df)OLD

#df=df%>%filter(cantidadOferta==1&CantidadAdjudicada<=1)
#df=df%>%filter(montoOferta>=10e6)
#df=df%>%filter(MontoEstimado>thresholdSize)

#Create unique ID
df = df %>% mutate(id = paste0(Codigo, RutProveedor))
df=df%>%mutate(indWinner=as.numeric(winner=='Seleccionada'),indAccepted=as.numeric(estadoOferta=='Seleccionada'))
df=df%>%group_by(id)%>%slice_max(n=1,with_ties = T,order_by =FechaEnvioOferta)

df.repetidos = df %>% group_by(id) %>% select(Codigo,
                                              RutProveedor,
                                              Nombre,
                                              NombreOrganismo,
                                              montoOferta,
                                              NombreProveedor,
                                             FechaEnvioOferta,
                                              FechaInicio) %>% mutate(n = length(NombreProveedor)) %>% filter(n > 1)
#df = df %>% filter(!Codigo %in% (df.repetidos$Codigo))
#df.repetidos.3=df%>%group_by(id2)%>%select(Codigo,Nombre,NombreOrganismo,montoOferta,NombreProveedor,FechaInicio)%>%mutate(n=length(NombreProveedor))%>%filter(n>1)
#This selects the latest date, or the one that was accepted, otherwise it is random. 
df=df%>%group_by(id)%>%arrange(-indWinner,-indAccepted,desc(FechaEnvioOferta))%>%slice_head(n = 1)
df=df%>%ungroup()%>%select(-id)

post.summarybids=generateDfBidsSummary(bids = df)
post.summarybids%>%create_kable(caption = 'Sample Descriptive Statistics',label='sample_descriptive')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\sample_descriptive.txt")
post.summaryorganism=generateGovSummary(df = df)
post.summaryorganism%>%create_kable(caption = 'Government Bodies Descriptive Statistics',label='gov_descriptive')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\gov_descriptive.txt")
post.summarygovtypes=df%>%group_by(TypeOrganism)%>%summarise(n=length(unique(Codigo)))%>%arrange(-n)%>%mutate(perc1=(n/sum(n)), cumu=label_percent(2)(cumsum(perc1)))%>%mutate(perc1=label_percent(2)(perc1))
colnames(post.summarygovtypes)<-c('Type of Government Body','Number of Contracts Performed','Percentage','Cumulative Percentage')
post.summarygovtypes%>%create_kable(caption = 'Types of Government Bodies Developing contracts',label = 'gov_descriptive_types')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\gov_descriptive_types.txt")

post.summarytimes=generateTimeSummary(df)
post.summarytimes%>%create_kable(label='time_descriptive',caption = 'Number of firms and contract per sample year')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\time_descriptive.txt")
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
#df.difs.ind=df.difs%>%mutate(isClose=ifelse(dif<0.005,yes='Close Win','No Close Win'))
#df=df%>%left_join(df.difs.ind)
#comparison.1=df%>%filter(isClose=='No Close Win')%>%generateDfBidsSummary()%>%dplyr::select('mean','std')%>%rename('mean_notClose'='mean','std_notClose'='std')
#comparison.2=df%>%filter(isClose=='Close Win')%>%generateDfBidsSummary()%>%dplyr::select(name,'mean','std')%>%rename('mean_close'='mean','std_close'='std')%>%cbind(comparison.1)%>%select(name,mean_notClose,mean_close,std_notClose,std_close)
#colnames(comparison.2)<-c('Variable','Mean (Not close win)','Mean (Close win)','Sd (Not close win)','Sd (Close win)')
#create_kable(comparison.2,caption = "Comparison between close and non-close wins")

##Merge with experiece dataset
df=df%>%left_join(listaContractExp,by=c('CodigoExterno'='id'))
df.difs = createDifsDf(df,thresholdClose = 0.005, weightPrice = 50,thresholdCloseRank = NA)
df=df%>%left_join(df.difs)
rows2=(nrow(df))

##Create Ranked DF
helpers.ELO=createHelpElo(df=df)
df.rating=helpers.ELO[[1]]
df.rating.elo=helpers.ELO[[2]]

max_players=(df.rating.elo%>%ncol()-1)/2
n = 7500

 #Important. Set up how much players win/lose with each auction
#base = c(25, rep(-10, max_players - 1))#seq(max_players,1,by = -1)
tabletimes = df%>% select(FechaInicio, Codigo, time)
vector = seq_len(nrow(df))

rm(df.names,listaContractExp,lowContracts,moreThanOneItem)
#vectorubicacion = df$Codigo %in% tabletimes$Codigo
winPoints=24
losePoints=8

df = CreateFullRankedDataset(
  max_players,
  df = df,
  df.rating=df.rating,
  df.rating.elo = df.rating.elo,
  n = n,
  winPoints,
  losePoints,
  startPoints = 1500
)
df=cleanRankDatabase(df.ranked = df)
lista.close.ranks=createDifsDf_rank(df,thresholdCloseRank = 1.03)
df=df%>%left_join(lista.close.ranks)
df=df%>%mutate(isCloseRanking=ifelse(is.na(isCloseRanking),yes=0,no=isCloseRanking))
df=df%>%mutate(isClose=ifelse(is.na(isClose),yes=0,no=isClose))

rows3=nrow(df)
print(paste0('All correct in merging: ' , (rows1==rows2&rows2==rows3)))
#saveRDS(df,file = 'C:\\repos\\learn-doing\\data\\contractData.rds')
#rm(df)



