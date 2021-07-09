library(readxl)
library(feather)
library(magrittr)
library(arrow)
library(dplyr)
library(lubridate)
library(ggplot2)
library(AER)
library(tidyr)
library(kableExtra)
library(stargazer)
library(sandwich)
library(skimr)
library(stargazer)
library(purrr)
library(broom)
library(scales)
library(cowplot)
library(PlayerRatings)
library(tictoc)
library(feather)
library(magrittr)
library(arrow)
library(dplyr)
library(lubridate)
library(ggplot2)
library(AER)
library(tidyr)
library(kableExtra)
library(stargazer)
library(sandwich)
library(skimr)
library(stargazer)
library(purrr)
library(broom)
library(cowplot)
library(stringr)

createHelpElo=function(df){
  df=df%>%arrange(FechaInicio)  
  #Create the two auxiliary dataset.
  ##Create the dataset of 
  df.ratings.1=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
    select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
    pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=RutProveedor)   
  
  ##Create the dataset of
  df.ratings.2=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
    select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
    pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=isWinner)   
  
  #The resulting dataset is smalller because it does not contain single wins(with no opponents)
  df.rating=df.ratings.1%>%left_join(df.ratings.2,by=c('Codigo'='Codigo','FechaInicio'='FechaInicio'))%>%ungroup%>%
    arrange(FechaInicio)%>%mutate(time=seq_len(length(FechaInicio)))
  df.rating.elo=df.rating%>%select(-Codigo,-FechaInicio)%>%select(time, everything())
  return(df.rating.elo)
}




createDifsDf<-function(df,thresholdCLose,thresholdCloseRank,weightPrice){
  df.difs = df %>% filter(estadoOferta == 'Aceptada') %>% group_by(Codigo) %>%
    arrange(montoOferta) %>%
    summarise(
      dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],
      difWinSecondLow = (montoOferta[winner != 'Seleccionada'][1] - montoOferta[winner == 'Seleccionada'][1]) / montoOferta[winner == 'Seleccionada'][1],
      lowest = montoOferta[1],
      second.lowest = montoOferta[2],
      difWinLowest = (
        (montoOferta[winner == 'Seleccionada'][1]-montoOferta[1]) / montoOferta[1]
      ) ,
      percPrice = max(percPrice),
      validOffers=length(Codigo),
    )%>%
    mutate(isCloseBids=difWinSecondLow<=thresholdClose&difWinLowest<=thresholdClose,
           isClose=isCloseBids&validOffers>=2&percPrice>=weightPrice)%>%select(Codigo,isClose)
  return(df.difs)
}

createDifsDf_rank<-function(df,thresholdCloseRank){
  df.difs = df %>% filter(estadoOferta == 'Aceptada') %>% group_by(Codigo) %>%
    summarise(isCloseRanking=as.numeric(length(Codigo)>=2&(max(rank)/min(rank)<=thresholdCloseRank)))
  return(df.difs)
}




CreateFullRankedDataset<-function(max_players,df,df.rating.elo,  n = 10000,winPoints,losePoints,startPoints=1500){
  
  #Important. Set up how much players win/lose with each auction
  base = c(winPoints, rep(-losePoints, max_players - 1))
  tabletimes = df.rating %>% select(FechaInicio, Codigo, time)
  vector = seq_len(nrow(df))
  vectorubicacion = df$Codigo %in% tabletimes$Codigo
  
  #Create first rankings
  df.ranked = df %>% mutate(rank = -1)
  ratings = elom(
    x = df.rating.elo[1:n, ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T
  )
  
  ## CReate by parts due to memory limitations
  df.ranked.1 = updateDfRanked(
    df.ranked = df.ranked,
    ratings = ratings,
    df.rating = df.rating,
    startPoint = 0
  )
  status = ratings$ratings
  rm(ratings)
  ratings.2 = elom(
    x = df.rating.elo[(n + 1):(2 * n), ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T,
    status = status
  )
  df.ranked.2 = updateDfRanked(
    df.ranked = df.ranked.1,
    ratings = ratings.2,
    df.rating = df.rating,
    startPoint = n
  )
  status = ratings.2$ratings
  rm(ratings.2)
  ratings.3 = elom(
    x = df.rating.elo[(2 * n+1):nrow(df.rating.elo), ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T,
    status = status
  )
  df.ranked.3 = updateDfRanked(
    df.ranked = df.ranked.2,
    ratings = ratings.3,
    df.rating = df.rating,
    startPoint = 2 * n
  )
  rm(ratings.3)
  
  #Fill all single contests which are NA right now
  df.ranked=df.ranked.3%>%left_join(tabletimes[,c(2,3)])
  table(df.ranked$rank==-1)
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(FechaInicio==min(FechaInicio),yes=startPoints,no=rank))
  
  
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  return(df.ranked)
}

updateDfRanked<-function(df.ranked, ratings,df.rating,startPoint=0,n=10000){
  largo=ratings$history[1,,1]%>%length()
  tabletimes=df.rating%>%select(FechaInicio,Codigo,time)%>%filter(time>=startPoint&time<=(startPoint+largo-1))
  vector=seq_len(nrow(df))
  maxcol=ncol(ratings$history[,,1])
  vectorubicacion=df$Codigo%in%tabletimes$Codigo
  
  ##Siguiente paso: hacer una function estilo map
 # for (i in seq(startPoint+1,startPoint+n)) {
  for (i in seq(1,nrow(df.ranked))) {
    if(i%%5000==0){print(label_percent()((i)/nrow(df.ranked)))}
    codigo=df$Codigo[i]
    if(!vectorubicacion[i]){next()}
    rut=df$RutProveedor[i]
    time1=((tabletimes[which(tabletimes$Codigo==codigo),'time'])%>%unlist())
    if(!is.na(time1)){
      df.ranked$rank[i]=ratings$history[rut,time1-startPoint+1,1]
      #print('replaced')
    }
  }
  return(df.ranked)
  
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
  urlActa = c$Listado[[1]]$Adjudicacion$UrlActa
  table.urls.iter=data.frame(id=idcheck,urlActa=urlActa)
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




createStargazerTxt<-function(table,filename){
table <- gsub("\\begin{tabular}","\\resizebox{0.95\\textwidth}{!}{\\begin{tabular}", table,fixed=T)
table <- gsub("\\end{tabular}","\\end{tabular}}", table,fixed=T)
table%>%cat(., file = paste0("C:\\repos\\learn-doing\\thesis\\tables\\",filename))
}




createDirectoryofCompanies<-function(){
directory=data.frame()
for (h in seq_len(10)) {
  a<-read_xlsx('C:\\repos\\mop-auctions\\DATA\\Tratados\\empresas\\PUB_Empresas_2005_2014_102020.xlsx',sheet = h)
  directory=a%>%bind_rows(directory)
  print(h)
}

colnames(directory)<-c('ano','rutcontratista','dv','razon','tramo','numtrab'
                       ,'inicioact','fechaterm','tipoterm','tipocon','subtipocon',
                       'tramocap','tramocapneg','rubro','subrubro','actividad','emp.region','emp.provincia','emp.comuna')


directory.simplifed=directory%>%select(1,2,5,6,7,8,17,18,19)#group_by(rutcontratista)%>%filter(!is.na(inicioact))%>%slice(1)#%>%
directory.simplifed=directory.simplifed%>%mutate(inicioact.year=year(inicioact))
save(directory.simplifed,file='C:\\repos\\learn-doing\\data\\directorySimplifedDB.Rdata')                           
}

# function source: tm.r-forge.r-project.org/faq.html#Bigrams
  BigramTokenizer <- function(x) { 
    unlist(
      lapply(ngrams(words(x), 2), paste, collapse = " "), 
      use.names = FALSE
    ) 
  }








#functions
create_kable<-function(df,caption,numcols,label=''){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', numcols),label = label,
    caption = caption
  ) %>% kable_styling(latex_options = c("hold_position","scale_down"))
}
create_kable_nocaption<-function(df,numcols){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', numcols),
  ) %>% kable_styling(latex_options = c("hold_position","scale_down"))
}


generateDfBidsSummary<-function(bids){

df=bids
df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])

general.statistics=df%>%
    filter(winner=='Seleccionada')%>%dplyr::select(Codigo,montoOferta,NumeroOferentes,year)%>%
    group_by(Codigo)%>%pivot_longer(cols=montoOferta:year)%>%group_by(name)%>%summarise(N=length(Codigo[!(is.na(Codigo))]),per_complete=length(name[!is.na(value)])/N,mean=round(mean(value),4),std=sd(value),
                                                                                          max=max(value),min=min(value))%>%mutate(name=gsub(x=name,pattern = 'montoOferta',replacement = 'Bid_Winning'))
  general.statistics.allbids=df%>%
    dplyr::select(Codigo,montoOferta)%>%
    group_by(Codigo)%>%pivot_longer(cols=montoOferta)%>%group_by(name)%>%summarise(N=length(Codigo[!(is.na(Codigo))]),per_complete=length(name[!is.na(value)])/N,mean=mean(value),std=sd(value),
                                                                                        max=max(value),min=min(value))%>%mutate(name=gsub(x=name,pattern = 'montoOferta',replacement = 'Bid (all)'))
  dif.statistics=df%>%left_join(df.difs)%>%
    filter(winner=='Seleccionada')%>%dplyr::select(Codigo,dif)%>%summarise(N=length(Codigo[!(is.na(Codigo))]),per_complete=length(dif[!is.na(dif)])/N,mean=mean(dif,na.rm = T),std=sd(dif,na.rm = T),
                                                                           max=max(dif,na.rm = T),min=min(dif,na.rm = T))%>%mutate(name='dif')%>%as.data.frame()

  general.statistics=rbind(general.statistics,dif.statistics,general.statistics.allbids)
  
  
general.statistics.output=general.statistics%>%arrange(name)%>%mutate(name=gsub(replacement = "Difference between 1st bid and 2nd (%)",pattern='dif',x=name),
                                                                     name=gsub(pattern = "montoOferta",replacement='Total Contract Amount',x=name),
                                                                     name=gsub(pattern = "Bid_Winning",replacement='Winning Bid',x=name),
                                                                     name=gsub(pattern = "NumeroOferentes",replacement='Number of Bidders per Contract',x=name),
                                                                     name=gsub(pattern = "year",replacement='Year',x=name))%>%mutate_if(is.numeric, funs(ifelse(.>1000&.<10000,yes=as.character(signif(., 4)),no=as.character(signif(., 3)))))

df.wins=df%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)
  
    
firm.statistics=df.wins%>%pivot_longer(cols=ofertas:probWin)%>%group_by(name)%>%summarise(N=length(RutProveedor),per_complete=length(name[!is.na(name)])/N,mean=mean(value,na.rm = T),std=sd(value,na.rm = T),
                                                                                            
                                                                                            max=max(value,na.rm = T),min=min(value,na.rm = T))
  
firm.statistics.output=firm.statistics%>%mutate(name=gsub(replacement = "Offers made by Firm",pattern='ofertas',x=name),
                                                             name=gsub(replacement = "Offers won by Firm",pattern='wins',x=name),
                                                             name=gsub(replacement = "Win prob. by Firm",pattern='probWin',x=name))%>%mutate_if(is.numeric, funs(as.character(signif(., 3))))
  

final.statistics=rbind(general.statistics.output,firm.statistics.output)
colnames(final.statistics)[3]<-'Complete Cases'
return(final.statistics)  

  
}

createTwoPeriodDataset<-function(df,start,stop,split1,split2,thresholdClose=0.005,filterReqExp=F,weightPrice=50){
  cutoff0=ymd(min(df$FechaInicio)) + years(start)
  cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
  cutoff2=cutoff1 + years(split2)
  df.period1=df%>%filter(FechaInicio<cutoff1&FechaInicio>=cutoff0)
  df.period2=df%>%filter(FechaInicio>=cutoff1&FechaInicio<=cutoff2)
  
  if(filterReqExp==T){
  df.period2=df.period2%>%filter(hasExp==0)
  }
  #Create winning statistics of each period
  ##Create Winning Statistics for first period
  #df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2],
  #                                                                                                    islowestBid = ((abs(montoOferta[1] - montoOferta[winner == 'Seleccionada'][1])/montoOferta[1])<=thresholdClose))
  
  
  df.difs=createDifsDf(df,thresholdCLose = thresholdCLose,thresholdCloseRank = NA,weightPrice = weightPrice)
  
  df.period1.wins=df.period1%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas,montoTotal=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'],na.rm=T),
                                                                 firstyearwin=min(year[winner=='Seleccionada']))%>%ungroup()%>%mutate(life= max(max(df.period1$year)-firstyearwin,1),annualwinspre=wins/(life))
  df.period2.wins=df.period2%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),
                                                                  accepted=length(Codigo[estadoOferta=='Aceptada']),
                                                                  wins=length(winner[winner=='Seleccionada']),
                                                                  probWin=wins/ofertas,montoTotal=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'],na.rm=T),
                                                                  probAc=accepted/ofertas, 
                                                                  avPriceWeight=mean(percPrice,na.rm=T),avQualityWeight=mean(percQuality,na.rm=T))
  
  
  #Add an ID column
  period1=paste0(cutoff0,"/",cutoff1)
  period2=paste0(cutoff1,"/",cutoff2)
  
  #Add A Pareto Variable
  df.period1.wins=df.period1.wins%>%ungroup()%>%mutate(idperiod=period1)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(sizerank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(sizerank<=0.20,yes='BIGF',no='NONBIGF'))
  df.period2.wins=df.period2.wins%>%ungroup()%>%mutate(idperiod=period2)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(sizerank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(sizerank<=0.20,yes='BIGF',no='NONBIGF'))
  
  
   ##Calculate narrow victories
  df.period1.difs=df.period1%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes,percPrice)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes),percPrice=max(percPrice))%>%left_join(df.difs)
  df.period2.difs=df.period2%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes,percPrice)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes),percPrice=max(percPrice))%>%left_join(df.difs)
  #df.period1.difs=df.period1%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(RutProveedor=RutProveedor[winner=='Seleccionada'],NumeroOferentes=max(NumeroOferentes))
  #df.period1.difs.close=(df.period1.difs)%>%filter(dif<=thresholdClose&!is.na(dif)&percPrice>=weightPrice*as.numeric(filterReqExp)&islowestBid)
  #df.period2.difs.close=(df.period2.difs)%>%filter(dif<=thresholdClose&!is.na(dif)&percPrice>=weightPrice*as.numeric(filterReqExp))
  df.period1.difs.close=(df.period1.difs)%>%filter(isClose)
  df.period2.difs.close=(df.period2.difs)%>%filter(isClose)

  df.period1.wins.close=df.period1.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
  df.period2.wins.close=df.period2.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspost_close=n)
  
  merged.wins=df.period2.wins%>%left_join(df.period1.wins,suffix = c('post','pre'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
    mutate(winsprebin=as.numeric(winspre>0))%>%left_join(df.period1.wins.close,by = 'RutProveedor')%>%mutate(winspre_close= replace_na(winspre_close,0))
  
   return(merged.wins)
  
  
}

createMultiPeriodDataset<-function(df,start,split1,split2,thresholdClose=0.005,ranks=FALSE,filterReqExp=F,thresholdCloseRank=1.08,weightPrice=50){
#Drop two years if strategy is ranks
if(ranks==TRUE){
fechamin.stability=ymd(min(df$FechaInicio))+years(2)
df=df%>%filter(FechaInicio>=fechamin.stability)
}
  
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))
multiperiod.mergedwins=data.frame()
while (cutoff2<=maxcutoff) {
  #Do stuff
  newstart=start+i
  if(ranks==FALSE){ 
  two.period.mergedwins=createTwoPeriodDataset(df,start = newstart, split1 =split1,split2=split2,thresholdClose= thresholdClose,filterReqExp=filterReqExp,weightPrice=weightPrice)
  }
  if(ranks==TRUE){
  two.period.mergedwins=createTwoPeriodDataset_ranks(df,start = newstart, split1 =split1,split2=split2,thresholdClose= thresholdClose,filterReqExp=filterReqExp,thresholdCloseRank = thresholdCloseRank)
   }
  ##Refresh
   cutoff0=ymd(min(df$FechaInicio)) + years(i)
   cutoff1=cutoff0 + years(split1)
   cutoff2=cutoff1 + years(split2)
   i=i+1
   multiperiod.mergedwins=rbind(multiperiod.mergedwins,two.period.mergedwins)
   print(cutoff2)
   } 

  
  
  return(multiperiod.mergedwins)
  
}


createAnnualizedWins<-function(df,start,split1,split2,filterReqExp = T,ranks=F,thresholdClose=0.005,thresholdCloseRank = 1.03,weightPrice=50){
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))
multiperiod.mergedwins=data.frame()
while (cutoff2<=maxcutoff) {
  #Do stuff
  newstart=start+i
  if(ranks==TRUE){
    two.period.mergedwins=createTwoPeriodDataset_ranks(df,start = start, split1 =(i+1),split2=split2,thresholdClose= thresholdClose,filterReqExp=filterReqExp,thresholdCloseRank = thresholdCloseRank)
    two.period.mergedwins=two.period.mergedwins%>%mutate(
                                                         annualwinspre_close=winspre_close/life,
                                                           annualwinspre_closerank= winspre_closerank/life
                                                         )
    
    }
  if(ranks==FALSE){
    two.period.mergedwins=createTwoPeriodDataset(df,start = start, split1 =(i+1),split2=split2,thresholdClose= thresholdClose,filterReqExp=filterReqExp,weightPrice=weightPrice)
  two.period.mergedwins=two.period.mergedwins%>%mutate(
                                                       annualwinspre_close=winspre_close/life)
  }
  
  #Create annualized experience
  ##Refresh
  cutoff0=ymd(min(df$FechaInicio)) + years(i)
  cutoff1=cutoff0 + years(split1)
  cutoff2=cutoff1 + years(split2)
  i=i+1
  multiperiod.mergedwins=rbind(multiperiod.mergedwins,two.period.mergedwins)
  print(cutoff2)
} 


multiperiod.mergedwins=multiperiod.mergedwins%>%mutate(annualwinspre_close=ifelse(is.na(annualwinspre_close),yes = 0,no=annualwinspre_close))

return(multiperiod.mergedwins)

}
# contractors.statistics=df%>%group_by(RutProveedor,NombreProveedor)%>%summarise(firstYear=min(year),
#                                                                                lastYear=max(year),
#                                                                                life=lastYear-firstYear,
#                                                                                firstYearAw=min(year[CantidadAdjudicada>=1]),
#                                                                                uniqueOrganism=length(unique(NombreOrganismo)),
#                                                                                uniqueUnits=length(unique(NombreUnidad)),
#                                                                                tot=length(Codigo),
#                                                                                totAdj=length(Codigo[CantidadAdjudicada>=1]),
#                                                                                montoTot=sum(montoOferta[CantidadAdjudicada>=1]))%>%arrange(-montoTot)%>%ungroup()%>%
#   mutate(acum=cumsum(montoTot)/sum(montoTot))%>%mutate(Pareto=ifelse(acum<=0.9,1,0))

createTwoPeriodDataset_ranks<-function(df,start,stop,split1,split2,thresholdClose=0.005,filterReqExp=F,thresholdCloseRank=1.08){
  cutoff0=ymd(min(df$FechaInicio)) + years(start)
  cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
  cutoff2=cutoff1 + years(split2)
  df.period1=df%>%filter(FechaInicio<cutoff1&FechaInicio>=cutoff0)
  df.period2=df%>%filter(FechaInicio>=cutoff1&FechaInicio<=cutoff2)
  
  if(filterReqExp==T){
    df.period2=df.period2%>%filter(hasExp==0)
  }
 
  if(filterReqExp==F){
    df.period2=df.period2%>%filter(hasExp==1)
  }
   
  
  #Create winning statistics of each period
  ##Create Winning Statistics for first period
  df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2],closeRanking=as.numeric(length(Codigo)>=2&(max(rank)/min(rank)<=thresholdCloseRank)))
  df.period1.wins=df.period1%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas,montoTotal=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'],na.rm=T),
                                                                  firstyearwin=min(year[winner=='Seleccionada']))%>%ungroup()%>%mutate(life= max(max(df.period1$year)-firstyearwin,1),annualwinspre=wins/(life))
  df.period2.wins = df.period2 %>% group_by(RutProveedor) %>% summarise(
    ofertas = length(winner),
    accepted=length(Codigo[estadoOferta=='Aceptada']),
    wins = length(winner[winner == 'Seleccionada']),
    probWin = wins / ofertas,
    probAc= accepted/ofertas,
    montoTotal = sum(`Monto Estimado Adjudicado`[winner == 'Seleccionada'], na.rm =
                       T)
  )
  
  #Add an ID column
  period1=paste0(cutoff0,"/",cutoff1)
  period2=paste0(cutoff1,"/",cutoff2)
  
  #Add A Pareto Variable
  df.period1.wins=df.period1.wins%>%ungroup()%>%mutate(idperiod=period1)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(sizerank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(sizerank<=0.20,yes='BIGF',no='NONBIGF'))
  df.period2.wins=df.period2.wins%>%ungroup()%>%mutate(idperiod=period2)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(sizerank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(sizerank<=0.20,yes='BIGF',no='NONBIGF'))
  
  
  ##Calculate narrow victories
  df.period1.difs=df.period1%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
  df.period2.difs=df.period2%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
  #df.period1.difs=df.period1%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(RutProveedor=RutProveedor[winner=='Seleccionada'],NumeroOferentes=max(NumeroOferentes))
  df.period1.difs.close=(df.period1.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
  df.period2.difs.close=(df.period2.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
  
  df.period1.wins.close=df.period1.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
  df.period2.wins.close=df.period2.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspost_close=n)
  
  ##Calculate narrow ranking victories
  df.period1.difs.closerank=(df.period1.difs)%>%filter(closeRanking==1)%>%group_by(RutProveedor)%>%count()%>%rename(winspre_closerank=n)%>%select(RutProveedor,winspre_closerank)
  df.period2.difs.closerank=(df.period2.difs)%>%filter(closeRanking==1)%>%group_by(RutProveedor)%>%count()%>%rename(winspost_closerank=n)%>%select(RutProveedor,winspost_closerank)
  
  merged.wins=df.period2.wins%>%left_join(df.period1.wins,suffix = c('post','pre'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
    mutate(winsprebin=as.numeric(winspre>0))%>%
    left_join(df.period1.wins.close,by = 'RutProveedor')%>%
    left_join(df.period1.difs.closerank,by = 'RutProveedor')%>%
    mutate(winspre_close= replace_na(winspre_close,0))%>%
    mutate(winspre_closerank= replace_na(winspre_closerank,0))
  
  
  
  return(merged.wins)
  
  
}
  

