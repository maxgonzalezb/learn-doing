library(readxl)
library(VFP)
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
create_kable<-function(df,caption,numcols){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', numcols),
    caption = caption
  ) %>% kable_styling(latex_options = c("hold_position"))
}
create_kable_nocaption<-function(df,numcols){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', numcols),
  ) %>% kable_styling(latex_options = c("hold_position"))
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

createTwoPeriodDataset<-function(df,start,stop,split1,split2,thresholdClose=0.005){
  cutoff0=ymd(min(df$FechaInicio)) + years(start)
  cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
  cutoff2=cutoff1 + years(split2)
  df.period1=df%>%filter(FechaInicio<cutoff1&FechaInicio>=cutoff0)
  df.period2=df%>%filter(FechaInicio>=cutoff1&FechaInicio<=cutoff2)
  
  #Create winning statistics of each period
  ##Create Winning Statistics for first period
  df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])
  df.period1.wins=df.period1%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas,montoTotal=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'],na.rm=T))
  df.period2.wins=df.period2%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas,montoTotal=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'],na.rm=T))
  
  #Add an ID column
  period1=paste0(cutoff0,"/",cutoff1)
  period2=paste0(cutoff1,"/",cutoff2)
  
  #Add A Pareto Variable
  df.period1.wins=df.period1.wins%>%ungroup()%>%mutate(idperiod=period1)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(rank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(rank<=0.20,yes='BIGF',no='NONBIGF'))
  df.period2.wins=df.period2.wins%>%ungroup()%>%mutate(idperiod=period2)%>%mutate(perc=montoTotal/sum(montoTotal))%>%arrange(-perc)%>%mutate(rank=(seq_len(length(perc))/length(perc)),acum=cumsum(perc),paretoInd=ifelse(rank<=0.20,yes='BIGF',no='NONBIGF'))
  
  
   ##Calculate narrow victories
  df.period1.difs=df.period1%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
  df.period2.difs=df.period2%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
  #df.period1.difs=df.period1%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(RutProveedor=RutProveedor[winner=='Seleccionada'],NumeroOferentes=max(NumeroOferentes))
  df.period1.difs.close=(df.period1.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
  df.period2.difs.close=(df.period2.difs)%>%filter(dif<=thresholdClose&!is.na(dif))

  df.period1.wins.close=df.period1.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
  df.period2.wins.close=df.period2.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
  
  merged.wins=df.period2.wins%>%left_join(df.period1.wins,suffix = c('post','pre'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
    mutate(winsprebin=as.numeric(winspre>0))%>%left_join(df.period1.wins.close,by = 'RutProveedor')%>%mutate(winspre_close= replace_na(winspre_close,0))
  
  
  
  return(merged.wins)
  
  
}

createMultiPeriodDataset<-function(df,start,split1,split2,thresholdClose=0.005){
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))
multiperiod.mergedwins=data.frame()
while (cutoff2<=maxcutoff) {
  #Do stuff
  newstart=start+i
   two.period.mergedwins=createTwoPeriodDataset(df,start = newstart, split1 =split1,split2=split2,thresholdClose= thresholdClose)
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


createAnnualizedWins<-function(df,start,split1,split2){
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))
multiperiod.mergedwins=data.frame()
while (cutoff2<=maxcutoff) {
  #Do stuff
  newstart=start+i
  two.period.mergedwins=createTwoPeriodDataset(df,start = start, split1 =(i+1),split2=split2 )
  #Create annualized experience
  two.period.mergedwins=two.period.mergedwins%>%mutate(annualwinspre=winspre/(i+1),annualwinspre_close=winspre_close/(i+1))
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


  

