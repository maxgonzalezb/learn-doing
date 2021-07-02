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

source('C:\\repos\\learn-doing\\R\\functions.R')

thresholdClose=0.005
thresholdSize=20e6

df = arrow::read_feather('C:\\repos\\learn-doing\\data\\lic_construccion_feather.feather') %>%
                  rename(estadoOferta=`Estado Oferta`,montoOferta = `Valor Total Ofertado`, winner = `Oferta seleccionada`,cantidadOferta=`Cantidad Ofertada`,tipoAdquisicion=`Tipo de Adquisición`)


listaContractExp=read.csv2(file='C:\\repos\\learn-doing\\data\\experience\\ExperienceFactorContract.csv')
#glimpse(df)
#skim(df)

#Accepted Offers? Be careful computing statistics, should not eliminate straight away. TODO:desert lics. 
colnames(df)
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
save(df,file = 'C:\\repos\\learn-doing\\data\\contractData.Rdata')
post.summarybids=generateDfBidsSummary(bids = df)
post.summarybids%>%create_kable(caption = 'Sample Descriptive Statistics')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\sample_descriptive.txt")

#Create Firm datasets
df.difs = createDifsDf(df,thresholdCLose = 0.005, weightPrice = 50,thresholdCloseRank = NA)
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
create_kable(comparison.2,caption = "Comparison between close and non-close wins")


##Merge with experiece dataset
df=df%>%left_join(listaContractExp,by=c('CodigoExterno'='id'))
#rm(df)

####################################
#First Measure of Computation
####################################

#First Analysis of Experience
start=0
split1=2
split2=2
#merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
mutate(logWinpre=log(winspre+1))

## Create summarised data for graphs later
merged.wins.means.exp1=merged.wins%>%group_by(winspre)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                              q0.25=quantile(probWinpost,probs = 0.25),
                                                              q0.75=quantile(probWinpost,probs = 0.75),
                                                              std.dev=sd(probWinpost),
                                                              sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre>0,'Experience','No experience'))

merged.wins.close.price.exp1=merged.wins%>%filter((((winspre==winspre_close)&winspre_close>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.price.means.exp1=merged.wins.close.price.exp1%>%group_by(winspre_close)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                          q0.25=quantile(probWinpost,probs = 0.25),
                                                                          q0.75=quantile(probWinpost,probs = 0.75),
                                                                          std.dev=sd(probWinpost),
                                                                          sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre_close>0,'Experience','No experience'))



### Create descriptive of the slices
slices=data.frame()
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))
multiperiod.mergedwins=data.frame()

while (cutoff2<=maxcutoff) {
  start=0
  split1=2
  split2=2
  
  #Do stuff
  newstart=start+i
  cutoff0=ymd(min(df$FechaInicio)) + years(i)
  cutoff1=cutoff0 + years(split1)
  cutoff2=cutoff1 + years(split2)
  i=i+1
period1=paste0(cutoff0,"/",cutoff1)
period2=paste0(cutoff1,"/",cutoff2)
  print(period1)
  
df.period1=df%>%filter(FechaInicio<cutoff1&FechaInicio>=cutoff0)
df.period2=df%>%filter(FechaInicio>=cutoff1&FechaInicio<=cutoff2)%>%filter(hasExp==0)  
  
tot.contracts=length(unique(df.period1$Codigo))+length(unique(df.period2$Codigo))
tot.contracts.p1=length(unique(df.period1$Codigo))
tot.contracts.p2=length(unique(df.period2$Codigo))
obs=length(unique(df.period2$RutProveedor))

row=data.frame(slice=i,exp.comp=period1,outcome.comp=period2,obs,
               years.exp.comp=round(difftime(time1 = cutoff0,time2 = cutoff1,units = 'weeks')/52.25)%>%abs()%>%as.numeric(),
               years.out.comp=round(difftime(time1 = cutoff1,time2 = cutoff2,units = 'weeks')/52.25)%>%abs()%>%as.numeric(),
               tot.contracts.p1=tot.contracts.p1,tot.contracts.p2=tot.contracts.p2)  
slices=rbind(row,slices) 
} 
slices=slices%>%arrange(slice)
colnames(slices)<-c('Slice','Period 1 dates','Period 2 dates','Observations','Length Period 1','Length Period 2','Contracts in Period 1','Contracts in Period 2')
table.slices.exp1=create_kable(slices,caption = 'Analysis dataset characteristics for experience computed in rolling periods of two years',label = 'slices_exp1')
table.slices.exp1%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_slices_exp1.txt")


##Analyze the close contracts

df.difs
table.close.contracts = df %>%left_join(df.difs) %>%filter(estadoOferta == 'Aceptada') %>%
  group_by(Codigo) %>% summarise(closePrice = as.numeric(length(Codigo) >=
                                                             2 & (
                                                               max(dif) <=0.005
                                                               &
                                                                 max(percPrice)>=50
                                                               &
                                                                 max(islowestBid)==1
                                                             )))
table(table.close.contracts$closePrice)

comparison.1 = df%>%left_join(df.difs) %>% filter(isClose == F) %>% generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_notClose' = 'mean', 'std_notClose' =
                                            'std')
comparison.2 =  df%>%left_join(df.difs) %>% filter(isClose == T) %>% generateDfBidsSummary() %>%
  dplyr::select(name, 'mean', 'std') %>% rename('mean_close' = 'mean', 'std_close' =
                                                  'std') %>% cbind(comparison.1) %>% select(name, mean_notClose, mean_close, std_notClose, std_close)
colnames(comparison.2) <-
  c(
    'Variable',
    'Mean (Not close win)',
    'Mean (Close win)',
    'Sd (Not close win)',
    'Sd (Close win)'
  )

table_output = create_kable(comparison.2[1:5, ], caption = "Comparison between close and non-close wins, by price", label =
                              'closewins_alt1_desc')
table_output %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_closewins_alt1_desc.txt")


table(table.close.contracts$closePrice)/nrow(table.close.contracts)

#Graphic analysis
#Begin Regression Analysis with outcome variable as probability
##OLS Specs
##1
lm.1<-lm(probWinpost~(winspre>0),data = merged.wins)
robust.lm1<- vcovHC(lm.1, type = "HC1")%>%diag()%>%sqrt()
summary(lm.1)

##2
lm.2<-lm(probWinpost~winspre,data = merged.wins)
robust.lm2<- vcovHC(lm.2, type = "HC1")%>%diag()%>%sqrt()
summary(lm.2)

##3
lm.3<-lm(probWinpost~poly(winspre,2),data = merged.wins)
robust.lm3<- vcovHC(lm.3, type = "HC1")%>%diag()%>%sqrt()
summary(lm.3)

##4
lm.4<-lm(probWinpost~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm4<- vcovHC(lm.4, type = "HC1")%>%diag()%>%sqrt()
summary(lm.4)

##5
lm.5<-lm(probWinpost~winspre+idperiodpost,data = merged.wins)
robust.lm5<- vcovHC(lm.5, type = "HC1")%>%diag()%>%sqrt()
summary(lm.5)

##6
lm.6<-lm(probWinpost~winspre+I(winspre^2)+idperiodpost,data = merged.wins)
robust.lm6<- vcovHC(lm.6, type = "HC1")%>%diag()%>%sqrt()
summary(lm.6)

##IV Specs
##7
lm.7<-ivreg(probWinpost~(winspre>0)|(winspre_close>0),data=merged.wins)
robust.lm7<- vcovHC(lm.7, type = "HC1")%>%diag()%>%sqrt()
summary(lm.7)

##8
lm.8<-ivreg(probWinpost~winspre|(winspre_close),data=merged.wins)
robust.lm8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()
summary(lm.8)

##9
lm.9<-ivreg(probWinpost~winspre+I(winspre^2)|winspre_close+I(winspre_close^2),data=merged.wins)
robust.lm9<- vcovHC(lm.9, type = "HC1")%>%diag()%>%sqrt()
summary(lm.9)

##10
lm.10<-ivreg(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost,data=merged.wins)
robust.lm10<- vcovHC(lm.10, type = "HC1")%>%diag()%>%sqrt()
summary(lm.10)

##11
lm.11<-ivreg(probWinpost~(winspre)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robust.lm11<- vcovHC(lm.11, type = "HC1")%>%diag()%>%sqrt()
summary(lm.11)
coeftest(lm.11,vcov = vcovHC(lm.11, type = "HC1"))

##12
lm.12<-ivreg(probWinpost~idperiodpost+winspre+I(winspre^2)|winspre_close+I(winspre_close^2)+idperiodpost,data=merged.wins)
robust.lm12<- vcovHC(lm.12, type = "HC1")%>%diag()%>%sqrt()
summary(lm.12)

####################################
#Second Measure of Computation
####################################
start=0
split1=2
split2=2

# Second analysis, experience as annualized cumulative experience. 
## Second analysis of the Experience
merged.wins=createAnnualizedWins(df,start = start, split1 =split1,split2=split2,filterReqExp = T,thresholdClose = 0.005)

merged.wins.means.exp2=merged.wins%>%group_by(annualwinspre=round(annualwinspre))%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                   q0.25=quantile(probWinpost,probs = 0.25),
                                                                   q0.75=quantile(probWinpost,probs = 0.75),
                                                                   std.dev=sd(probWinpost),
                                                                   sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(annualwinspre>0,'Experience','No experience'))

merged.wins.close.price.exp2=merged.wins%>%filter((((winspre==winspre_close)&winspre_close>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.price.means.exp2=merged.wins.close.price.exp2%>%group_by(winspre_close=round(winspre_close))%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                                                q0.25=quantile(probWinpost,probs = 0.25),
                                                                                                q0.75=quantile(probWinpost,probs = 0.75),
                                                                                                std.dev=sd(probWinpost),
                                                                                                sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre_close>0,'Experience','No experience'))


### Describe this sample
slices=data.frame()
i=0
i=0
maxcutoff=ymd(max(df$FechaInicio))
cutoff2=ymd(min(df$FechaInicio))

while (cutoff2<=maxcutoff) {
  start=0
  split1=i+1
  split2=2
  
  #Do stuff
  cutoff0=ymd(min(df$FechaInicio)) + years(start)
  cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
  cutoff2=cutoff1 + years(split2)
  
  period1=paste0(cutoff0,"/",cutoff1)
  period2=paste0(cutoff1,"/",cutoff2)
  print(period1)

  df.period1=df%>%filter(FechaInicio<cutoff1&FechaInicio>=cutoff0)
  df.period2=df%>%filter(FechaInicio>=cutoff1&FechaInicio<=cutoff2)%>%filter(hasExp==0)  
  
  tot.contracts=length(unique(df.period1$Codigo))+length(unique(df.period2$Codigo))
  tot.contracts.p1=length(unique(df.period1$Codigo))
  tot.contracts.p2=length(unique(df.period2$Codigo))
  obs=length(unique(df.period2$RutProveedor))
  
  row=data.frame(slice=i,exp.comp=period1,outcome.comp=period2,obs=obs,
                 years.exp.comp=round(difftime(time1 = cutoff0,time2 = cutoff1,units = 'weeks')/52.25)%>%abs()%>%as.numeric(),
                 years.out.comp=round(difftime(time1 = cutoff1,time2 = cutoff2,units = 'weeks')/52.25)%>%abs()%>%as.numeric(),
                 tot.contracts.p1=tot.contracts.p1,tot.contracts.p2=tot.contracts.p2)  
  slices=rbind(row,slices) 
  i=i+1 
  
  
} 
slices=slices%>%arrange(slice)
colnames(slices)<-c('Slice','Period 1 dates','Period 2 dates','Observations','Length Period 1','Length Period 2','Contracts in Period 1','Contracts in Period 2')
table.slices.exp1=create_kable(slices,caption = 'Analysis dataset characteristics for experience computed as cumulative annualized ',label = 'slices_exp2')
table.slices.exp1%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_slices_exp2.txt")

##13
lm.13<-lm(probWinpost~(annualwinspre>0),data = merged.wins)
robust.lm13<- vcovHC(lm.13, type = "HC1")%>%diag()%>%sqrt()
summary(lm.13)

##14
lm.14<-lm(probWinpost~annualwinspre,data = merged.wins)
robust.lm.14<- vcovHC(lm.14, type = "HC1")%>%diag()%>%sqrt()
summary(lm.14)

##15
lm.15<-lm(probWinpost~poly(annualwinspre,2),data = merged.wins)
robust.lm15<- vcovHC(lm.15, type = "HC1")%>%diag()%>%sqrt()
summary(lm.15)

##16
lm.16<-lm(probWinpost~(annualwinspre>0)+idperiodpost,data = merged.wins)
robust.lm16<- vcovHC(lm.16, type = "HC1")%>%diag()%>%sqrt()
summary(lm.16)

##17
lm.17<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins)
robust.lm17<- vcovHC(lm.17, type = "HC1")%>%diag()%>%sqrt()
summary(lm.17)

##18
lm.18<-lm(probWinpost~(annualwinspre)+I(annualwinspre^2)+idperiodpost,data = merged.wins)
robust.lm18<- vcovHC(lm.18, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18)

#IVs
##19
lm.19<-ivreg(probWinpost~(annualwinspre>0)|(annualwinspre_close>0),data=merged.wins)
robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18)

##20
lm.20<-ivreg(probWinpost~annualwinspre|(annualwinspre_close),data=merged.wins)
robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
summary(lm.20)

##21
lm.21<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)|annualwinspre_close+I(annualwinspre_close^2),data=merged.wins)
robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
summary(lm.21)

##22
lm.22<-ivreg(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost,data=merged.wins)
robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
summary(lm.22)

##23
lm.23<-ivreg(probWinpost~(annualwinspre)+idperiodpost|(annualwinspre_close+idperiodpost),data=merged.wins)
robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
summary(lm.23)

##24
lm.24<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)+idperiodpost|annualwinspre+I(annualwinspre_close^2)+idperiodpost,data=merged.wins)
robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
summary(lm.24)

#############################
## Robustness checks
############################

###################
## Robustness Period of outcomes
###################

start=0
split1=2
split2=2
splits=c(1,2,3)
res.ols=splits%>%map_dfr(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                          lm(probWinpost~(winspre>0)+idperiodpost,data = .) %>%
                       coeftest(vcov = vcovHC(., type = "HC1")) %>%
                       tidy() %>% filter(term == 'winspre > 0TRUE')%>%mutate(outcome.per=x, type='OLS',exp='Rolling') )

res.iv.price=splits%>%map_dfr(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                          ivreg(data= .,formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost)) %>%
                           coeftest(vcov = vcovHC(., type = "HC1")) %>%
                           tidy() %>% filter(term == 'winspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.ranks=splits%>%map_dfr(function(x) createMultiPeriodDataset(df.ranked,start = start, split1 =split1,split2=x,ranks = T,filterReqExp = T,thresholdCloseRank = 1.03)%>%
                                ivreg(data= .,formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_closerank>0)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'winspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Ranks',exp='Rolling')) 

res.ols2=splits%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                           lm(probWinpost~(annualwinspre>0)+idperiodpost,data = .) %>%
                           coeftest(vcov = vcovHC(., type = "HC1")) %>%
                           tidy() %>% filter(term == 'annualwinspre > 0TRUE')%>%mutate(outcome.per=x, type='OLS',exp='Annualized') )

res.iv.price2=splits%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                                ivreg(data= .,formula=(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'annualwinspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 

res.iv.ranks2=splits%>%map_dfr(function(x) createAnnualizedWins(df.ranked,start = start, split1 =split1,split2=x,ranks = T,filterReqExp = T,thresholdCloseRank = 1.03)%>%
                                ivreg(data= .,formula=(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_closerank>0)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'annualwinspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Ranks',exp='Annualized')) 
table_robustness_period.binary=rbind(res.ols,res.iv.price,res.iv.ranks,res.ols2,res.iv.price2,res.iv.ranks2)%>%mutate('ff'='Indicator')

## Linear Experience
res.ols=splits%>%map_dfr(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                           lm(probWinpost~winspre+idperiodpost,data = .) %>%
                           coeftest(vcov = vcovHC(., type = "HC1")) %>%
                           tidy() %>% filter(term == 'winspre')%>%mutate(outcome.per=x, type='OLS',exp='Rolling') )

res.iv.price=splits%>%map_dfr(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                                ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|(winspre_close)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'winspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.ranks=splits%>%map_dfr(function(x) createMultiPeriodDataset(df.ranked,start = start, split1 =split1,split2=x,ranks = T,filterReqExp = T,thresholdCloseRank = 1.03)%>%
                                ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|(winspre_closerank)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'winspre')%>%mutate(outcome.per=x, type='IV-Ranks',exp='Rolling')) 

res.ols2=splits%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                            lm(probWinpost~annualwinspre+idperiodpost,data = .) %>%
                            coeftest(vcov = vcovHC(., type = "HC1")) %>%
                            tidy() %>% filter(term == 'annualwinspre')%>%mutate(outcome.per=x, type='OLS',exp='Annualized') )

res.iv.price2=splits%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=x,filterReqExp = T)%>%
                                 ivreg(data= .,formula=(probWinpost~annualwinspre+idperiodpost|(annualwinspre_close)+idperiodpost)) %>%
                                 coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                 tidy() %>% filter(term == 'annualwinspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 

res.iv.ranks2=splits%>%map_dfr(function(x) createAnnualizedWins(df.ranked,start = start, split1 =split1,split2=x,ranks = T,filterReqExp = T,thresholdCloseRank = 1.03)%>%
                                 ivreg(data= .,formula=(probWinpost~annualwinspre+idperiodpost|(annualwinspre_closerank)+idperiodpost)) %>%
                                 coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                 tidy() %>% filter(term == 'annualwinspre')%>%mutate(outcome.per=x, type='IV-Ranks',exp='Annualized')) 

table_robustness_period.linear=rbind(res.ols,res.iv.price,res.iv.ranks,res.ols2,res.iv.price2,res.iv.ranks2)%>%mutate('ff'='Linear')
table_robustness_period=rbind(table_robustness_period.binary,table_robustness_period.linear)

save(table_robustness_period,file = 'C:\\repos\\learn-doing\\data\\table_robustness_period.rds')
load(file='C:\\repos\\learn-doing\\data\\table_robustness_period.rds')

table_robustness_period_o=table_robustness_period%>%mutate(estimate=round(estimate,3),std.error=round(std.error,3))%>%
  mutate(condensed.output=paste0(estimate,' (',std.error,') ',ifelse(p.value<0.01,yes='***',no='')))%>%select(exp,ff,type,outcome.per,condensed.output)%>%
  arrange(exp,outcome.per)%>%pivot_wider(  id_cols=exp:type,names_from = outcome.per,values_from = condensed.output)%>%arrange(exp,ff,type)%>%filter(exp=='Rolling')%>%select(-exp)

colnames(table_robustness_period_o)<-c('Experience Computation','Specification','1 year outcomes','2 year outcomes (Main)','3 year outcomes')

table_robustness_period_o=create_kable(table_robustness_period_o,caption = 'Robustness analysis for the coefficient on Experience (Rolling) by length of outcome computation period',label = 'robust_bin_outcomes')
table_robustness_period_o%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\robust_bin_outcomes.txt")


###################
## Robustness for IV - Price Close Wins by price
###################
# Robustness checks: close wins
close_wins_vector=c(thresholdClose=seq(0.001,0.03,by = 0.001))
##Revisar como puede haber diferencia de cero
start=0
split1=2
split2=2
robustness_close_wins=close_wins_vector%>%map_dfr(function(x) (createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x,filterReqExp = T )%>%
                                                                    ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(thresholdClose=x)))%>%mutate(model='Linear Experience')


robustness_close_wins=robustness_close_wins%>%mutate(lower95=estimate-2*std.error,upper95=estimate+2*std.error)

##Study the different weight thresholds

thresholds=c(0.5,0.6,0.65,0.7,0.8)*100
res.iv.price=thresholds%>%map_dfr(function(x)  createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,filterReqExp = T,weightPrice=x,thresholdClose = 0.005)%>%
                                ivreg(data= .,formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'winspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.price2=thresholds%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=split2,filterReqExp = T, weightPrice=x,thresholdClose = 0.005)%>%
                                ivreg(data= .,formula=(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'annualwinspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 
table_robustness_weightpricebin=rbind(res.iv.price,res.iv.price2)%>%mutate(term='Binary Indicator')


res.iv.price=thresholds%>%map_dfr(function(x)  createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,filterReqExp = T,weightPrice=x,thresholdClose = 0.005)%>%
                                ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|(winspre_close)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'winspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.price2=thresholds%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=split2,filterReqExp = T, weightPrice=x,thresholdClose = 0.005)%>%
                                ivreg(data= .,formula=(probWinpost~(annualwinspre)+idperiodpost|(annualwinspre_close)+idperiodpost)) %>%
                                coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                tidy() %>% filter(term == 'annualwinspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 

table_robustness_weightpricelinear=rbind(res.iv.price,res.iv.price2)%>%mutate(term='Linear')
table_robustness_weightprice=rbind(table_robustness_weightpricebin,table_robustness_weightpricelinear)

table_robustness_weightprice_o=table_robustness_weightprice%>%mutate(estimate=round(estimate,3),std.error=round(std.error,3))%>%filter(outcome.per!=65)%>%
  mutate(condensed.output=paste0(estimate,' (',std.error,') ',ifelse(p.value<0.01,yes='***',no=ifelse(p.value<0.05,yes='**',no=""))))%>%select(exp,term,outcome.per,condensed.output)%>%
  arrange(exp,term,outcome.per)%>%pivot_wider(  id_cols=exp:term,names_from = outcome.per,values_from = condensed.output)


colnames(table_robustness_weightprice_o)[1:2]<-c('Experience Computation','Functional Form')
table_robustness_weightprice_o=create_kable(table_robustness_weightprice_o,caption = 'Robustness analysis for the price weight parameter in the IV Regression by price',label = 'robust_weightprice_outcomes')
table_robustness_weightprice_o%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\robust_weightprice_outcomes.txt")



















#Output
results.ols.linear.output=results.ols.linear%>%select(tramo,model,n, estimate,significance,p.value)%>%mutate(across(where(is.numeric), function(x) round(x,3)))
colnames(results.ols.linear.output)<-c('Sales group','Model','Observations','Experience Estimate','Significant at 0.05','p-value')
results.ols.linear.output%>%kable(format = 'latex',booktabs=T,table.envir = 'table')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_sizes_explinear1.txt")



tramos%>%map_dfr(function(x) lm(data= merged.wins.juridicas%>%filter(tramo.group==x&tramo>1),formula=(probWinpost~winspre+idperiodpost))%>%tidy()%>%
  filter(term=='winspre')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x&tramo>1))))%>%mutate(model='Linear Experience')%>%arrange(tramo)
merged.wins.juridicas$numtrab

merged.wins%>%ivreg(data=.,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))
directory.simplifed$tramo
merged.wins2[1:100,c('RutProveedor','RutProveedor_clean','tramo')]%>%as.data.frame()
merged.wins2%>%group_by(is.na(tramo))%>%summarise(count=length(tramo),avmonto=mean(montoTotalpost),medmonto=median(montoTotalpost),totmonto=sum(montoTotalpost))%>%ungroup()%>%mutate(totmonto=totmonto/(sum(totmonto)))


summary(ivreg(data=merged.wins,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost)))
(function(x) (lm(probWinpost~winspre+idperiodpost,data=x))%>%map(function(y) (tidy(y)%>%dplyr::select(term,p.value)%>%filter(term=='winspre')%>%mutate(thresholdClose=x)))))
1uk=close_wins_vector%>%map(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x ))%>%map(function(x) lm(winspre+idperiodpost,data=x))
ak=uk%>%map(function(x) lm(probWinpost~winspre+idperiodpost,data=x))%>%map(function(y) (tidy(y)%>%dplyr::select(term,p.value)%>%filter(term=='winspre')%>%mutate(thresholdClose=x)))

library(broom)
tyest1=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = param )
merged.wins%>%lm(formula=probWinpost~winspre+idperiodpost)


table(merged.wins.2$idperiodpost)




##Second specification. Outcome is p2 winning probability. OLS. With F.E controls
lm.spec2.cont<-lm(probWinpost~log(winspre+1)+idperiodpost,data = merged.wins)
robust.spec2.cont<- vcovHC(lm.spec2.cont, type = "HC1")%>%diag()%>%sqrt()

lm.spec2.bin<-lm(probWinpost~(winspre>0)+idperiodpost,data = merged.wins)
robust.spec2.bin<- vcovHC(lm.spec2.bin, type = "HC1")%>%diag()%>%sqrt()

##Third specification Firm FE by type

#Instrumental Variables
#First specification
iv.spec1.cont<-ivreg(probWinpost~logWinpre|(winspre_close),data=merged.wins)
robustiv.spec1.cont<- vcovHC(iv.spec1.cont, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec1.cont)

iv.spec1.bin<-ivreg(probWinpost~(winspre>0)|(winspre_close),data=merged.wins)
robustiv.spec1.bin<- vcovHC(iv.spec1.bin, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec1.bin)

#Second specification
iv.spec2.cont<-ivreg(probWinpost~logWinpre|(winspre_close)+idperiodpost,data=merged.wins)
robustiv.spec2.cont<- vcovHC(iv.spec2.cont, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec2.cont)

iv.spec2.bin<-ivreg(probWinpost~(winspre>0)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robustiv.spec2.bin<- vcovHC(iv.spec1.bin, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec2.bin)

#Create Simple Table of Outcomes
stargazer(lm.spec1.bin,lm.spec1.cont,lm.spec1.cont,robust.spec2.cont,iv.spec1.cont,iv.spec2.cont, type = "latex",
          se = list(NULL, c(robust.spec1.bin,robust.spec1.cont,robust.spec2.cont,robustiv.spec1.cont,robustiv.spec2.cont)),omit.stat = c( "f","adj.rsq"),title="Regression for OLS and IV specifications")










?stargazer



#Specific Analysis Data Creation

prelim.names=c('Period 1','Period 2')
cutoff0=ymd(min(df$FechaInicio)) + years(start)
cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
cutoff2=cutoff1 + years(split2)
df.period1=df%>%filter(FechaInicio<=cutoff1&FechaInicio>=cutoff0)
df.period2=df%>%filter(FechaInicio>cutoff1&FechaInicio<=cutoff2)

#create winning statistics of each period
##Create Winning Statistics for first period
df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])
df.period1.wins=df.period1%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)
df.period2.wins=df.period2%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)

##Calculate narrow victories
df.period1.difs=df.period1%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
df.period2.difs=df.period2%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
#df.period1.difs=df.period1%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(RutProveedor=RutProveedor[winner=='Seleccionada'],NumeroOferentes=max(NumeroOferentes))
df.period1.difs.close=(df.period1.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
df.period2.difs.close=(df.period2.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
nrow(df.period1.difs.close)/nrow(df.period1.wins)
df.period1.wins.close=df.period1.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
df.period2.wins.close=df.period2.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)

prelim.n=c((length(unique(df.period1$Codigo))),(length(unique(df.period2$Codigo))))
prelim.years=c(split1,split2)
prelim.bids=c(nrow(df.period1),nrow(df.period2))
prelim.closewins=c(sum(df.period1.wins.close$winspre_close),sum(df.period2.wins.close$winspre_close))
prelim.firms=c((length(unique(df.period1$RutProveedor))),(length(unique(df.period2$RutProveedor))))
prelim.df=data.frame(Type=prelim.names,Span=prelim.years,N=prelim.n,'Total Bids'=prelim.bids,'Close Wins'=prelim.closewins,Prob.Close.wins=0,Firms=prelim.firms)%>%mutate(Prob.Close.wins=round(Close.Wins/N,3))
prelim.df

##Why do I need cutoff?

#II. Exploratory Analysis
##What is the simple graph look like
#merged.wins=df.period1.wins%>%left_join(df.period2.wins,suffix = c('pre','post'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
   #         mutate(winsprebin=as.numeric(winspre>0))%>%left_join(df.period1.wins.close,by = 'RutProveedor')%>%mutate(winspre_close= replace_na(winspre_close,0))

merged.wins.means=merged.wins%>%group_by(winspre)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                              q0.25=quantile(probWinpost,probs = 0.25),q0.75=quantile(probWinpost,probs = 0.75),std=sd(probWinpost) )%>%
                                                    filter(n>10)%>%mutate(exp=ifelse(winspre>0,'Experience','No experience'))

merged.wins.close=merged.wins%>%filter((((winspre==winspre_close)&winspre_close>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.means=merged.wins.close%>%group_by(winspre)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                         q0.25=quantile(probWinpost,probs = 0.25),q0.75=quantile(probWinpost,probs = 0.75) )%>%
                                        filter(n>10)%>%mutate(exp=ifelse(winspre>0,'Experience','No experience'))




plotDisc.closewins<-ggplot(merged.wins.close.means,aes(x=(winspre),y=probWinpost_mean))+geom_point(size=3,color='red')+xlim(-0.1,2.1)+
    geom_errorbar(aes(x=winspre,ymin=q0.25, ymax=q0.75,width=0.1))+
    #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
    theme_bw()+xlab('Number of (close) contracts won in (t-1)')+ylab('Win probability on t')+
    theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))
plotDisc.closewins
  
plotDisc.2<-ggplot(merged.wins.close,aes(x=(winspre),y=probWinpost))+
    stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
    stat_summary(geom = "point", fun.y = mean,color='red',size=5)+
    theme_bw()+xlab('Number of Close Contracts won in (t-1)')+ylab('Win probability on t')+
    theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))+ 
    coord_cartesian(ylim=c(0, 0.35))+ scale_y_continuous(breaks = seq(0, 0.35, by = 0.05))
plotDisc.2

bplotDisc<-ggplot(merged.wins,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=3)+xlim(0,15)+
  geom_smooth(data=merged.wins%>%filter(winspre>0),se=F,formula=y ~ log(x), method = 'lm',color='steelblue')+
  geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Number of Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))
plotDisc
merged.wins$win

plotDisc.2<-ggplot(merged.wins.close,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=5)+
  theme_bw()+xlab('Number of Close Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))+ 
  coord_cartesian(ylim=c(0, 0.35))+ scale_y_continuous(breaks = seq(0, 0.35, by = 0.05))
plotDisc.2

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plowins_all.png",width = 6, height = 4.5,
    units = "in",res=1000)
plotDisc.allwins
dev.off()

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plowins_close.png",width = 6, height = 4.5,
    units = "in",res=1000)
plotDisc.closewins
dev.off()

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotwins_both.png",width = 9, height = 4.5,units = "in",res=1000)
cowplot::plot_grid(plotDisc.allwins,plotDisc.closewins,labels = 'AUTO')
dev.off()
unique(merged.wins$idperiodpost)

#III. Linear Regression Models
                            qa      =merged.wins%>%mutate(logWinpre=log(winspre+1),logWinpre.close=log(winspre_close+1))
##Binary Linear Model
lm1<-lm(probWinpost~winsprebin,data=merged.wins)
summary(lm1)

cov1<- vcovHC(lm1, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

##Continuous Linear Model
lm2<-lm(probWinpost~logWinpre,data=merged.wins)
summary(lm2)

cov2<- vcovHC(lm2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

## Instrumental Variables Regression
ivreg1<-ivreg(probWinpost~logWinpre|(winspre_close),data=merged.wins)
cov3<- vcovHC(ivreg1, type = "HC1")
robust_se3 <- sqrt(diag(cov3))

stargazer(lm1,lm2,ivreg1, type = "latex",
          se = list(NULL, c(robust_se1,robust_se2,robust_se3)),omit.stat = c( "f","adj.rsq"),title="Regression for OLS and IV specifications")

#Sample statistics de la muestra analizada
#stargazer output de las tres especificaciones
#Fin

###Re1quired Outputs
create_kable<-function(df,caption){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', 5),
    caption = caption
  ) %>% kable_styling(latex_options = c("hold_position"))
}
create_kable(final.statistics,"Sample Descriptive Statistics")
create_kable(prelim.df,"Sample Descriptive Statistics")

nrow(merged.wins)
  

# merged.wins$idperiodpost
# est_binary=(summary(lm.4)$coefficients['winspre > 0TRUE', 2])
# binary_low=confint(lm.4,)['winspre > 0TRUE',1]
# binary_high=confint(lm.4,)['winspre > 0TRUE',2]
# 
# se_linear=(summary(lm.5)$coefficients['winspre', 2])
# linear_low=confint(lm.5,)['winspre',1]
# linear_high=confint(lm.5,)['winspre',2]
# 
# se_quadratic.1=(summary(lm.6)$coefficients['winspre', 2])
# se_quadratic.2=(summary(lm.6)$coefficients['I(winspre^2', 2])
# quadratic.1_low=confint(lm.5)['winspre',1]
# quadratic_high=effects_binary+se_binary*2
# 
# confidence_binary=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')
# confidence_linear=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')
# confidece_qadratic=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')


#Create Simple Table of Outcomes
stargazer(lm.1,lm.2,lm.3,lm.4,lm.5,lm.6, type = "latex",
          se = list(NULL, c(robust.lm1,robust.lm2,robust.lm3,robust.lm4,robust.lm5,robust.lm6)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
          title="Regression for OLS and IV specifications",
          dep.var.labels=c("Share of Contracts won in t"),
          covariate.labels=c("Experience in (t-1)",'Experience in (t-1)','(Experience in (t-1))^2'),
          add.lines = list(c("Fixed effects", "No", "No",'No','Yes','Yes','Yes')))

regression.output.1=capture.output({stargazer(lm.4,   lm.5,lm.6,lm.10,lm.11,lm.12, type = "latex",label = 'tab:table_exp_1', header = F,
                                              se = list(NULL, c(robust.lm4,robust.lm5,robust.lm6,robust.lm10,robust.lm11,robust.lm12)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)','(Experience in (t-1)) (Squared)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.1,'table_ols_exp1.txt')


#Chosen Output II
regression.output.2=capture.output({stargazer(lm.16,lm.17,lm.18,lm.22,lm.23,lm.24, type = "latex",label = 'tab:tableExp2',header = F,
                                              se = list(NULL, c(robust.lm16,robust.lm17,robust.lm18,robust.lm22,robust.lm23,robust.lm24)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)','(Experience in (t-1)) (Squared)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.2,'table_ols_exp2.txt')

# #Period Durations: Outcomes are one and three years
# start=0
# split1=2
# split2=2
# merged.wins.original=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2)
# merged.wins.original.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# split2=1
# merged.wins.1=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
# merged.wins.1.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# split2=3
# merged.wins.3=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
# merged.wins.3.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# 
# lm.19<-lm(probWinpost~(winspre)+idperiodpost,data = merged.wins.original)
# robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
# lm.20<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.1)
# robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
# lm.21<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.3)
# robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
# 
# lm.22<-lm(probWinpost~(annualwinspre)+idperiodpost,data = merged.wins.original.annualized)
# robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
# lm.23<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.1.annualized)
# robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
# lm.24<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.3.annualized)
# robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
# 
# robustness.outcomes=stargazer(lm.19,lm.20,lm.21,lm.22,lm.23,lm.24, type = "latex",
#           se = list(NULL, c(robust.lm19,robust.lm20,robust.lm21,robust.lm22,robust.lm23,robust.lm24)),
#           dep.var.caption ="Contracts Won/Contracts Bid in Outcome Period",
#           dep.var.labels   = "Outcome period of length (years):",
#           column.labels = c("1", "2 (Original)","3","1", "2 (Original)","3"),
#           omit='idperiodpost',
#           model.numbers = FALSE,
#           covariate.labels=c("Experience",'Annualized Cumulative Experience'),
#           omit.stat = c( "f","adj.rsq"),title="Robustness checks for duration of outcomes period of interest")
# robustness.outcomes%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_robustness_periodoutcomes_ols.txt")