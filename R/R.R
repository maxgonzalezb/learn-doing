library(feather)
library(magrittr)
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


source('C:\\repos\\learn-doing\\R\\functions.R')

thresholdClose=0.005
thresholdSize=20e6

df = read_feather('C:\\repos\\learn-doing\\data\\lic_construccion_feather.feather') %>%
                  rename(estadoOferta=`Estado Oferta`,montoOferta = `Valor Total Ofertado`, winner = `Oferta seleccionada`,cantidadOferta=`Cantidad Ofertada`,tipoAdquisicion=`Tipo de Adquisición`)
glimpse(df)
skim(df)

#Accepted Offers? Be careful computing statistics, should not eliminate straight away. TODO:desert lics. 

#Cleaning
#df <- read.csv("C:/repos/public-procurement/bids.csv", encoding="CP1252")
df=df%>%mutate(FechaInicio=as.Date(FechaInicio))%>%mutate(year=year(FechaInicio),MCA_MPO=montoOferta/MontoEstimado)
df=df%>%mutate(year=as.numeric(year))%>%
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


#Filtering
df=df%>%filter(cantidadOferta==1&CantidadAdjudicada<=1,montoOferta>=1e6)
df=df%>%filter(MontoEstimado>thresholdSize)
df.repetidos=df%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Nombre,NombreOrganismo,montoOferta,NombreProveedor)%>%mutate(n=length(NombreProveedor))%>%filter(n>1)
df=df%>%filter(!Codigo%in%(df.repetidos$Codigo))
save(df,file = 'C:\\repos\\learn-doing\\data\\contractData.Rdata')
post.summarybids=generateDfBidsSummary(bids = df)

#Create Firm datasets
df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])
df.wins=df%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)
df.names=df%>%dplyr::select(RutProveedor,NombreProveedor)%>%group_by(RutProveedor,NombreProveedor)%>%slice(1)

#Study how are the close wins
df.difs.ind=df.difs%>%mutate(isClose=ifelse(dif<0.005,yes='Close Win','No Close Win'))
df=df%>%left_join(df.difs.ind)
comparison.1=df%>%filter(isClose=='No Close Win')%>%generateDfBidsSummary()%>%dplyr::select('mean','std')%>%rename('mean_notClose'='mean','std_notClose'='std')
comparison.2=df%>%filter(isClose=='Close Win')%>%generateDfBidsSummary()%>%dplyr::select(name,'mean','std')%>%rename('mean_close'='mean','std_close'='std')%>%cbind(comparison.1)%>%select(name,mean_notClose,mean_close,std_notClose,std_close)
colnames(comparison.2)<-c('Variable','Mean (Not close win)','Mean (Close win)','Sd (Not close win)','Sd (Close win)')
create_kable(comparison.2,caption = "Comparison between close and non-close wins")

#General Statistics





contractors.statistics=df%>%group_by(RutProveedor,NombreProveedor)%>%summarise(firstYear=min(year),
                                                                               lastYear=max(year),
                                                                               life=lastYear-firstYear,
                                                                               firstYearAw=min(year[CantidadAdjudicada>=1]),
                                                                               uniqueOrganism=length(unique(NombreOrganismo)),
                                                                               uniqueUnits=length(unique(NombreUnidad)),
                                                                               tot=length(Codigo),
                                                                               totAdj=length(Codigo[CantidadAdjudicada>=1]),
                                                                               montoTot=sum(montoOferta[CantidadAdjudicada>=1]))%>%arrange(-montoTot)%>%ungroup()%>%
mutate(acum=cumsum(montoTot)/sum(montoTot))%>%mutate(Pareto=ifelse(acum<=0.9,1,0))

#Create Datasets

#First Analysis of Experience
start=0
split1=2
split2=2
merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
mutate(logWinpre=log(winspre+1))



head(merged.wins)

#Begin Regression Analysis with outcome variable as probability
head(merged.wins)

##OLS Specs
##1
lm.1<-lm(probWinpost~(winspre>0),data = merged.wins)
robust.lm.1<- vcovHC(lm.1, type = "HC1")%>%diag()%>%sqrt()
summary(lm1)

##2
lm.2<-lm(probWinpost~winspre,data = merged.wins)
robust.lm2<- vcovHC(lm.2, type = "HC1")%>%diag()%>%sqrt()
summary(lm.2)

##3
lm.3<-lm(probWinpost~poly(winspre,2),data = merged.wins)
robust.lm3<- vcovHC(lm.3, type = "HC1")%>%diag()%>%sqrt()
summary(lm.3)

##4
lm.4<-lm(probWinpost~(winspre>0)+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm4<- vcovHC(lm.4, type = "HC1")%>%diag()%>%sqrt()

##5
lm.5<-lm(probWinpost~winspre+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm5<- vcovHC(lm.5, type = "HC1")%>%diag()%>%sqrt()
summary(lm.5)

##6
lm.6<-lm(probWinpost~poly(winspre,2)+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm6<- vcovHC(lm.6, type = "HC1")%>%diag()%>%sqrt()
summary(lm.6)

#Create Simple Table of Outcomes
stargazer(lm.1,lm.2,lm.3,lm.4,lm.5,lm.6, type = "latex",
          se = list(NULL, c(robust.lm1,robust.lm2,robust.lm3,robust.lm4,robust.lm5,robust.lm6)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
          title="Regression for OLS and IV specifications",
          dep.var.labels=c("Share of Contracts won in t"),
          covariate.labels=c("Experience in (t-1)",'Experience in (t-1)','(Experience in (t-1))^2'),
          add.lines = list(c("Fixed effects", "No", "No",'No','Yes','Yes','Yes')))

##IV Specs
##7
lm.7<-ivreg(probWinpost~(winspre>0)|(winspre_close),data=merged.wins)
robust.lm.7<- vcovHC(lm.7, type = "HC1")%>%diag()%>%sqrt()
summary(lm.7)

##8
lm.8<-ivreg(probWinpost~winspre|(winspre_close),data=merged.wins)
robust.lm.8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()
summary(lm.8)

##9
lm.9<-ivreg(probWinpost~poly(winspre,2)|poly(winspre_close,2),data=merged.wins)
robust.lm.9<- vcovHC(lm.9, type = "HC1")%>%diag()%>%sqrt()
summary(lm.9)

##10
lm.10<-ivreg(probWinpost~(winspre>0)+idperiodpost|(winspre_close)+idperiodpost,data=merged.wins)
robust.lm.10<- vcovHC(lm.10, type = "HC1")%>%diag()%>%sqrt()
summary(lm.10)

##11
lm.11<-ivreg(probWinpost~(winspre)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robust.lm.11<- vcovHC(lm.11, type = "HC1")%>%diag()%>%sqrt()
summary(lm.11)

##12
lm.12<-ivreg(probWinpost~poly(winspre,2)+idperiodpost|poly(winspre_close,2)+idperiodpost,data=merged.wins)
robust.lm.12<- vcovHC(lm.12, type = "HC1")%>%diag()%>%sqrt()
summary(lm.12)

stargazer(lm.7,lm.8,lm.9,lm.10,lm.11,lm.12, type = "latex",
          se = list(NULL, c(robust.lm.1,robust.lm.2,robust.lm.3,robust.lm.5,robust.lm.5,robust.lm.6)),omit.stat = c( "f","adj.rsq"),title="Regression for OLS and IV specifications")


##Second analysis, experience as annual experience. 
##Second analysis of the Experience
merged.wins=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
table(merged.wins$idperiodpost)
hist(merged.wins$annualwins)

##1
lm.13<-lm(probWinpost~(annualwinspre>0),data = merged.wins)
robust.lm13<- vcovHC(lm.13, type = "HC1")%>%diag()%>%sqrt()
summary(lm.13)

##2
lm.14<-lm(probWinpost~annualwinspre,data = merged.wins)
robust.lm.14<- vcovHC(lm.14, type = "HC1")%>%diag()%>%sqrt()
summary(lm.14)

##3
lm.15<-lm(probWinpost~poly(annualwinspre,2),data = merged.wins)
robust.lm15<- vcovHC(lm.15, type = "HC1")%>%diag()%>%sqrt()
summary(lm.15)

##4
lm.16<-lm(probWinpost~(annualwinspre>0)+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm16<- vcovHC(lm.16, type = "HC1")%>%diag()%>%sqrt()

##5
lm.17<-lm(probWinpost~annualwinspre+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm17<- vcovHC(lm.17, type = "HC1")%>%diag()%>%sqrt()
summary(lm.17)

##6
lm.18<-lm(probWinpost~poly(annualwinspre,2)+idperiodpost+paretoIndpost,data = merged.wins)
robust.lm18<- vcovHC(lm.18, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18)

## Robustness checks
#Period Durations: Outcomes are one and three years
start=0
split1=2
split2=2
merged.wins.original=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2)
merged.wins.original.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
split2=1
merged.wins.1=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
merged.wins.1.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
split2=3
merged.wins.3=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
merged.wins.3.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)

lm.19<-lm(probWinpost~(winspre)+idperiodpost,data = merged.wins.original)
robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
lm.20<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.1)
robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
lm.21<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.2)
robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()

lm.22<-lm(probWinpost~(annualwinspre)+idperiodpost,data = merged.wins.original.annualized)
robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
lm.23<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.1.annualized)
robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
lm.24<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.3.annualized)
robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()

stargazer(lm.19,lm.20,lm.21,lm.22,lm.23,lm.24, type = "latex",
          se = list(NULL, c(robust.lm19,robust.lm20,robust.lm21,robust.lm22,robust.lm23,robust.lm24)),
          dep.var.caption ="Contracts Won/Contracts Bid in Outcome Period",
          dep.var.labels   = "Outcome period of length (years):",
          column.labels = c("1", "2 (Original)","3","1", "2 (Original)","3"),
          omit='idperiodpost',
          model.numbers = FALSE,
          covariate.labels=c("Experience",'Annualized Cumulative Experience'),
          omit.stat = c( "f","adj.rsq"),title="Robustness checks for duration of outcomes period of interest")

library(purrr)
## Robustness checks: close wins
close_wins_vector=c(thresholdClose=seq(0.001,0.03,by = 0.001))
##Revisar como puede haber diferencia de cero
start=0
split1=2
split2=2
robustness_close_wins=close_wins_vector%>%map_dfr(function(x) (createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x )%>%
                                                                    ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(thresholdClose=x)))%>%mutate(model='Linear Experience')


robustness_close_wins=robustness_close_wins%>%mutate(lower95=estimate-2*std.error,upper95=estimate+2*std.error)
p1<-ggplot(robustness_close_wins,aes(x=thresholdClose,y=estimate))+geom_line(color='darkblue',lwd=1.5)+geom_vline(xintercept = 0.005,color='red')+#geom_point(color='darkblue')+
  geom_line(aes(y=lower95),color='darkgrey',alpha=0.9,linetype=2,lwd=1)+geom_line(aes(y=upper95),color='darkgrey',alpha=0.9,linetype=2,lwd=1)+ylim(0.01,0.03)+theme_bw()+xlab('Threshold for a close win (Percentage)')+ylab('Experience Estimate')

png(filename="C:\\repos\\learn-doing\\R\\Output\\robustness_threshold.png",width = 4, height = 3,
    units = "in",res=1000)
p1
dev.off()

?ivreg
?geom_line


merged.wins%>%ivreg(data=.,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))


summary(ivreg(data=merged.wins,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost)))
                                                                                                                                           %>%(function(x) (lm(probWinpost~winspre+idperiodpost,data=x))%>%map(function(y) (tidy(y)%>%dplyr::select(term,p.value)%>%filter(term=='winspre')%>%mutate(thresholdClose=x)))))
uk=close_wins_vector%>%map(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x ))%>%map(function(x) lm(winspre+idperiodpost,data=x))
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
merged.wins=df.period1.wins%>%left_join(df.period2.wins,suffix = c('pre','post'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
            mutate(winsprebin=as.numeric(winspre>0))%>%left_join(df.period1.wins.close,by = 'RutProveedor')%>%mutate(winspre_close= replace_na(winspre_close,0))

merged.wins.means=merged.wins%>%group_by(winspre)%>%summarise(probWinpost=mean(probWinpost),n=length(RutProveedor))

merged.wins.close=merged.wins%>%filter((winspre==1&winspre_close==1)|(winspre==0&ofertaspre>0))

plotDisc.old<-ggplot(merged.wins.means,aes(x=(winspre),y=probWinpost))+geom_point()+xlim(0,20)+geom_smooth(data=, se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+ylim(0,0.5)

plotDisc<-ggplot(merged.wins,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=3)+xlim(0,15)+
  geom_smooth(data=merged.wins%>%filter(winspre>0),se=F,formula=y ~ log(x), method = 'lm',color='steelblue')+
  geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Number of Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))
plotDisc
merged.wins$win

plotDisc.2<-ggplot(merged.wins.close,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=5)+
  theme_bw()+xlab('Number of Close Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))+ 
  coord_cartesian(ylim=c(0, 0.35))+ scale_y_continuous(breaks = seq(0, 0.35, by = 0.05))
plotDisc.2


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
  

