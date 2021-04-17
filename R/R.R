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
post.summarybids=generateDfBidsSummary(bids = df)

#Create Firm datasets
df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])
df.wins=df%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)

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
start=0
split1=2
split2=2
merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
  mutate(logWinpre=log(winspre+1))
skim(merged.wins)

#Begin Regression Analysis with outcome variable as probability
head(merged.wins)

##Nonparametric
lm.nonp.bin<-lm(probWinpost~(winspre>0),data = merged.wins)
robust.nonp.bin<- vcovHC(lm.nonp.bin, type = "HC1")%>%diag()%>%sqrt()
summary(lm.nonp.bin)

##First specification
robust.spec1.cont<- vcovHC(lm.spec1.cont, type = "HC1")%>%diag()%>%sqrt()
lm.spec1.cont<-lm(probWinpost~log(winspre+1),data = merged.wins)

lm.spec1.bin<-lm(probWinpost~(winspre>0),data = merged.wins)
robust.spec1.bin<- vcovHC(lm.spec1.bin, type = "HC1")%>%diag()%>%sqrt()

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
  

