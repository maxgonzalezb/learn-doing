
#################################
## First Investigation: Bids
#################################
df.bids = df %>% mutate(WinContr = as.numeric(winner == 'Seleccionada')) %>%
  group_by(RutProveedor) %>% arrange(FechaInicio) %>% mutate(
    firstyear = min(year),
    offers=cumsum(as.numeric(WinContr>=0)),
    exp = max(cumsum(WinContr) - 1, 0),
    isCloseWin_rank=WinContr * isCloseRanking,
    isCloseWin = WinContr * isClose)%>%
  mutate(
    exp_close =
      max(cumsum(isCloseWin) - 1, 0),
    exp_closerank =
      max(cumsum(isCloseWin_rank) - 1, 0)
  ) %>%
  mutate(exp_close = ifelse(is.na(exp_close), yes = 0, no = exp_close)) %>%
  mutate(indExp = as.numeric(exp > 0),
         indExpClose = as.numeric(exp_close > 0)) %>%
  mutate(
    annualexp = exp / (year - firstyear + 1),
    annualexp_close = exp_close / (year - firstyear + 1),
    life = (year - firstyear + 1)
  )%>%
  mutate(indFirstYear=(year==firstyear))

df.bids=df.bids%>%filter(MCA_MPO<=5&MCA_MPO>0.1&hasExp==0&firstyear>=2011&!is.na(MCA_MPO))

eliminated=df%>%filter(MCA_MPO>5|MCA_MPO<=0.1&hasExp==0)%>%nrow()

#Summarised datasets
df.bids.means=df.bids%>%dplyr::group_by(exp)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
n=length(exp))%>%mutate(std.error=std/sqrt(n))

df.bids.means=df.bids%>%dplyr::group_by(exp)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
                                                         n=length(exp))%>%mutate(std.error=std/sqrt(n))

#df.bids.means.close=df.bids%>%dplyr::group_by(exp_close)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
 #                                                        n=length(exp))%>%mutate(std.error=std/sqrt(n))

df.bids.means.closeranks=df.bids%>%filter(exp==exp_closerank)%>%dplyr::group_by(exp_closerank)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
                                                                     n=length(exp))%>%mutate(std.error=std/sqrt(n))

#Analysis
lm.34=lm(MCA_MPO~as.factor(year)+RegionUnidad+(exp>0)+(indFirstYear), data=df.bids)
lm.35=lm(MCA_MPO~(exp)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)
lm.36=ivreg(MCA_MPO~as.factor(year)+RegionUnidad+(exp>0)+indFirstYear|
              (exp_close>0)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)
lm.37=ivreg(MCA_MPO~as.factor(year)+RegionUnidad+(exp)+indFirstYear|
              (exp_close)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)

robust.lm34<- vcovHC(lm.34, type = "HC1")%>%diag()%>%sqrt()
robust.lm35<- vcovHC(lm.35, type = "HC1")%>%diag()%>%sqrt()
robust.lm36<- vcovHC(lm.36, type = "HC1")%>%diag()%>%sqrt()
robust.lm37<- vcovHC(lm.37, type = "HC1")%>%diag()%>%sqrt()

summary(lm.36)
#####
##Second Investigation: Quality
#####

df.quality=df %>% mutate(WinContr = as.numeric(winner == 'Seleccionada')) %>%
  group_by(RutProveedor) %>% arrange(FechaInicio) %>% mutate(
    firstyear = min(year),
    exp = max(cumsum(WinContr) - 1, 0),
    isCloseWin = WinContr * isClose)%>%
  mutate(
    exp_close =
      max(cumsum(isCloseWin) - 1, 0)
  ) %>%
  mutate(exp_close = ifelse(is.na(exp_close), yes = 0, no = exp_close)) %>%
  mutate(indExp = as.numeric(exp > 0),
         indExpClose = as.numeric(exp_close > 0)) %>%
  mutate(
    annualexp = exp / (year - firstyear + 1),
    annualexp_close = exp_close / (year - firstyear + 1),
    life = (year - firstyear + 1)
  )%>%
  mutate(indFirstYear=(year==firstyear))

df.quality = df.quality %>% mutate(indAccepted = as.numeric(estadoOferta ==
                                                              'Aceptada')) %>% arrange(FechaInicio) %>% group_by(RutProveedor) %>%
  mutate(tot.acc = cumsum(indAccepted), tot.sub=cumsum(as.numeric(indAccepted>=0)))%>%mutate(ac.rate=tot.acc/tot.sub)%>% filter(year >= 2011) 

accepted.rates=df.quality%>%group_by(RutProveedor)%>%summarise(accepted=sum(indAccepted), submitted=length(indAccepted),AcceptanceRate=accepted/submitted)
accepted.rates.3=df.quality%>%group_by(RutProveedor)%>%summarise(accepted=sum(indAccepted), submitted=length(indAccepted),AcceptanceRate=accepted/submitted)%>%filter(submitted>=3)

plot.accepted.rates=ggplot(accepted.rates,aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')+ggtitle('Histogram of proposal acceptance rate, all firms')
plot.accepted.rates.3=ggplot(accepted.rates.3,aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')+ggtitle('Histogram of proposal acceptance rate, firms with 3+ proposals')
plot.accepted.rates.10=ggplot(accepted.rates.3%>%filter(submitted>=10),aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')

plot.acc.rates.all=plot_grid(plot.accepted.rates,plot.accepted.rates.3)
png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_acceptance_rates.png",width = 12, height = 4.0,units = "in",res=1000)
plot.acc.rates.all
dev.off()

## Results

df.quality.plot = df.quality %>% mutate(annualexp = round(annualexp)) %>% group_by(annualexp) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))

plot.quality.exp=ggplot(df.quality.plot,aes(x=annualexp,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(0,10)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.1))+
  ylim(0.6,1)+theme_bw()+xlab('Annualized Experience')+ylab('Mean Proposal Acceptance  Indicator')+
  theme(axis.text.x = element_text(size=9,color = 'black'))+ggtitle('All bids and firms')
plot.quality.exp

df.quality.plot.restricted=df.quality%>%filter(exp%in%c(0,1)&tot.sub==1&firstyear>=2011)%>% group_by(exp) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))

plot.quality.exp.restricted=ggplot(df.quality.plot.restricted,aes(x=exp,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(-0.05,1.05)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.05))+
  ylim(0.6,1)+theme_bw()+xlab('Annualized Experience')+ylab('Mean Proposal Acceptance  Indicator')+
  theme(axis.text.x = element_text(size=9,color = 'black'))+ggtitle('Firms with one previous proposal')
plot.quality.exp.restricted

row.1=ggdraw() + 
  draw_label(
    "Mean of Proposal Acceptance Indicator by Past Experience",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
row.2=plot_grid(plot.quality.exp,plot.quality.exp.restricted,nrow = 1)
plot.quality.results=plot_grid(row.1,row.2,rel_heights = c(0.1, 1),nrow = 2)

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_acceptance_results.png",width = 9, height = 4.5,units = "in",res=1000)
plot.quality.results
dev.off()

###### Second quality idea. 
start=0
split1=2
split2=2
#merged.wins = createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )#%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createAnnualizedWins(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)%>%filter(!is.na(idperiodpre))

lm.54<-lm(probAc~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm4<- vcovHC(lm.4, type = "HC1")%>%diag()%>%sqrt()
summary(lm.54)

lm.55<-lm(probAc~(winspre)+idperiodpost,data = merged.wins)
robust.lm55<- vcovHC(lm.55, type = "HC1")%>%diag()%>%sqrt()
summary(lm.55)

lm.56<-ivreg(probAc~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost,data=merged.wins)
robust.lm56<- vcovHC(lm.56, type = "HC1")%>%diag()%>%sqrt()
summary(lm.56)

lm.57<-ivreg(probAc~(winspre)+idperiodpost|(winspre_close)+idperiodpost,data = merged.wins)
robust.lm57<- vcovHC(lm.57, type = "HC1")%>%diag()%>%sqrt()
summary(lm.57)

lm.58<-lm(probAc~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm4<- vcovHC(lm.4, type = "HC1")%>%diag()%>%sqrt()
summary(lm.58)

##Annualized
lm.59<-lm(probAc~(annualwinspre)+idperiodpost,data = merged.wins)
robust.lm55<- vcovHC(lm.55, type = "HC1")%>%diag()%>%sqrt()
summary(lm.59)

lm.60<-ivreg(probAc~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost,data=merged.wins)
robust.lm56<- vcovHC(lm.56, type = "HC1")%>%diag()%>%sqrt()
summary(lm.60)

lm.61<-ivreg(probAc~(annualwinspre)+idperiodpost|(annualwinspre_close)+idperiodpost,data = merged.wins)
robust.lm57<- vcovHC(lm.57, type = "HC1")%>%diag()%>%sqrt()
summary(lm.61)

# Ranks
start=0
split1=2
split2=2
merged.wins=createMultiPeriodDataset(df%>%filter(year>=2011),start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005, ranks=T)
lm.62<-ivreg(probAc~(winspre>0)+idperiodpost|(winspre_closerank>0)+idperiodpost,data = merged.wins)
robust.lm62<- vcovHC(lm.62, type = "HC1")%>%diag()%>%sqrt()
summary(lm.62)

lm.63<-ivreg(probAc~(winspre)+idperiodpost|(winspre_closerank)+idperiodpost,data = merged.wins)
robust.lm58<- vcovHC(lm.63, type = "HC1")%>%diag()%>%sqrt()
summary(lm.63)




############# OLD

## Linear Model Results
### OLS
df.quality.restricted=df.quality%>%filter(exp%in%c(0,1)&tot.sub==1&firstyear>=2011)
lm.50=lm(df.quality.restricted,formula = indAccepted~as.factor(year)+RegionUnidad+NombreOrganismo+(exp>0))
robust.lm50<- vcovHC(lm.50, type = "HC1")%>%diag()%>%sqrt()
lm.50%>%tidy()%>%tail()

lm.51=lm(df.quality,formula = indAccepted~as.factor(year)+RegionUnidad+NombreOrganismo+(annualexp))
robust.lm51<- vcovHC(lm.51, type = "HC1")%>%diag()%>%sqrt()
summary(lm.51)%>%tidy()%>%tail()

### IV
lm.52=ivreg(data=df.quality.restricted,formula = indAccepted~as.factor(year)+RegionUnidad+(exp>0)|
              as.factor(year)+RegionUnidad+(exp_close>0))
robust.lm52<- vcovHC(lm.52, type = "HC1")%>%diag()%>%sqrt()
summary(lm.52)

lm.53=ivreg(data=df.quality,formula = indAccepted~as.factor(year)+RegionUnidad+(annualexp)|
              as.factor(year)+RegionUnidad+(annualexp_close))
robust.lm53<- vcovHC(lm.53, type = "HC1")%>%diag()%>%sqrt()
summary(lm.53)





start=0
split1=2
split2=2
#merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
  mutate(logWinpre=log(winspre+1))


##IV Specs
##7
lm.77<-ivreg(probWinpost~(winspre>0)|(winspre_close>0),data=merged.wins)
robust.lm77<- vcovHC(lm.77, type = "HC1")%>%diag()%>%sqrt()
summary(lm.77)

##8
lm.8<-ivreg(probWinpost~winspre+(winspre):avQualityWeight+avQualityWeight+idperiodpost|
              (winspre_close)+(winspre_close):avQualityWeight+avQualityWeight+idperiodpost,data=merged.wins)
robust.lm8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()

summary(lm.8)
##8
lm.8<-ivreg(probWinpost~(winspre>0)+(winspre>0)*(avPriceWeight>60)+(winspre>0):(avQualityWeight>40)+(avQualityWeight>40)+idperiodpost|
              (winspre_close>0)+(winspre_close>0)*(avPriceWeight>60)+(winspre_close>0):(avQualityWeight>40)+(avQualityWeight>40)+idperiodpost,data=merged.wins)
#robust.lm8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()
summary(lm.8)

summary(merged.wins$avPriceWeight)
summary(merged.wins$avQualityWeight)

## Analyze possible mechanisms to explain the gained advantages
df.indiv=df%>%mutate(WinContr=as.numeric(winner=='Seleccionada'))
df.indiv.exp=df.indiv%>%group_by(RutProveedor)%>%arrange(FechaInicio)%>%mutate(firstyear=min(year),exp=max(cumsum(WinContr)-1,0),isCloseWin=WinContr*isClose,
                                                                               exp_close=max(cumsum(isCloseWin)-1,0))%>%
  mutate(exp_close=ifelse(is.na(exp_close),yes=0, no=exp_close))%>%
  mutate(indExp=as.numeric(exp>0),indExpClose=as.numeric(exp_close>0))%>%
  mutate(annualexp=exp/(year-firstyear+1),annualexp_close=exp_close/(year-firstyear+1),life=(year-firstyear+1))

summary(sample.df$annualexp_close)
#Exploratory analysis

small=c('Licitación Pública Entre 100 y 1000 UTM (LE)')
big=c("Licitación Pública Mayor a 5000 (LR)")
typeLicitacion=data.frame(tipoAdquisicion=c(small,big),typeSize=c('Small','Big'))

regionesExtremas=c("Región de Arica y Parinacota",'Región de Magallanes y de la Antártica','Región Aysén del General Carlos Ibáñez del Campo')

df.indiv.exp.comp=df.indiv.exp%>%mutate(isExtreme=RegionUnidad%in%regionesExtremas)%>%filter(hasExp==0)%>%
  left_join(typeLicitacion)%>%mutate(percentilePrice=ntile(percPrice,5))%>%
  mutate(percentileQuality=ntile(percQuality,5),q=(percQuality))


table.typeSize=df.indiv.exp.comp%>%group_by(typeSize,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)
table.extreme=df.indiv.exp.comp%>%group_by(isExtreme,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.price=df.indiv.exp.comp%>%group_by(percentilePrice,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.q=df.indiv.exp.comp%>%group_by(percentileQuality,indExp)%>%summarise(,q=mean(q),n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
df.indiv.exp.comp%>%group_by(percentileQuality)%>%summarise(of=mean(NumeroOferentes))                     

###Create indicators for firms with and without experience at the begginings of their careers.
##Segment by different types of contract variables
#Show results with standard error.
sample.df.offers=df.indiv.exp%>%filter(hasExp==FALSE&MCA_MPO<=5)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%
sample.df=df.indiv.exp%>%filter(hasExp==FALSE&year>=2011)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%

ivreg(WinContr~(exp>0)+(exp>0):percPrice+(exp>0):percQuality+percQuality+percPrice+as.factor(year)|
        (exp_close>0)+(exp_close>0):percPrice+(exp_close>0):percQuality+percQuality+percPrice+as.factor(year), data=sample.df)%>%summary()

ivreg(WinContr~annualexp+annualexp:percPrice+annualexp:percQuality+percQuality+percPrice+as.factor(year)|
        exp_close+exp_close:percPrice+exp_close:percQuality+percQuality+percPrice+as.factor(year), data=sample.df)%>%summary()

ivreg(WinContr~annualexp+annualexp:percQuality+percQuality+as.factor(year)|
        exp_close+exp_close:percQuality+percQuality+as.factor(year), data=sample.df)%>%summary()

lm(formula=annualexp~(exp_close),data=sample.df)%>%summary()

####
#Price and Quality
####

lm.29=lm(WinContr~(indExp>0)*percPrice+(indExp>0):percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.29)

lm.30=lm(WinContr~(exp)*percPrice+exp:percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.30)
table(sample.df$exp_close)

lm.31=ivreg(WinContr~(exp>0)*percPrice+(exp>0):percQuality+as.factor(year)+percQuality+NumeroOferentes|
              (exp_close>0)*percPrice+(exp_close>0):percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.31)

lm.32=ivreg(as.numeric(WinContr)~(exp)*percPrice+(exp):percQuality+as.factor(year)+percQuality+NumeroOferentes+RegionUnidad|
              exp_close*percPrice+exp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes+RegionUnidad, data=sample.df)
summary(lm.32)


sample.df%>%summary




lm.32.2=ivreg(as.numeric(WinContr)~(annualexp)*percPrice+(annualexp):percQuality+as.factor(year)+percQuality+NumeroOferentes|
                annualexp_close*percPrice+annualexp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.32.2)

a=ivreg(WinContr~(exp>0)+as.factor(year)|(exp_close>0)+as.factor(year),data=sample.df)
summary(a)

####
#Price of the Bids
####

sample.df.2=sample.df%>%group_by(RutProveedor)%>%mutate(n=length(Codigo))%>%filter(n>=3)

lm.33=lm(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.33)

lm.34=ivreg(MCA_MPO~as.factor(year)+RegionUnidad+(exp>0)|
              (exp_close>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.34)

lm.35=ivreg(MCA_MPO~(exp)+as.factor(year)+RegionUnidad|
              (exp_close)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.35)

sample.df