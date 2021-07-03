## First Investigation: Bids
df.bids = df %>% mutate(WinContr = as.numeric(winner == 'Seleccionada')) %>%
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

df.bids=df.bids%>%filter(MCA_MPO<=5&MCA_MPO>0.1&hasExp==0&firstyear>=2011)

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



summary(lm.35)
models=list(ols.bin,ols.linear,iv.bin,iv.lin)
results=models%>%map_dfr(function(x) coeftest(x,vcov = vcovHC(x, type = "HC1"))%>%tidy()%>%mutate())%>%filter(term%in%c('exp > 0TRUE','exp','indFirstYearTRUE'))










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
